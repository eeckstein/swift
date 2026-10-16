//===--- TrivialEnumCaseDestroyElimination.swift --------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Removes the destroy of a value in memory if the memory is known to contain an enum case without
/// a non-trivial payload.
///
/// The memory content is known if the destroy is in a case block of a `switch_enum` on a value
/// which is loaded from the same memory location:
/// ```
///   %1 = load_borrow %a
///   switch_enum %1, case #E.trivialCase: bb1, ...
/// bb1(%2 : $Int):
///   end_borrow %1
///   store %new to [assign] %a
/// ```
/// ->
/// ```
/// bb1(%2 : $Int):
///   end_borrow %1
///   store %new to [init] %a
/// ```
/// or if a value with "none" ownership, e.g. a trivial enum case, was stored to the memory before:
/// ```
///   %e = enum $E, #E.trivialCase, %payload
///   store %e to [assign] %a
///   ...
///   destroy_addr %a     // removed
/// ```
///
/// Handled destroys are `store [assign]`, `destroy_addr` and `destroy_value` of a `load [take]`.
///
/// Note that such memory is not deinitialized anymore before it is re-initialized or deallocated.
/// This is fine, because memory which contains a trivial enum case is treated like uninitialized
/// memory, which is also what the MemoryLifetimeVerifier assumes.
///
let trivialEnumCaseDestroyElimination = FunctionPass(name: "trivial-enum-case-destroy-elimination") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  // Collect the candidates first, because the transformation erases instructions.
  let candidates = function.instructions.filter {
    $0 is StoreInst || $0 is DestroyAddrInst || $0 is DestroyValueInst
  }
  for inst in candidates {
    switch inst {
    case let store as StoreInst:
      if store.storeOwnership == .assign,
         store.destination.memoryContainsValueWithoutDestroy(at: store, context)
      {
        store.set(ownership: .initialize, context)
      }

    case let destroyAddr as DestroyAddrInst:
      if destroyAddr.destroyedAddress.memoryContainsValueWithoutDestroy(at: destroyAddr, context) {
        context.erase(instruction: destroyAddr)
      }

    case let destroy as DestroyValueInst:
      if !destroy.isDeadEnd,
         let load = destroy.destroyedValue as? LoadInst,
         load.loadOwnership == .take,
         load.uses.ignoreDebugUses.singleUse?.instruction == destroy,
         load.address.memoryContainsValueWithoutDestroy(at: load, context)
      {
        context.erase(instructionIncludingAllUsers: load)
      }

    default:
      break
    }
  }
}

private extension Value {
  /// Returns true if the memory at this address is known to contain a value which doesn't need to
  /// be destroyed at `inst`.
  func memoryContainsValueWithoutDestroy(at inst: Instruction, _ context: FunctionPassContext) -> Bool {
    let aliasAnalysis = context.aliasAnalysis

    // Limits the compile time for long instruction sequences.
    var budget = 64

    // Walk up the control flow. On all paths, we must find a proof that the memory contains a value
    // which doesn't need to be destroyed, without passing any instruction which may write to the
    // memory.
    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }

    guard pushPredecessors(of: inst, to: &worklist, aliasAnalysis) else {
      return false
    }
    while let i = worklist.pop() {
      budget -= 1
      if budget == 0 {
        return false
      }
      if let store = i as? StoreInst, isSameMemoryLocation(as: store.destination) {
        if store.source.ownership == .none {
          // This path is proven.
          continue
        }
        return false
      }
      if i.mayWriteIgnoringScopeEnds(to: self, aliasAnalysis) {
        return false
      }
      guard pushPredecessors(of: i, to: &worklist, aliasAnalysis) else {
        return false
      }
    }
    return true
  }

  /// Pushes the predecessor instructions of `inst` to the `worklist`, except predecessor blocks which
  /// switch to `inst`'s block for a trivial enum case of the memory content.
  /// Returns false if `inst` is the first instruction in the function.
  private func pushPredecessors(of inst: Instruction, to worklist: inout InstructionWorklist,
                                _ aliasAnalysis: AliasAnalysis) -> Bool {
    if let prev = inst.previous {
      worklist.pushIfNotVisited(prev)
      return true
    }
    let block = inst.parentBlock
    if block.predecessors.isEmpty {
      return false
    }
    for pred in block.predecessors where !isTrivialCaseEdge(from: pred, to: block, aliasAnalysis) {
      worklist.pushIfNotVisited(pred.terminator)
    }
    return true
  }

  /// Returns true if `pred` ends with a `switch_enum` on a value which is loaded from this address
  /// and `block` is the successor of an enum case without a non-trivial payload.
  private func isTrivialCaseEdge(from pred: BasicBlock, to block: BasicBlock,
                                 _ aliasAnalysis: AliasAnalysis) -> Bool {
    let function = block.parentFunction
    guard let switchEnum = pred.terminator as? SwitchEnumInst,
          switchEnum.getSuccessorForDefault() != block,
          switchEnum.cases.filter({ $0.1 == block }).count == 1,
          let caseIndex = switchEnum.getUniqueCase(forSuccessor: block),
          let load = switchEnum.enumOp.definingInstruction,
          load is LoadBorrowInst || load is LoadInst,
          load.parentBlock == pred,
          isSameMemoryLocation(as: load.operands[0].value),
          let enumCase = switchEnum.enumOp.type.getEnumCases(in: function)?[caseIndex],
          enumCase.payload?.isTrivial(in: function) ?? true
    else {
      return false
    }
    // The memory must not be written between the load and the `switch_enum`.
    return !InstructionList(first: load.next).contains {
      $0 != switchEnum && $0.mayWriteIgnoringScopeEnds(to: self, aliasAnalysis)
    }
  }

  func isSameMemoryLocation(as other: Value) -> Bool {
    let lhs = self.lookThroughBeginAccesses
    let rhs = other.lookThroughBeginAccesses
    if lhs == rhs {
      return true
    }
    if let lhsField = lhs as? RefElementAddrInst, let rhsField = rhs as? RefElementAddrInst {
      return lhsField.fieldIndex == rhsField.fieldIndex && lhsField.instance == rhsField.instance
    }
    return false
  }

  var lookThroughBeginAccesses: Value {
    var value: Value = self
    while let beginAccess = value as? BeginAccessInst {
      value = beginAccess.address
    }
    return value
  }
}

private extension Instruction {
  func mayWriteIgnoringScopeEnds(to address: Value, _ aliasAnalysis: AliasAnalysis) -> Bool {
    switch self {
    case is EndBorrowInst, is BeginAccessInst, is EndAccessInst:
      // Those instructions don't write to memory, even if alias analysis says so.
      return false
    default:
      return mayWrite(toAddress: address, aliasAnalysis)
    }
  }
}
