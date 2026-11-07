//===--- CodeSinking.swift ------------------------------------------------==//
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

/// Sinks side-effect free instructions closer to their users.
///
/// Many SIL instructions that don't have side effects at the SIL level are lowered to a sequence
/// of LLVM instructions which LLVM cannot sink, e.g. because they have side effects at the LLVM
/// level. This pass moves such instructions to the begin of the block which is the nearest common
/// dominator of all users:
///
/// ```
///   bb0:
///     %1 = upcast %0 to $X
///     %2 = class_method %1, #X.foo
///     cond_br %c, bb1, bb2
///   bb1:
///     %3 = apply %2(%1)
/// ```
/// ->
/// ```
///   bb0:
///     cond_br %c, bb1, bb2
///   bb1:
///     %1 = upcast %0 to $X
///     %2 = class_method %1, #X.foo
///     %3 = apply %2(%1)
/// ```
///
/// Instructions are never sunk into a loop.
///
/// The pass also works on OSSA. Owned operands prevent sinking: either the instruction consumes
/// the operand or its results are not bound to the operand's liverange. Instructions with
/// guaranteed operands are sunk if their results are bound to the borrow scope (e.g. `upcast` with
/// a guaranteed result or `ref_element_addr`) or if the borrow scope contains the new location.
///
let codeSinking = FunctionPass(name: "code-sinking") {
  (function: Function, context: FunctionPassContext) in

  let domTree = context.dominatorTree
  let loopTree = context.loopTree

  // Visit the blocks in post-order to sink the users of an instruction before the instruction
  // itself. This allows to sink whole instruction chains in a single scan over the function.
  for block in function.postOrderBlocks(context) {
    var inst = block.instructions.last
    while let i = inst {
      // Get the previous instruction before `i` is moved to another block.
      inst = i.previous
      i.trySink(domTree, loopTree, context)
    }
  }
}

private extension Instruction {
  func trySink(_ domTree: DominatorTree, _ loopTree: LoopTree, _ context: FunctionPassContext) {
    guard canBeSunk,
          var destBlock = blockDominatingAllUsers(domTree),
          // Don't sink into unreachable blocks.
          domTree.getImmediateDominator(of: destBlock) != nil
    else {
      return
    }

    // Don't sink instructions into loops: walk up the dominator tree until we are outside of all
    // loops which don't contain the original block of the instruction.
    while let loop = loopTree.innermostLoop(of: destBlock), !loop.contains(block: parentBlock) {
      guard let idom = domTree.getImmediateDominator(of: destBlock),
            idom != parentBlock
      else {
        return
      }
      destBlock = idom
    }

    let insertionPoint = destBlock.instructions.first!
    guard operands.allSatisfy({ canBeSunk(withOperand: $0, before: insertionPoint, context) }) else {
      return
    }
    move(before: insertionPoint, context)
  }

  var canBeSunk: Bool {
    // Sinking instructions which read or write memory or have other side effects could change
    // the semantics of the program.
    if hasUnspecifiedSideEffects || mayReadOrWriteMemory {
      return false
    }
    switch self {
    case is MarkUninitializedInst, is MarkFunctionEscapeInst, is MarkDependenceInstruction,
         // Don't move the begin of scopes, e.g. a `begin_borrow` of a value with none-ownership.
         is BeginBorrowInstruction, is ScopedInstruction:
      return false
    default:
      break
    }
    // Sinking stack allocations would break the proper nesting of stack allocations.
    if isAllocatingStack || isDeallocatingStack {
      return false
    }
    return true
  }

  /// Returns true if moving this instruction to `insertionPoint` doesn't violate any lifetime
  /// constraints of `operand`.
  func canBeSunk(withOperand operand: Operand, before insertionPoint: Instruction,
                 _ context: FunctionPassContext) -> Bool {
    if operand.isTypeDependent {
      return true
    }
    switch operand.value.ownership {
    case .none:
      return canBeSunk(withTrivialOperand: operand.value, before: insertionPoint, context)
    case .guaranteed:
      switch operand.ownership {
      case .interiorPointer:
        // The verifier checks that all uses of the address are within the borrow scope.
        return true
      case .guaranteedForwarding where results.allSatisfy({ $0.ownership == .guaranteed }):
        // Uses of forwarded guaranteed values are within the borrow scope of the operand.
        // This is not the case for trivial results, e.g. a `struct_extract` of a trivial field.
        return true
      default:
        // E.g. a `class_method` or a pointer escape, like `ref_to_raw_pointer`. The result is not
        // bound to the borrow scope and we have to check if the borrow scope is still alive at the
        // new location.
        return operand.value.borrowScopesContain(insertionPoint, context)
      }
    case .owned, .unowned:
      // Owned operands are either consumed by this instruction, which ends their lifetime, or
      // the users of this instruction's results are not required to be in their liverange.
      return false
    }
  }

  /// Handles operands with none-ownership, which can still be bound to a borrow scope, e.g. addresses
  /// which are projected from a borrowed class reference.
  func canBeSunk(withTrivialOperand value: Value, before insertionPoint: Instruction,
                 _ context: FunctionPassContext) -> Bool {
    if !parentFunction.hasOwnership {
      return true
    }
    if value.type.isAddress {
      if results.allSatisfy({ $0.type.isAddress }) {
        // An address projection. The verifier checks that all uses of the projected address are
        // within the borrow scope of the base.
        return true
      }
      // E.g. an `address_to_pointer`: the result is not bound to the borrow scope.
      let accessBase = value.accessBase
      switch accessBase {
      case .box, .class, .tail:
        let reference = accessBase.reference!
        switch reference.ownership {
        case .none:
          return true
        case .guaranteed:
          return reference.borrowScopesContain(insertionPoint, context)
        case .owned, .unowned:
          return false
        }
      case .stack, .global, .argument, .pointer:
        return true
      case .yield, .storeBorrow, .index, .unidentified:
        return false
      }
    }
    // E.g. a non-escaping closure which is derived from a borrowed closure.
    return !value.type.isNoEscapeFunction
  }

  /// Returns the nearest common dominator block of all users, or nil if it's the parent block of
  /// this instruction or if there are no users.
  func blockDominatingAllUsers(_ domTree: DominatorTree) -> BasicBlock? {
    var destBlock: BasicBlock? = nil
    for result in results {
      for use in result.uses {
        let userBlock = use.instruction.parentBlock
        if userBlock == parentBlock {
          return nil
        }
        if let d = destBlock {
          guard let commonDominator = d.nearestCommonDominator(with: userBlock, domTree),
                commonDominator != parentBlock
          else {
            return nil
          }
          destBlock = commonDominator
        } else {
          destBlock = userBlock
        }
      }
    }
    return destBlock
  }
}

private extension Value {
  func borrowScopesContain(_ inst: Instruction, _ context: FunctionPassContext) -> Bool {
    for introducer in getBorrowIntroducers(context) where introducer.hasLocalScope {
      var scope = InstructionRange(for: introducer.value, context)
      defer { scope.deinitialize() }
      scope.insert(contentsOf: introducer.scopeEndingOperands.users)

      // If `inst` is a scope ending instruction, it's still fine to insert before it.
      if !scope.inclusiveRangeContains(inst) {
        return false
      }
    }
    return true
  }
}

private extension BasicBlock {
  func nearestCommonDominator(with other: BasicBlock, _ domTree: DominatorTree) -> BasicBlock? {
    var block = self
    while !block.dominates(other, domTree) {
      guard let idom = domTree.getImmediateDominator(of: block) else {
        return nil
      }
      block = idom
    }
    return block
  }
}
