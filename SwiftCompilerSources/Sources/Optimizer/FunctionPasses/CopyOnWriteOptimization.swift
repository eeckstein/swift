//===--- CopyOnWriteOptimization.swift -------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

let copyOnWriteOptimization = FunctionPass(name: "copy-on-write-optimization") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    if let beginCOW = inst as? BeginCOWMutationInst {
      optimizeBeginCOW(beginCOW, context)
    }
  }
}

private func optimizeBeginCOW(_ beginCOW: BeginCOWMutationInst, _ context: FunctionPassContext) {
  var findEndCOWWalker = FindEndCOWMutations(context)
  defer { findEndCOWWalker.deinitialize() }

  if findEndCOWWalker.walkUp(value: beginCOW.instance, path: SmallProjectionPath()) == .abortWalk {
    return
  }

  var liferange = InstructionSet(context)
  defer { liferange.deinitialize() }

  liferange.insertAll(from: findEndCOWWalker.endCOWMutations, to: beginCOW, context)

  var findCopyWalker = FindCopies(within: liferange, context)
  defer { findCopyWalker.deinitialize() }

  for endCowInst in findEndCOWWalker.endCOWMutations {
    if findCopyWalker.isCopied(value: endCowInst, mustBeDestroyedWithinLiferange: false) {
      return
    }
  }

  while let copy = findCopyWalker.copies.pop() {
    if findCopyWalker.isCopied(value: copy, mustBeDestroyedWithinLiferange: true) {
      return
    }
  }

  let builder = Builder(before: beginCOW, context)
  let one = builder.createIntegerLiteral(1, type: beginCOW.uniquenessResult.type)
  beginCOW.uniquenessResult.uses.replaceAll(with: one, context)
  for endCOW in findEndCOWWalker.endCOWMutations {
    endCOW.set(keepUnique: true, context)
  }
}

private struct FindCopies : ValueDefUseWalker {
  var copies: ValueWorklist
  let liferange: InstructionSet

  private let context: FunctionPassContext
  private let aliasAnalysis: AliasAnalysis
  private var mustBeDestroyedWithinLiferange = false
  var walkDownCache = WalkerCache<SmallProjectionPath>()

  init(within liferange: InstructionSet, _ context: FunctionPassContext) {
    self.copies = ValueWorklist(context)
    self.liferange = liferange
    self.context = context
    self.aliasAnalysis = context.aliasAnalysis
  }

  mutating func deinitialize() {
    copies.deinitialize()
  }

  mutating func isCopied(value: Value, mustBeDestroyedWithinLiferange: Bool) -> Bool {
    self.mustBeDestroyedWithinLiferange = mustBeDestroyedWithinLiferange
    return walkDownUses(ofValue: value, path: SmallProjectionPath()) == .abortWalk
  }

  mutating func walkDown(value operand: Operand, path: SmallProjectionPath) -> WalkResult {
    if !liferange.contains(operand.instruction) {
      return .continueWalk
    }

    // TODO: handle applies
    switch operand.instruction {
    case is BeginCOWMutationInst,
         is RefElementAddrInst,
         is RefTailAddrInst,
         is DebugValueInst:
      return .continueWalk
    case let copy as CopyValueInst:
      copies.pushIfNotVisited(copy)
      return .continueWalk
    case let store as StoreInst:
      return findLoads(of: store, path: path)
    case is DestroyValueInst:
      return mustBeDestroyedWithinLiferange == liferange.contains(operand.instruction) ? .continueWalk : .abortWalk
    default:
      return walkDownDefault(value: operand, path: path)
    }
  }

  mutating func leafUse(value: Operand, path: SmallProjectionPath) -> WalkResult {
    return .abortWalk
  }

  mutating func findLoads(of store: StoreInst, path: SmallProjectionPath) -> WalkResult {

    let storeAccessPath = store.destination.getAccessPath(fromInitialPath: path)

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(store)
    worklistLoop: while let startInst = worklist.pop() {
      for inst in InstructionList(first: startInst) {
        if !liferange.contains(inst) {
          continue worklistLoop
        }
        switch inst {
        case is BeginAccessInst:
          break
        case let load as LoadInst:
          if let loadedValuePath = storeAccessPath.getProjection(to: load.address.accessPath) {
            return walkDownUses(ofValue: load, path: loadedValuePath)
          }
          fallthrough
        default:
          if inst.mayRead(fromAddress: store.destination, aliasAnalysis) {
            return .abortWalk
          }
        }
      }
      for succ in startInst.parentBlock.successors {
        let firstSuccInst = succ.instructions.first!
        if liferange.contains(firstSuccInst) {
          worklist.pushIfNotVisited(firstSuccInst)
        }
      }
    }
    return .continueWalk
  }
}

private struct FindEndCOWMutations : ValueUseDefWalker {
  var endCOWMutations: Stack<EndCOWMutationInst>

  let context: FunctionPassContext
  let aliasAnalysis: AliasAnalysis
  var walkUpCache = WalkerCache<SmallProjectionPath>()

  init(_ context: FunctionPassContext) {
    self.endCOWMutations = Stack(context)
    self.context = context
    self.aliasAnalysis = context.aliasAnalysis
  }

  mutating func deinitialize() {
    endCOWMutations.deinitialize()
  }

  mutating func walkUp(value: Value, path: SmallProjectionPath) -> WalkResult {
    switch value {
    case let endCOW as EndCOWMutationInst:
      endCOWMutations.append(endCOW)
      return .continueWalk
    case let load as LoadInst:
      return findStores(of: load, path: path)
    default:
      return walkUpDefault(value: value, path: path)
    }
  }

  mutating func rootDef(value: Value, path: SmallProjectionPath) -> WalkResult {
    return .abortWalk
  }

  mutating func findStores(of load: LoadInst, path: SmallProjectionPath) -> WalkResult {

    let loadAccessPath = load.address.getAccessPath(fromInitialPath: path)

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(load)
    while let startInst = worklist.pop() {
      for inst in ReverseInstructionList(first: startInst) {
        switch inst {
        case is BeginAccessInst:
          break
        case let store as StoreInst:
          if let storedValuePath = store.destination.accessPath.getProjection(to: loadAccessPath) {
            return walkUp(value: store.source, path: storedValuePath)
          }
          fallthrough
        default:
          if inst != load && inst.mayWrite(toAddress: load.address, aliasAnalysis) {
            return .abortWalk
          }
        }
      }
      let block = startInst.parentBlock
      if block.predecessors.isEmpty {
        return .abortWalk
      }
      worklist.pushIfNotVisited(contentsOf: block.predecessors.lazy.map { $0.terminator })
    }
    return .continueWalk
  }
}

private extension InstructionSet {
  mutating func insertAll(from beginInstructions: Stack<EndCOWMutationInst>, to endInstruction: Instruction,
                          _ context: FunctionPassContext) {
    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(endInstruction)

    var beginSet = InstructionSet(context)
    defer { beginSet.deinitialize() }
    beginSet.insert(contentsOf: beginInstructions)

    worklistLoop: while let startInst = worklist.pop() {
      for inst in ReverseInstructionList(first: startInst) {
        if beginSet.contains(inst) {
          continue worklistLoop
        }
        insert(inst)
      }
      worklist.pushIfNotVisited(contentsOf: startInst.parentBlock.predecessors.lazy.map { $0.terminator })
    }
  }
}
