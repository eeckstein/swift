//===--- LifetimeCompletion.swift -----------------------------------------===//
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
import OptimizerBridging

func completeLifetimes(in function: Function, includeTrivialVars: Bool = false, _ context: FunctionPassContext) {
  context.setNeedCompleteLifetimes(to: false)

  guard function.hasOwnership else {
    return
  }

  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }

  context.updateAnalysis()
  let dominatorTree = context.dominatorTree

  var blocksToComplete = BasicBlockSet(context)
  defer { blocksToComplete.deinitialize() }

  worklist.pushIfNotVisited(function.entryBlock)
  while let block = worklist.pop() {
    if block.successors.isEmpty {
      var toInsert = block
      while blocksToComplete.insert(toInsert) {
        guard let parent = dominatorTree.getParent(of: toInsert) else {
          break
        }
        toInsert = parent
      }
    } else {
      worklist.pushIfNotVisited(contentsOf: block.successors)
    }
  }

  let blocks = dominatorTree.dominanceOrder(startingAt: function.entryBlock) { blocksToComplete.contains($0) }

  // Process blocks in reverse dominance order.
  for block in blocks.reversed() {
    for inst in block.instructions.reversed() {
      for result in inst.results {
        completeLifetime(of: result, includeTrivialVars: includeTrivialVars, context)
      }
    }
    for arg in block.arguments {
      completeLifetime(of: arg, includeTrivialVars: includeTrivialVars, context)
    }
  }
}

func completeLifetime(of value: Value, includeTrivialVars: Bool = false, _ context: FunctionPassContext) {

  var endBlocks = BasicBlockSet(context)
  defer { endBlocks.deinitialize() }

  let valueToComplete: Value
  switch value.ownership {
  case .owned:
    valueToComplete = value
  case .guaranteed:
    guard let beginBorrow = BeginBorrowValue(value) else {
      return
    }
    switch beginBorrow {
    case .beginBorrow, .loadBorrow, .uncheckOwnershipConversion:
      valueToComplete = value
    case .beginApply(let v):
      let ba = v.definingInstruction as! BeginApplyInst
      guard v == ba.token else {
        return
      }
      valueToComplete = v
    case .reborrow(let phi):
      valueToComplete = phi.borrowedFrom!
    case .functionArgument:
      return
    }
  case .none:
    switch value {
    case let sb as StoreBorrowInst:
      endBlocks.insert(contentsOf: sb.uses.users(ofType: EndBorrowInst.self).lazy.map { $0.parentBlock })
      valueToComplete = sb
    case let mv as MoveValueInst where includeTrivialVars && mv.isFromVarDecl:
      endBlocks.insert(contentsOf: mv.uses.users(ofType: ExtendLifetimeInst.self).lazy.map { $0.parentBlock })
      valueToComplete = mv
    case let ba as BeginAccessInst:
      endBlocks.insert(contentsOf: ba.uses.users(ofType: EndAccessInst.self).lazy.map { $0.parentBlock })
      valueToComplete = ba
    default:
      return
    }
  case .unowned:
    return
  }

  endBlocks.insert(contentsOf: valueToComplete.uses.endingLifetime.lazy.map { $0.instruction.parentBlock })

  var liveBlocks = BasicBlockWorklist(context)
  defer { liveBlocks.deinitialize() }

  liveBlocks.pushIfNotVisited(valueToComplete.parentBlock)
  while let block = liveBlocks.pop() {
    if endBlocks.contains(block) {
      continue
    }
    if let unreachable = block.terminator as? UnreachableInst {
      let builder = Builder(before: unreachable, context)
      switch valueToComplete.ownership {
      case .owned:
        builder.createEndLifetime(of: valueToComplete)
      case .guaranteed:
        builder.createEndBorrow(of: valueToComplete)
      case .none:
        switch valueToComplete {
        case let sb as StoreBorrowInst:
          builder.createEndBorrow(of: sb)
        case let ba as BeginAccessInst:
          builder.createEndAccess(beginAccess: ba)
        case let mv as MoveValueInst:
          builder.createExtendLifetime(of: mv)
        default:
          fatalError("wrong value to complete")
        }
      case .unowned:
        fatalError("wrong ownership")
      }
    } else {
      liveBlocks.pushIfNotVisited(contentsOf: block.successors)
    }
  }
}

func registerLifetimeCompletion() {
  BridgedOptimizerUtilities.registerLifetimeCompletion(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction, includeTrivialVars: Bool) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let function = bridgedFunction.function;
      completeLifetimes(in: function, includeTrivialVars: includeTrivialVars, context)
    }
  )
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let lifetimeComletionTest = FunctionTest("lifetime_completion") {
  function, arguments, context in

  completeLifetimes(in: function, context)
}
