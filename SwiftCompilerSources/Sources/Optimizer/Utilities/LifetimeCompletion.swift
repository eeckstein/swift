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

func completeLifetimes(in function: Function, _ context: FunctionPassContext) {
  context.setNeedCompleteLifetimes(to: false)

  guard function.hasOwnership else {
    return
  }

  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }

  context.updateAnalysis()
  let dominatorTree = context.dominatorTree

  var blocksToComplete = IterableBasicBlockSet(context)
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

  // Process blocks in reverse dominance order.
  for block in blocksToComplete {
    for inst in block.instructions.reversed() {
      for result in inst.results {
        completeLifetime(of: result, context)
      }
    }
    for arg in block.arguments {
      completeLifetime(of: arg, context)
    }
  }
}

func completeLifetime(of value: Value, _ context: FunctionPassContext) {
  let valueToComplete: Value
  switch value.ownership {
  case .owned:
    valueToComplete = value
  case .guaranteed:
    guard let beginBorrow = BeginBorrowValue(value) else {
      return
    }
    switch beginBorrow {
    case .beginBorrow, .loadBorrow, .beginApply:
      valueToComplete = value
    case .reborrow(let phi):
      valueToComplete = phi.borrowedFrom!
    case .uncheckOwnershipConversion, .functionArgument:
      return
    }
  case .none:
    guard value is StoreBorrowInst else {
      return
    }
    valueToComplete = value
  case .unowned:
    return
  }

  var endBlocks = BasicBlockSet(context)
  defer { endBlocks.deinitialize() }

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
      if valueToComplete.ownership == .owned {
        builder.createDestroyValue(operand: valueToComplete, isDeadEnd: true)
      } else {
        builder.createEndBorrow(of: valueToComplete)
      }
    } else {
      liveBlocks.pushIfNotVisited(contentsOf: block.successors)
    }
  }
}

func registerLifetimeCompletion() {
  BridgedOptimizerUtilities.registerLifetimeCompletion(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let function = bridgedFunction.function;
      completeLifetimes(in: function, context)
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
