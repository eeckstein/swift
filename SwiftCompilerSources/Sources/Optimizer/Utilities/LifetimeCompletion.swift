//===--- LifetimeCompletion.swift -----------------------------------------===//
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
import OptimizerBridging

func completeLifetimes(in function: Function, _ context: FunctionPassContext) {
  if !function.hasOwnership {
    return
  }

  context.updateAllAnalysis()

  let deadEndBlocks = context.deadEndBlocks

  // Contains all blocks which dominate or are dead-end blocks.
  // In dominance order.
  var blocksToUpdate = Stack<BasicBlock>(context)
  defer { blocksToUpdate.deinitialize() }

  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }

  for block in function.blocks {
    if deadEndBlocks.isDeadEnd(block) {
      let dominatorTree = context.dominatorTree

      var b = block
      while worklist.pushIfNotVisited(b) {
        guard let parent = dominatorTree.getParent(of: b) else {
          break
        }
        b = parent
      }
      while let b = worklist.pop() {
        blocksToUpdate.push(b)
      }
    }
  }
  // Process blocks in reverse dominance order.
  while let block = blocksToUpdate.pop() {
    for inst in block.instructions.reversed() {
      for result in inst.results {
        context.completeLifetime(of: result)
      }
    }
    for arg in block.arguments {
      context.completeLifetime(of: arg)
    }
  }
}

func registerLifetimeCompletion() {
  BridgedUtilities.registerLifetimeCompletion(
    { (bridgedCtxt: BridgedPassContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let function = bridgedFunction.function;
      completeLifetimes(in: function, context)
    }
  )
}

/// This pass is only used for testing.
/// In the regular pipeline it's not needed because optimization passes must make sure that borrowed-from
/// instructions are updated once the pass finishes.
let completeLifetimesPass = FunctionPass(name: "complete-lifetimes") {
  (function: Function, context: FunctionPassContext) in

  completeLifetimes(in: function, context)
}
