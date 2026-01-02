//===--- ControlFlowUtils.swift -------------------------------------------===//
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

func breakInfiniteLoops(in function: Function, _ context: FunctionPassContext) {
  if !function.hasOwnership {
    // The algorithm relies on not having critical edges in the CFG.
    return
  }

  var noInfiniteLoops = BasicBlockWorklist(context)
  defer { noInfiniteLoops.deinitialize() }

  for block in function.blocks {
    if block.successors.isEmpty {
      noInfiniteLoops.transitivelyAddBlockWithPredecessors(startingAt: block)
    }
  }

  var changed: Bool
  repeat {
    changed = false

    for block in function.blocks where block.isEntryToInfiniteLoopRegion(noInfiniteLoops) {
      let newDeadEndBlock = breakInfiniteLoop(startingAt: block, context)
      noInfiniteLoops.transitivelyAddBlockWithPredecessors(startingAt: newDeadEndBlock)
      changed = true
    }
  } while changed
}

private func breakInfiniteLoop(startingAt startBlock: BasicBlock, _ context: FunctionPassContext) -> BasicBlock {
  var visited = BasicBlockSet(context)
  defer { visited.deinitialize() }

  var block = startBlock
  while true {
    guard let succ = block.successors.first else {
      fatalError("all blocks in an inifinite loop region must have at least one successor")
    }
    guard visited.insert(succ) else {
      break
    }
    block = succ
  }

  guard let branch = block.terminator as? BranchInst else {
    fatalError("back-edge of a loop must be a branch instruction")
  }

  let deadEndBlock = context.createBlock(after: block)
  Builder(atBeginOf: deadEndBlock, context).createUnreachable()

  let builder = Builder(before: branch, context)
  let trueValue = builder.createBuiltin(name: "infinite_loop_true_condition",
                                        type: context.getBuiltinIntegerType(bitWidth: 1),
                                        arguments: [])
  builder.createCondBranch(condition: trueValue, trueBlock: branch.targetBlock, falseBlock: deadEndBlock)
  return deadEndBlock
}

private extension BasicBlock {
  func isEntryToInfiniteLoopRegion(_ noInfiniteLoops: BasicBlockWorklist) -> Bool {
    if !noInfiniteLoops.hasBeenPushed(self),
       let pred = singlePredecessor,
       noInfiniteLoops.hasBeenPushed(pred)
    {
      return true
    }
    return false
  }
}
