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
  context.setNeedBreakInfiniteLoops(to: false)

  guard function.hasOwnership else {
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

  let backEdgeBlock = context.createBlock(after: block)
  Builder(atEndOf: backEdgeBlock, location: branch.location, context).createBranch(to: branch.targetBlock)

  let deadEndBlock = context.createBlock(after: backEdgeBlock)
  Builder(atEndOf: deadEndBlock, location: branch.location, context).createUnreachable()

  let builder = Builder(before: branch, context)
  let trueValue = builder.createBuiltin(name: "infinite_loop_true_condition",
                                        type: context.getBuiltinIntegerType(bitWidth: 1),
                                        arguments: [])
  builder.createCondBranch(condition: trueValue, trueBlock: backEdgeBlock, falseBlock: deadEndBlock)
  context.erase(instruction: branch)
  return deadEndBlock
}

private extension BasicBlock {
  func isEntryToInfiniteLoopRegion(_ noInfiniteLoops: BasicBlockWorklist) -> Bool {
    return !noInfiniteLoops.hasBeenPushed(self) &&
           (predecessors.contains{ noInfiniteLoops.hasBeenPushed($0) } ||
            predecessors.isEmpty)
  }
}

func registerControlFlowUtils() {
  BridgedOptimizerUtilities.registerControlFlowUtils(
    { (bridgedCtxt: BridgedContext, bridgedFunction: BridgedFunction) in
      let context = FunctionPassContext(_bridged: bridgedCtxt)
      let function = bridgedFunction.function;
      breakInfiniteLoops(in: function, context)
    }
  )
}

//===--------------------------------------------------------------------===//
//                              Tests
//===--------------------------------------------------------------------===//

let breakInfiniteLoopsTest = FunctionTest("break_infinite_loops") {
  function, arguments, context in

  breakInfiniteLoops(in: function, context)
}
