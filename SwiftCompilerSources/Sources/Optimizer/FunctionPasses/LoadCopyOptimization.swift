//===--- LoadCopyOptimization.swift ----------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

let loadCopyOptimization = FunctionPass(name: "load-copy-optimization") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    if let load = inst as? LoadInst {
      optimize(load: load, context)
    }
  }
}

private func optimize(load: LoadInst, _ context: FunctionPassContext) {
  if load.loadOwnership != .copy {
    return
  }

  guard let destroys = getFinalDestroys(of: load, context) else {
    return
  }

  let aliasAnalysis = context.aliasAnalysis
  var liferange = InstructionWorklist(context)
  defer { liferange.deinitialize() }

  liferange.markAsPushed(load)

  for destroy in destroys {
    if liferange.mayWrite(to: load.address, before: destroy, aliasAnalysis) {
      return
    }
  }

  let builder = Builder(before: load, context)
  let loadBorrow = builder.createLoadBorrow(fromAddress: load.address)
  load.uses.replaceAll(with: loadBorrow, context)
  context.erase(instruction: load)

  for destroy in destroys {
    if !liferange.hasBeenPushed(destroy) {
      let builder = Builder(before: destroy, context)
      builder.createEndBorrow(of: loadBorrow)
    }
    context.erase(instruction: destroy)
  }
}

private func getFinalDestroys(of ownedValue: Value, _ context: FunctionPassContext) -> [DestroyValueInst]? {
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(ownedValue)

  var destroys = [DestroyValueInst]()

  while let value = worklist.pop() {
    for use in value.uses {
      if !use.isLifetimeEnding {
        continue
      }

      switch use.instruction {
      case let destroy as DestroyValueInst:
        destroys.append(destroy)
      case let forwarding as OwnershipForwardingInstruction:
        if !forwarding.preservesOwnership {
          return nil
        }
        if !forwarding.canForwardGuaranteedValues {
          return nil
        }
        let numOwnedOperands = forwarding.operands.lazy.filter({ $0.value.ownership == .owned }).count
        if numOwnedOperands > 1 {
          return nil
        }
        if let termInst = forwarding as? TermInst {
          for succ in termInst.successors where succ.arguments.isEmpty {
            worklist.pushIfNotVisited(succ.arguments.first!)
          }
        } else {
          worklist.pushIfNotVisited(contentsOf: forwarding.results)
        }
      default:
        return nil
      }
    }
  }
  return destroys
}

private extension InstructionWorklist {
  mutating func mayWrite(to address: Value, before toInst: Instruction, _ aliasAnalysis: AliasAnalysis) -> Bool {
    pushPredecessors(of: toInst)

    while let inst = pop() {
      if inst.mayWrite(toAddress: address, aliasAnalysis) {
        return true
      }
      pushPredecessors(of: inst)
    }
    return false
  }

  private mutating func pushPredecessors(of inst: Instruction) {
    if let prevInst = inst.previous {
      pushIfNotVisited(prevInst)
    } else {
      pushIfNotVisited(contentsOf: inst.parentBlock.predecessors.lazy.map { $0.terminator})
    }
  }
}
