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

  guard var ownershipUses = OwnershipUses(of: load, context) else {
    return
  }
  defer { ownershipUses.deinitialize() }

  let aliasAnalysis = context.aliasAnalysis
  var liferange = InstructionWorklist(context)
  defer { liferange.deinitialize() }

  liferange.markAsPushed(load)

  for destroy in ownershipUses.finalDestroys {
    if liferange.mayWrite(to: load.address, before: destroy, aliasAnalysis) {
      return
    }
  }

  let builder = Builder(before: load, context)
  let loadBorrow = builder.createLoadBorrow(fromAddress: load.address)
  load.uses.replaceAll(with: loadBorrow, context)
  context.erase(instruction: load)

  for destroy in ownershipUses.finalDestroys {
    if !liferange.hasBeenPushed(destroy) {
      let builder = Builder(before: destroy, context)
      builder.createEndBorrow(of: loadBorrow)
    }
    context.erase(instruction: destroy)
  }

  for forwardingUse in ownershipUses.forwardingUses {
    forwardingUse.changeOwnership(from: .owned, to: .guaranteed, context)
  }

  for liferangeExitBlock in ownershipUses.nonDestroyingLiferangeExits {
    let builder = Builder(atBeginOf: liferangeExitBlock, context)
    builder.createEndBorrow(of: loadBorrow)
  }
}

private struct OwnershipUses {
  private(set) var forwardingUses: Stack<Operand>
  private(set) var finalDestroys: Stack<DestroyValueInst>
  private(set) var nonDestroyingLiferangeExits: Stack<BasicBlock>

  init?(of ownedValue: Value, _ context: FunctionPassContext) {
    self.forwardingUses = Stack(context)
    self.finalDestroys = Stack(context)
    self.nonDestroyingLiferangeExits = Stack(context)

    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(ownedValue)

    while let value = worklist.pop() {

      var lifetimeEndUseFound = false

      for use in value.uses {
        if !use.isLifetimeEnding {
          continue
        }
        lifetimeEndUseFound = true

        switch use.instruction {
        case let destroy as DestroyValueInst:
          finalDestroys.append(destroy)
        case let forwarding as OwnershipForwardingInstruction:
          if !forwarding.preservesOwnership {
            deinitialize()
            return nil
          }
          if !forwarding.canForwardGuaranteedValues {
            deinitialize()
            return nil
          }
          let numOwnedOperands = forwarding.operands.lazy.filter({ $0.value.ownership == .owned }).count
          if numOwnedOperands > 1 {
            deinitialize()
            return nil
          }
          if let termInst = forwarding as? TermInst {
            for succ in termInst.successors {
              if let forwardedArg = succ.arguments.first {
                worklist.pushIfNotVisited(forwardedArg)
              } else {
                nonDestroyingLiferangeExits.append(succ)
              }
            }
          } else {
            worklist.pushIfNotVisited(contentsOf: forwarding.results)
          }
          forwardingUses.append(use)
        default:
          deinitialize()
          return nil
        }
      }
      if !lifetimeEndUseFound {
        deinitialize()
        return nil
      }
    }
  }

  mutating func deinitialize() {
    forwardingUses.deinitialize()
    finalDestroys.deinitialize()
    nonDestroyingLiferangeExits.deinitialize()
  }
}

private extension InstructionWorklist {
  mutating func mayWrite(to address: Value, before toInst: Instruction, _ aliasAnalysis: AliasAnalysis) -> Bool {
    pushPredecessors(of: toInst)

    while let inst = pop() {
      switch inst {
      case is BeginBorrowInst, is EndBorrowInst:
        break
      default:
        if inst.mayWrite(toAddress: address, aliasAnalysis) {
          return true
        }
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
