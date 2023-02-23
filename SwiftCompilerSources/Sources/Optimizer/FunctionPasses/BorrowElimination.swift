//===--- SimplifyBeginBorrow.swift ----------------------------------------===//
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

let borrowEliminationPass = FunctionPass(name: "borrow-elimination") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    if let bi = inst as? BeginBorrowInst {
      if bi.isLexical {
        tryRemove(beginBorrow: bi, context)
      }
    }
  }
}

private func tryRemove(beginBorrow: BeginBorrowInst, _ context: FunctionPassContext) {
  if beginBorrow.hasEscapingUses {
    return
  }

  if beginBorrow.isLexical {
    if !tryRemoveLexical(of: beginBorrow, context) {
      return
    }
  }

  for use in beginBorrow.uses {
    if let eb = use.instruction as? EndBorrowInst {
      context.erase(instruction: eb)
    } else {
      use.set(value: beginBorrow.borrowedValue, context)
    }
  }
  context.erase(instruction: beginBorrow)
}

private extension Value {
  var hasEscapingUses: Bool {
    for use in uses {
      switch use.ownership {
      case .trivialUse,
           .nonUse,
           .borrow,
           .endBorrow,
           .instantaneousUse,
           .unownedInstantaneousUse:
        break
      case .interiorPointer:
        switch use.instruction {
        case is RefElementAddrInst,
             is RefTailAddrInst:
          if (use.instruction as! SingleValueInstruction).hasEscapingAddressUses {
            return true
          }
        default:
          return true
        }
      case .forwardingUnowned,
           .pointerEscape,
           .bitwiseEscape,
           .reborrow:
        return true
      case .guaranteedForwarding:
        guard let svi = use.instruction as? SingleValueInstruction else {
          return true
        }
        if svi.hasEscapingUses {
          return true
        }
      case .destroyingConsume,
           .forwardingConsume:
        fatalError("borrowed value shouldn't have owned use")
      default:
        fatalError()
      }
    }
    return false
  }

  var hasEscapingAddressUses: Bool {
    // TODO
    false
  }
}

private func tryRemoveLexical(of beginBorrow: BeginBorrowInst, _ context: FunctionPassContext) -> Bool {
  let ownedValue = beginBorrow.borrowedValue
  if ownedValue.ownership != .owned {
    return false
  }

  for use in beginBorrow.uses {
    switch use.instruction {
    case is BranchInst:
      return false
    case let eb as EndBorrowInst:
      if !eb.isDominating(anyConsumesOf: ownedValue, context) {
        return false
      }
    default:
      break
    }
  }

  let builder = Builder(before: beginBorrow, context)
  let moveToLexical = builder.createMoveValue(operand: ownedValue, isLexical: true)

  ownedValue.uses.replaceDominated(with: moveToLexical, context)

  for use in beginBorrow.uses {
    if let endBorrow = use.instruction as? EndBorrowInst {
      let builder = Builder(after: endBorrow, context)
      let moveBack = builder.createMoveValue(operand: moveToLexical, isLexical: false)
      moveToLexical.uses.replaceDominated(with: moveBack, context)
    }
  }

  beginBorrow.removeIsLexical(context)

  return true
}


private extension UseList {
  func replaceDominated(with replacement: SingleValueInstruction, _ context: FunctionPassContext) {
    let domTree = context.dominatorTree
    for use in self where replacement.strictlyDominates(use.instruction, domTree) {
      use.set(value: replacement, context)
    }
  }
}

private func tryRemoveMoveValue(moveValue: MoveValueInst, _ context: FunctionPassContext) {
  for use in moveValue.uses {
    if use.ownership.isConsuming,
       use.instruction.parentBlock == moveValue.parentBlock,
       !isDeinitBarrier(between: moveValue, and: use.instruction, context) {

      moveValue.uses.replaceAll(with: moveValue.fromValue, context)
      context.erase(instruction: moveValue)
      return
    }
  }
}

private extension Instruction {
  func isDominating(anyConsumesOf ownedValue: Value, _ context: FunctionPassContext) -> Bool {
    let domTree = context.dominatorTree
    for use in ownedValue.uses where use.ownership.isConsuming {
      if self.dominates(use.instruction, domTree) {
        return true
      }
    }
    return false
  }
}

private func isDeinitBarrier(between fromInst: Instruction, and toInst: Instruction, _ context: FunctionPassContext) -> Bool {
  var currentInst = fromInst.next
  let calleeAnalysis = context.calleeAnalysis
  while let i = currentInst {
    if i == toInst {
      return false
    }
    if i.isDeinitBarrier(calleeAnalysis) {
      return true
    }
    currentInst = i.next
  }
  fatalError("toInst not in same block as fromInst")
}
