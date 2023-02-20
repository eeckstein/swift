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

extension BeginBorrowInst : OnoneSimplifyable {
  func simplify(_ context: SimplifyContext) {
    if isLexical {
      tryRemoveLexical(context)
    }
  }
}

private extension BeginBorrowInst {
  func tryRemoveLexical(_ context: SimplifyContext) {
    if borrowedValue.ownership != .owned {
      return
    }

    var ownedValueLifetimeEndings = InstructionSet(context)
    defer { ownedValueLifetimeEndings.deinitialize() }

    for ownedUse in borrowedValue.uses {
      switch ownedUse.ownership {
      case .destroyingConsume, .forwardingConsume:
        ownedValueLifetimeEndings.insert(ownedUse.instruction)
      default:
        break
      }
    }

    for use in uses {
      switch use.instruction {
      case is BranchInst:
        return
      case is EndBorrowInst:
        if !use.instruction.isFollowedByEndOfLifetime(ownedValueLifetimeEndings, context) {
          return
        }
      default:
        break
      }
    }
    let builder: Builder
    if let di = borrowedValue.definingInstruction {
      builder = Builder(after: di, context)
    } else if let arg  = borrowedValue as? Argument {
      builder = Builder(atBeginOf: arg.parentBlock, context)
    } else {
      return
    }
    let move = builder.createMoveValue(operand: borrowedValue, isLexical: true)
    borrowedValue.uses.replaceAll(with: move, except: move, context)
    removeIsLexical(context)
  }
}

private extension Instruction {
  func isFollowedByEndOfLifetime(_ endOfLifetimeInsts: InstructionSet, _ context: SimplifyContext) -> Bool {
    var currentInst = next
    let calleeAnalysis = context.calleeAnalysis
    while let i = currentInst {
      if endOfLifetimeInsts.contains(i) {
        return true
      }
      if i.isDeinitBarrier(calleeAnalysis) {
        return false
      }
      currentInst = i.next
    }
    return false
  }
}
