//===--- SimplifyUncheckedOwnershipConversion.swift -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension UncheckedOwnershipConversionInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    guard let refCast = operand.value as? UncheckedRefCastInst,
          let beginBorrow = refCast.fromInstance as? BeginBorrowInst,
          beginBorrow.borrowedValue.ownership == .owned,
          let endBorrowInst = beginBorrow.endInstructions.singleElement,
          endBorrowInst is EndBorrowInst,
          beginBorrow.borrowedValue.uses.ignore(user: beginBorrow).ignoreDebugUses.ignore(usersOfType: EndLifetimeInst.self).isEmpty,
          self.parentBlock == endBorrowInst.parentBlock,
          self.hasNoUsers(before: endBorrowInst)
    else {
      return
    }

    let builder = Builder(after: endBorrowInst, context)
    let newRefCast = builder.createUncheckedRefCast(from: beginBorrow.borrowedValue, to: refCast.type)
    self.replace(with: newRefCast, context)
    context.erase(instruction: refCast)
    context.erase(instructions: beginBorrow.borrowedValue.uses.users(ofType: EndLifetimeInst.self))
  }

  private func hasNoUsers(before otherInst: Instruction) -> Bool {
    for inst in InstructionList(first: self.next) {
      if inst == otherInst {
        return true
      }
      if inst.operands.contains(where: { $0.value == self }) {
        return false
      }
    }
    fatalError("end_borrow expected to be in same block as unchecked_ownership_conversion")
  }
}

