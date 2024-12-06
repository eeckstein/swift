//===--- SimplifyCondFail.swift -------------------------------------------===//
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

extension CondFailInst : OnoneSimplifyable, SILCombineSimplifyable {
  func simplify(_ context: SimplifyContext) {

    guard let literal = condition as? IntegerLiteralInst,
          let value = literal.value else
    {
      return
    }
    if value == 0 {

      /// Eliminates
      /// ```
      ///   %0 = integer_literal 0
      ///   cond_fail %0, "message"
      /// ```
      context.erase(instruction: self)
    } else {

      /// Cuts of the control flow after the (unconditional) cond_fail
      /// ```
      ///   %0 = integer_literal 1
      ///   cond_fail %0, "message"
      ///   some_other_instructions
      /// ```
      /// ->
      /// ```
      ///   %0 = integer_literal 1
      ///   cond_fail %0, "message"
      ///   unreachable
      /// split_block:                      // becomes a dead block
      ///   some_other_instructions
      /// ```
      if context.runsInSILCombine {
        // SILCombine is not allowed to change the CFG
        return
      }
      // Do not cut off any instructions, which would be inserted by e.g. lifetime completion again.
      if InstructionList(first: self.next!).allSatisfy({ $0.isRequiredAtDeadEnd }) {
        return
      }
      // Move all instructions to a new block, which is a dead block.
      // We cannot easily delete those instructions, because they might have uses in other blocks or
      // - even worse - are scope beginning instructions (like begin_borrow) with scope-ending instructions
      // in other blocks.
      _ = context.splitBlock(after: self)
      let builder = Builder(after: self, context)
      builder.createUnreachable()
    }
  }
}

private extension Instruction {
  var isRequiredAtDeadEnd: Bool {
    switch self {
    case // Avoid deleting and re-creating an `unreachable`
         is UnreachableInst,
         // Instructions which would be re-inserted by lifetime completion.
         is DestroyValueInst, is EndBorrowInst:
      return true
    default:
      return false
    }
  }
}
