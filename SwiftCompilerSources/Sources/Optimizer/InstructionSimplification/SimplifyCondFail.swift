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
      cutOffControlFlow(after: self, context)
    }
  }
}
