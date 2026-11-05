//===--- SimplifyStructExtract.swift --------------------------------------===//
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

extension StructExtractInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if let structInst = self.struct as? StructInst {
      context.tryReplaceRedundantInstructionPair(first: structInst, second: self,
                                                 with: structInst.operands[fieldIndex].value)
      return
    }
    // Replace struct_extract(begin_borrow(struct(x))) -> x, if x has no ownership:
    // ```
    //   %1 = struct $S (%0, %owned)    // %0 has "none" ownership
    //   %2 = begin_borrow %1
    //   %3 = struct_extract %2, #S.field0
    // ```
    // ->
    // ```
    //   // replace %3 with %0
    // ```
    if let borrowedStruct = self.struct.lookThroughBorrow as? StructInst,
       case let field = borrowedStruct.operands[fieldIndex].value,
       field.ownership == .none
    {
      self.replace(with: field, context)
    }
  }
}
