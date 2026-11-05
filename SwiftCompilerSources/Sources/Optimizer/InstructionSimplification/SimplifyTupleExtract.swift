//===--- SimplifyTupleExtract.swift ---------------------------------------===//
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

extension TupleExtractInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {

    // Replace tuple_extract(tuple(x)) -> x

    if let tupleInst = tuple as? TupleInst {
      context.tryReplaceRedundantInstructionPair(first: tupleInst, second: self,
                                                 with: tupleInst.operands[fieldIndex].value)
      return
    }

    // Replace tuple_extract(begin_borrow(tuple(x))) -> x, if x has no ownership.
    // See the corresponding struct_extract simplification.
    if let borrowedTuple = tuple.lookThroughBorrow as? TupleInst,
       case let element = borrowedTuple.operands[fieldIndex].value,
       element.ownership == .none
    {
      self.replace(with: element, context)
    }
  }
}
