//===--- MergeBorrowScopes.swift -------------------------------------------==//
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

let mergeBorrowScopes = FunctionPass(name: "merge-borrow-scopes") {
    (function: Function, context: FunctionPassContext) in
  for block in function.blocks {
    mergeBorrowScopes1(in: block, context)
  }
}

private func mergeBorrowScopes1(in block: BasicBlock, _ context: FunctionPassContext) {
  var endBorrows = Dictionary<ObjectIdentifier, EndBorrowInst>()

  for inst in block.instructions {
    switch inst {
    case let endBorrow as EndBorrowInst:
      if let beginBorrow = endBorrow.borrow as? BeginBorrowInst {
        endBorrows[ObjectIdentifier(beginBorrow.borrowedValue)] = endBorrow
      }
    case let beginBorrow as BeginBorrowInst:
      if let endBorrow = endBorrows[ObjectIdentifier(beginBorrow.borrowedValue)] {
        endBorrows.removeValue(forKey: ObjectIdentifier(beginBorrow.borrowedValue))
        beginBorrow.replace(with: endBorrow.borrow, context)
        context.erase(instruction: endBorrow)
      }
    default:
      break
    }
  }
}
