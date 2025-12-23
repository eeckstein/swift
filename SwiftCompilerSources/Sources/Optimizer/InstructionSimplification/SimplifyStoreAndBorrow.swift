//===--- SimplifyStoreAndBorrow.swift -------------------------------------===//
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

extension StoreAndBorrowInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {

    // Replace a `store_and_borrow` with a `store` if its result is not used.
    // Example:
    // ```
    //   %2 = store_and_borrow %1 to %0
    //   end_borrow %2
    // ```
    // ->
    // ```
    //   store %1 to [init] %0
    // ```
    if uses.ignore(usersOfType: EndBorrowInst.self).ignoreDebugUses.isEmpty {
      let builder = Builder(before: self, context)
      builder.createStore(source: source, destination: destination, ownership: .initialize)
      context.erase(instructionIncludingAllUsers: self)
    }
  }
}
