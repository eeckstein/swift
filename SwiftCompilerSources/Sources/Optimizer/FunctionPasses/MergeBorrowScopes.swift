//===--- MergeBorrowScopes.swift -------------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// Merges to adjacent borrow scopes in a basic block.
///
/// ```
///   %2 = begin_borrow %1
///   use(%2)
///   end_borrow %2
///   ...
///   %6 = begin_borrow %1
///   use(%6)
///   end_borrow %6
///
/// ```
/// ->
/// ```
///   %2 = begin_borrow %1
///   use(%2)
///   ...
///   use(%2)
///   end_borrow %2
/// ```
///
/// This helps other optimizations, like common-subexpression-elimination, because the
/// borrow liveranges are larger and not split.
///
/// Borrow scopes in different blocks are merged, too, if the first `begin_borrow` dominates the
/// second one:
///
/// ```
///   bb1:
///     %2 = begin_borrow %1
///     use(%2)
///     end_borrow %2
///     cond_br %c, bb2, bb3
///   bb2:
///     %6 = begin_borrow %1
///     use(%6)
///     end_borrow %6
///     ...
///   bb3:
///     ...
/// ```
/// ->
/// ```
///   bb1:
///     %2 = begin_borrow %1
///     use(%2)
///     cond_br %c, bb2, bb3
///   bb2:
///     use(%2)
///     end_borrow %2
///     ...
///   bb3:
///     end_borrow %2
///     ...
/// ```
///
let mergeBorrowScopes = FunctionPass(name: "merge-borrow-scopes") {
    (function: Function, context: FunctionPassContext) in
  for block in function.blocks {
    doMergeBorrowScopes(in: block, context)
  }
  mergeBorrowScopesAcrossBlocks(in: function, context)
}

private func doMergeBorrowScopes(in block: BasicBlock, _ context: FunctionPassContext) {
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

private func mergeBorrowScopesAcrossBlocks(in function: Function, _ context: FunctionPassContext) {
  // Collect the borrow scopes per borrowed value. An array of borrowed values is used to keep the
  // order of merging deterministic.
  var borrowedValues = [Value]()
  var borrowScopes = Dictionary<ObjectIdentifier, [BeginBorrowInst]>()
  for inst in function.instructions {
    if let beginBorrow = inst as? BeginBorrowInst,
       beginBorrow.uses.endingLifetime.hasOnlyUsers(ofType: EndBorrowInst.self)
    {
      let key = ObjectIdentifier(beginBorrow.borrowedValue)
      if borrowScopes[key] == nil {
        borrowedValues.append(beginBorrow.borrowedValue)
      }
      borrowScopes[key, default: []].append(beginBorrow)
    }
  }

  let domTree = context.dominatorTree

  for borrowedValue in borrowedValues {
    let scopesOfValue = borrowScopes[ObjectIdentifier(borrowedValue)]!
    // Avoid quadratic complexity for values with a huge number of borrow scopes.
    if scopesOfValue.count < 2 || scopesOfValue.count > 32 {
      continue
    }
    // The remaining scopes after merging.
    var mergedScopes = [BeginBorrowInst]()
    for scope in scopesOfValue {
      if let dominatingScope = mergedScopes.first(where: { $0.dominates(scope, domTree) }) {
        merge(scope, into: dominatingScope, context)
        continue
      }
      if let idx = mergedScopes.firstIndex(where: { scope.dominates($0, domTree) }) {
        merge(mergedScopes[idx], into: scope, context)
        mergedScopes[idx] = scope
        continue
      }
      mergedScopes.append(scope)
    }
  }
}

/// Merges the borrow scope of `beginBorrow` into the dominating borrow scope `scope`, which borrows
/// the same value.
///
/// It's legal to extend `scope` up to the end of `beginBorrow`'s scope: the borrowed value is alive
/// in `beginBorrow`'s scope, which means that it cannot be consumed on any path from `scope` to
/// the uses in `beginBorrow`'s scope.
private func merge(_ beginBorrow: BeginBorrowInst, into scope: BeginBorrowInst, _ context: FunctionPassContext) {
  var range = InstructionRange(begin: scope, context)
  defer { range.deinitialize() }
  range.insert(contentsOf: scope.uses.endingLifetime.users)
  range.insert(contentsOf: beginBorrow.uses.endingLifetime.users)

  beginBorrow.replace(with: scope, context)

  context.erase(instructions: scope.uses.endingLifetime.users.filter { !range.isEnd($0) })

  for exit in range.exits {
    Builder(before: exit, context).createEndBorrow(of: scope)
  }
}
