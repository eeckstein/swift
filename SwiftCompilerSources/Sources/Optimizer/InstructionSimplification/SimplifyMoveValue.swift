//===--- SimplifyMoveValue.swift ------------------------------------------===//
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

/// The `move_value` instruction is only used to specify flags (e.g. `[lexical]`) for passes in the
/// mandatory pipeline. In the optimizer pipeline `move_value` has no purpose anymore and we can remove it.
///
/// Replaces
///
/// ```
///   %1 = move_value %0
///   use %1
/// ```
/// ->
/// ```
///   use %0
/// ```
///
extension MoveValueInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    replace(with: fromValue, context)
  }
}
