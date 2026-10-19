//===--- SimplifyUncheckedBitwiseCast.swift -------------------------------===//
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

extension UncheckedBitwiseCastInst : Simplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    // Casts the bits of a reference back to a reference, e.g. when a String is created from a
    // `__StringStorage`:
    // ```
    //   %1 = unchecked_trivial_bit_cast %0 to $UInt64
    //   %2 = struct_extract %1, #UInt64._value
    //   %3 = unchecked_bitwise_cast %2 to $Builtin.BridgeObject
    // ```
    // ->
    // ```
    //   %3 = unchecked_ref_cast %0 to $Builtin.BridgeObject
    // ```
    // In OSSA, this converts the unowned bitwise cast to a forwarding instruction, which enables
    // removing copies of the result.
    _ = optimizeCastOfReferenceBits(context)
  }
}

private extension UncheckedBitwiseCastInst {
  func optimizeCastOfReferenceBits(_ context: SimplifyContext) -> Bool {
    guard type.isHeapObjectReferenceType,
          let bitCast = bitCastOfReference,
          canReplace(withCastOf: bitCast, context)
    else {
      return false
    }
    let builder = Builder(before: self, context)
    let refCast = builder.createUncheckedRefCast(from: bitCast.fromValue, to: type)
    replace(with: refCast, context)
    return true
  }

  /// Returns the `unchecked_trivial_bit_cast` if the operand is the unchanged bits of a reference.
  var bitCastOfReference: UncheckedTrivialBitCastInst? {
    var value = fromValue
    while let structExtract = value as? StructExtractInst {
      guard let fields = structExtract.struct.type.getNominalFields(in: parentFunction),
            fields.count == 1
      else {
        return nil
      }
      value = structExtract.struct
    }
    guard let bitCast = value as? UncheckedTrivialBitCastInst,
          bitCast.fromValue.type.isHeapObjectReferenceType || bitCast.fromValue.type.isClassExistential
    else {
      return nil
    }
    return bitCast
  }

  func canReplace(withCastOf bitCast: UncheckedTrivialBitCastInst, _ context: SimplifyContext) -> Bool {
    if !parentFunction.hasOwnership {
      return true
    }
    let reference = bitCast.fromValue
    switch reference.ownership {
    case .none:
      return true
    case .guaranteed:
      // The result of the bitwise cast is unowned, i.e. it can be used outside the borrow scope of
      // the reference. Only allow uses which also work for a guaranteed value and which are inside the
      // borrow scope.
      guard uses.ignoreDebugUses.allSatisfy({ $0.instruction is CopyValueInst }) else {
        return false
      }
      var range = InstructionRange(begin: bitCast, ends: users, context)
      defer { range.deinitialize() }
      return range.isFullyContainedIn(scopeOf: reference)
    case .owned, .unowned:
      // An `unchecked_ref_cast` of an owned value would consume the reference.
      return false
    }
  }
}
