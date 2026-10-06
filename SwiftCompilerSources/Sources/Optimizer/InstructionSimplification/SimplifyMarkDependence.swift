//===--- SimplifyMarkDependence.swift -------------------------------------===//
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

// Note: this simplification cannot run before dependency diagnostics.
// See `var isRedundant` below.

extension MarkDependenceInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if isRedundant ||
       // A literal lives forever, so no mark_dependence is needed.
       // This pattern can occur after StringOptimization when a utf8CString of a literal is replaced
       // by the string_literal itself.
       value.isLiteral
    {
      replace(with: value, context)
      return
    }
    simplifyBaseOperand(context)
  }
}

extension MarkDependenceAddrInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if isRedundant {
      context.erase(instruction: self)
      return
    }
    simplifyBaseOperand(context)
  }
}

private extension MarkDependenceInstruction {
  var isRedundant: Bool {
    if base.type.isObject && base.type.isTrivial(in: base.parentFunction)
         && !(base.definingInstruction is BeginApplyInst) {
      // Sometimes due to specialization/builtins, we can get a mark_dependence whose base is a trivial
      // typed object. Trivial values live forever. Therefore the mark_dependence does not have a meaning.
      // begin_apply is a special case. A dependency on the token is limited to the coroutine scope (ideally, the token
      // would have a non-trivial type like $Builtin.Token).
      //
      // Note: the mark_dependence is still needed for lifetime diagnostics. So it's important that this
      //       simplification does not run before the lifetime diagnostic pass.
      return true
    }
    // If the value is an address projection from the base the mark_dependence is not needed because the
    // base cannot be destroyed before the accessing the value, anyway.
    if valueOrAddress.type.isAddress, base.type.isAddress,
       // But we still need to keep the mark_dependence for non-escapable types because a non-escapable
       // value can be copied and copies must not outlive the base.
       valueOrAddress.type.isEscapable(in: parentFunction),
       base.accessPath.isEqualOrContains(valueOrAddress.accessPath)
    {
      return true
    }
    return false
  }

  func simplifyBaseOperand(_ context: SimplifyContext) {
    // Replace the base operand with the operand of the base value if it's a certain kind of forwarding
    // instruction.
    if parentFunction.hasOwnership {
      while let operand = base.enumOrExistentialRefOperand {
        switch (base.ownership, operand.ownership) {
        case (.guaranteed, .guaranteed):
          // The operand is in the same borrow scope as the forwarded value.
          baseOperand.set(to: operand, context)
        case (.owned, .owned):
          guard removeOwnedForwardingBase(operand: operand, context) else {
            return
          }
        default:
          return
        }
      }
      return
    }
    while let operand = base.enumOrExistentialRefOperand {
      baseOperand.set(to: operand, context)
    }
  }

  /// Removes an owned forwarding instruction which is only used as the base and destroyed:
  /// ```
  ///   %2 = enum $Optional<C>, #Optional.some!enumelt, %1
  ///   %3 = mark_dependence %0 on %2
  ///   destroy_value %2
  /// ```
  /// ->
  /// ```
  ///   %3 = mark_dependence %0 on %1
  ///   destroy_value %1
  /// ```
  private func removeOwnedForwardingBase(operand: Value, _ context: SimplifyContext) -> Bool {
    let forwardingInst = base as! SingleValueInstruction
    for use in forwardingInst.uses {
      switch use.instruction {
      case is DestroyValueInst:
        break
      case is DebugValueInst where !context.preserveDebugInfo:
        break
      default:
        if use != baseOperand {
          return false
        }
      }
    }
    baseOperand.set(to: operand, context)
    for user in forwardingInst.users {
      if let destroy = user as? DestroyValueInst {
        Builder(before: destroy, context).createDestroyValue(operand: operand, isDeadEnd: destroy.isDeadEnd)
      }
    }
    // The remaining users are `destroy_value` and `debug_value` instructions.
    context.erase(instructionIncludingAllUsers: forwardingInst)
    return true
  }
}

private extension Value {
  /// True, if this is a literal instruction or a struct of a literal instruction.
  /// What we want to catch here is a `UnsafePointer<Int8>` of a string literal.
  var isLiteral: Bool {
    switch self {
    case let s as StructInst:
      if let singleOperand = s.operands.singleElement {
        return singleOperand.value.isLiteral
      }
      return false
    case  is IntegerLiteralInst, is FloatLiteralInst, is StringLiteralInst:
      return true
    default:
      return false
    }
  }

  var enumOrExistentialRefOperand: Value? {
    switch self {
    case let e as EnumInst:
      return e.payload
    case let ier as InitExistentialRefInst:
      return ier.instance
    case let oer as OpenExistentialRefInst:
      return oer.existential
    default:
      return nil
    }
  }
}
