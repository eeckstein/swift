//===--- SimplifyDestroyValue.swift ---------------------------------------===//
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

extension DestroyValueInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    // If the value has `.none` ownership, the destroy is a no-op. Note that a value can have `.none`
    // ownership even if it's type is not trivial, e.g.
    //
    // ```
    //   %1 = enum $NonTrivialEnum, #NonTrivialEnum.trivialCase!enumelt  // ownership: none
    //   %2 = destroy_value %1
    // ```
    //
    if destroyedValue.ownership == .none {
      context.erase(instruction: self)
      return
    }

    if replaceWithDeadEndDestroy(context) {
      return
    }

    if let phi = Phi(destroyedValue) {
      tryHoistDestroysIntoPredecessors(of: phi, context)
      return
    }

    if let forwardingInst = destroyedValue.asOptimizableForwardingInstruction {
      tryRemoveForwardingInstruction(forwardingInst, context)
    }
  }

  /// A `destroy_value` which comes after a no-return call is never executed. Therefore it can be
  /// replaced by a `destroy_value [dead_end]`:
  /// ```
  ///   apply %fatalError() : $@convention(thin) () -> Never
  ///   destroy_value %0
  ///   unreachable
  /// ```
  /// ->
  /// ```
  ///   apply %fatalError() : $@convention(thin) () -> Never
  ///   destroy_value [dead_end] %0
  ///   unreachable
  /// ```
  /// Unlike a regular `destroy_value`, a dead-end destroy doesn't count as a release of an object,
  /// e.g. for dead object elimination.
  private func replaceWithDeadEndDestroy(_ context: SimplifyContext) -> Bool {
    guard !isDeadEnd,
          parentBlock.terminator is UnreachableInst,
          ReverseInstructionList(first: previous).contains(where: { ($0 as? ApplyInst)?.isCalleeNoReturn ?? false })
    else {
      return false
    }
    Builder(before: self, context).createDestroyValue(operand: destroyedValue, isDeadEnd: true)
    context.erase(instruction: self)
    return true
  }

  /// Moves a `destroy_value` of a phi argument to the phi's predecessor blocks.
  ///
  /// ```
  /// bb1:
  ///   br bb3(%0)
  /// bb2:
  ///   br bb3(%1)
  /// bb3(%3 : @owned T):
  ///   ...                // no deinit-barriers
  ///   destroy_value %3   // the only use of %3
  /// ```
  /// ->
  /// ```
  /// bb1:
  ///   destroy_value %0
  ///   br bb3
  /// bb2:
  ///   destroy_value %1
  ///   br bb3
  /// bb3:
  ///   ...
  /// ```
  ///
  /// Re-borrow phis which are borrowed from the phi and which are only ended in the phi's block,
  /// are removed as well:
  ///
  /// ```
  /// bb1:
  ///   %2 = begin_borrow %0
  ///   br bb3(%0, %2)
  /// ...
  /// bb3(%3 : @owned T, %4 : @reborrow T):
  ///   %5 = borrowed %4 from (%3)
  ///   end_borrow %5
  ///   destroy_value %3
  /// ```
  /// ->
  /// ```
  /// bb1:
  ///   %2 = begin_borrow %0
  ///   end_borrow %2
  ///   destroy_value %0
  ///   br bb3
  /// ...
  /// ```
  ///
  private func tryHoistDestroysIntoPredecessors(of phi: Phi, _ context: SimplifyContext) {
    let phiBlock = phi.value.parentBlock

    guard phiBlock == parentBlock,
          // Dead-end destroys are no-ops, anyway. Don't try to move them away from an `unreachable` instruction.
          !isDeadEnd,
          let reborrows = phi.endedReborrows(context),
          !isDeinitBarrierInBlock(before: self, context)
    else {
      return
    }

    for reborrow in reborrows {
      for incomingOp in reborrow.incomingOperands {
        Builder(before: incomingOp.instruction, context).createEndBorrow(of: incomingOp.value)
      }
      // Users of the `borrowed_from` are `end_borrow` and `debug_value` instructions.
      context.erase(instructionIncludingAllUsers: reborrow.borrowedFrom!)
      erasePhiArgument(phi: reborrow, context)
    }

    for incomingOp in phi.incomingOperands {
      Builder(before: incomingOp.instruction, context).createDestroyValue(operand: incomingOp.value)
    }

    // Users of `phi` include `debug_value` instructions and this `destroy_value`
    context.erase(instructions: phi.value.users)

    erasePhiArgument(phi: phi, context)
  }
}

private extension Phi {
  /// Returns the re-borrow phis which are borrowed from this phi, in case this phi is only used by
  /// `destroy_value`s and such re-borrows, which are only ended by `end_borrow`s in the phi's block.
  /// Returns nil if this phi has other uses.
  func endedReborrows(_ context: SimplifyContext) -> [Phi]? {
    var reborrows = [Phi]()
    for use in value.uses {
      switch use.instruction {
      case is DestroyValueInst:
        break
      case is DebugValueInst where !context.preserveDebugInfo:
        break
      case let borrowedFrom as BorrowedFromInst:
        guard use.index != 0,
              borrowedFrom.borrowedPhi.isReborrow,
              // The re-borrow must not be an enclosing value of other guaranteed phis.
              borrowedFrom.borrowedValue.uses.singleUse != nil,
              borrowedFrom.uses.allSatisfy({
                ($0.instruction is EndBorrowInst && $0.instruction.parentBlock == value.parentBlock) ||
                ($0.instruction is DebugValueInst && !context.preserveDebugInfo)
              })
        else {
          return nil
        }
        // The phi can be used as multiple enclosing values of the same `borrowed_from`.
        if !reborrows.contains(where: { $0.value == borrowedFrom.borrowedValue }) {
          reborrows.append(borrowedFrom.borrowedPhi)
        }
      default:
        return nil
      }
    }
    // Other re-borrows in the phi's block may be nested in the lifetime of this phi or in the removed
    // borrow scopes (e.g. a `load_borrow` from a field of the borrowed reference). Ending the
    // lifetimes in the predecessors would then violate the nesting.
    let otherReborrowExists = value.parentBlock.arguments.contains { arg in
      arg.isReborrow && !reborrows.contains(where: { $0.value == arg })
    }
    if otherReborrowExists {
      return nil
    }
    return reborrows
  }
}

/// If `forwardingInst` has no other users than `destroy_value` (except `fix_lifetime` and `debug_value`),
/// remove the `forwardingInst` and destroy its (owned) operands instead.
///
/// ```
///   %3 = struct $S (%1, %2)
///   destroy_value %3         // the only use of %3
/// ```
/// ->
/// ```
///   destroy_value %1
///   destroy_value %2
/// ```
///
private func tryRemoveForwardingInstruction(_ forwardingInst: SingleValueInstruction, _ context: SimplifyContext) {

  // Support fix_lifetime as use:
  // ```
  //   %3 = struct $S (%1, %2)
  //   fix_lifetime %3
  //   destroy_value %3         // the only use of %3, except `fix_lifetime`
  // ```
  // ->
  // ```
  //   fix_lifetime %1
  //   fix_lifetime %2
  //   destroy_value %1
  //   destroy_value %2
  // ```
  guard forwardingInst.isOnlyDestroyed(ignoringFixLifetime: true, context) else {
    return
  }

  for user in forwardingInst.users {
    switch user {
    case is DebugValueInst:
      break
    case let destroy as DestroyValueInst:
      let builder = Builder(before: destroy, context)
      for op in forwardingInst.definedOperands where op.value.ownership == .owned {
        builder.createDestroyValue(operand: op.value, isDeadEnd: destroy.isDeadEnd)
      }
    case let fixLifetime as FixLifetimeInst:
      let builder = Builder(before: fixLifetime, context)
      for op in forwardingInst.definedOperands where op.value.ownership == .owned {
        builder.createFixLifetime(operand: op.value)
      }
    default:
      fatalError("unexpected user")
    }
  }

  // Users include `destroy_value`, `fix_lifetime` and `debug_value` instructions.
  context.erase(instructionIncludingAllUsers: forwardingInst)
}

private extension Value {
  func isOnlyDestroyed(ignoringFixLifetime: Bool, _ context: SimplifyContext) -> Bool {
    for user in users {
      switch user {
      case is DestroyValueInst:
        break
      case is DebugValueInst where !context.preserveDebugInfo:
        break
      case is FixLifetimeInst where ignoringFixLifetime:
        break
      default:
        return false
      }
    }
    return true
  }

  /// Return this value as a SingleValueInstruction if it is a forwarding instruction which can
  /// be optimized by `tryRemoveForwardingInstruction`.
  var asOptimizableForwardingInstruction: SingleValueInstruction? {
    switch self {
    case is StructInst,
         is EnumInst:
      guard type.nominal!.valueTypeDestructor == nil else {
        // Moving the destroy to a non-copyable struct/enum's operands would drop the deinit call!
        return nil
      }
      return self as? SingleValueInstruction

    // Handle various "forwarding" instructions that simply pass through values
    // without performing operations that would affect destruction semantics.
    //
    // We are intentionally _not_ handling `unchecked_enum_data`, because that would not necessarily be
    // a simplification, because destroying the whole enum is more effort than to destroy an enum payload.
    // We are also not handling `destructure_struct` and `destructure_tuple`. That would end up in
    // an infinite simplification loop in MandatoryPerformanceOptimizations because there we "split" such
    // destroys again when de-virtualizing deinits of non-copyable types.
    //
    case let cast as UnconditionalCheckedCastInst:
      return cast.preservesReferenceCounts ? cast : nil

    case is TupleInst,
         is RefToBridgeObjectInst,
         is ConvertFunctionInst,
         is ThinToThickFunctionInst,
         is UpcastInst,
         is UncheckedRefCastInst,
         is BridgeObjectToRefInst,
         is InitExistentialRefInst,
         is OpenExistentialRefInst:
      return self as? SingleValueInstruction

    default:
      return nil
    }
  }
}

private func isDeinitBarrierInBlock(before instruction: Instruction, _ context: SimplifyContext) -> Bool {
  return ReverseInstructionList(first: instruction.previous).contains(where: {
    $0.isDeinitBarrier(context.calleeAnalysis)
  })
}
