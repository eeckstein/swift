//===--- UnownedCopyElimination.swift -------------------------------------===//
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

/// Removes `copy_value`s of unowned values which are not needed to keep the referenced object alive.
///
/// An unowned value cannot be used directly in OSSA - it must be copied first. The most important
/// source of such copies is the self argument of an `@objc` thunk, which the ObjC calling convention
/// passes as unowned:
/// ```
///   sil @thunk : $@convention(objc_method) (C) -> () {
///   bb0(%0 : @unowned $C):
///     %1 = copy_value %0
///     %2 = apply %f(%1) : $@convention(method) (@guaranteed C) -> ()
///     destroy_value %1
/// ```
/// ->
/// ```
///   sil @thunk : $@convention(objc_method) (C) -> () {
///   bb0(%0 : @unowned $C):
///     %1 = unchecked_ownership_conversion %0, @unowned to @guaranteed
///     %2 = apply %f(%1) : $@convention(method) (@guaranteed C) -> ()
///     end_borrow %1
/// ```
///
/// The copy is only needed if the object can be released within the lifetime of the copy. At the
/// `copy_value` the object must be alive, which means that _something else_ is holding a reference to
/// it. If nothing in the liverange can drop a reference, that other reference is still holding the
/// object at the end of the liverange and the copy - and with it a retain/release pair - is redundant.
/// This is the same reasoning which `ARCSequenceOpts` applies to a matching pair of
/// `strong_retain`/`strong_release` after ownership has been lowered.
///
/// The optimization can be done if:
/// * All (forward-extended) uses of the copy support guaranteed ownership.
/// * The (forward-extended) lifetime of the copy ends with `destroy_value`(s).
/// * No instruction within that lifetime can release the referenced object.
///
/// This pass runs immediately before the `OwnershipModelEliminator`, which is the point where the
/// implicit retain of an unowned value would become explicit.
///
let unownedCopyElimination = FunctionPass(name: "unowned-copy-elimination") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }

  var changed = false

  for inst in function.instructions {
    guard let copy = inst as? CopyValueInst,
          copy.fromValue.ownership == .unowned
    else {
      continue
    }
    if !context.continueWithNextSubpassRun(for: copy) {
      return
    }
    if optimize(copy: copy, context) {
      changed = true
    }
  }

  if changed {
    updateBorrowedFrom(in: function, context)
  }
}

private func optimize(copy: CopyValueInst, _ context: FunctionPassContext) -> Bool {
  var collectedUses = OwnedToGuaranteedUses(context)
  defer { collectedUses.deinitialize() }
  if !collectedUses.collectUses(of: copy) {
    return false
  }

  // A copy which is never destroyed - e.g. because its lifetime ends in a dead-end block - doesn't
  // have a liverange we could create `end_borrow`s for.
  if collectedUses.ends.isEmpty {
    return false
  }

  if mayReleaseReferencedObject(of: copy, endingAt: collectedUses.ends, context) {
    return false
  }

  let builder = Builder(before: copy, context)
  let borrow = builder.createUncheckedOwnershipConversion(operand: copy.fromValue,
                                                          resultOwnership: .guaranteed)
  copy.replace(with: borrow, context)

  var liverange = InstructionRange(begin: borrow, ends: collectedUses.ends, context)
  defer { liverange.deinitialize() }

  createEndBorrows(for: borrow, atEndOf: liverange, collectedUses: collectedUses)
  collectedUses.changeOwnedToGuaranteed(outerScope: borrow, within: liverange)
  return true
}

/// Returns true if any instruction between the `copy` and `ends` can drop a reference to the
/// object which `copy` refers to.
private func mayReleaseReferencedObject(of copy: CopyValueInst,
                                        endingAt ends: IterableInstructionSet,
                                        _ context: FunctionPassContext) -> Bool
{
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for endInst in ends {
    worklist.pushPredecessors(of: endInst, ignoring: copy)
  }

  let calleeAnalysis = context.calleeAnalysis

  while let inst = worklist.pop() {
    if inst.mayReleaseAnyObject(calleeAnalysis),
       // Destroys of the copy's own (forward-extended) lifetime are replaced by `end_borrow`s -
       // or erased - by this optimization. This includes interior destroys, e.g. of a decomposed
       // aggregate where only the last field's destroy ends the liverange.
       !ends.contains(inst),
       !inst.isBalancedDestroy(of: copy)
    {
      return true
    }
    worklist.pushPredecessors(of: inst, ignoring: copy)
  }
  return false
}

private extension Instruction {
  /// Conservatively, whether this instruction can drop a reference to any object.
  ///
  /// This is like `mayRelease`, except that an `apply` is only releasing if the computed side effects
  /// of its callees say so. `mayRelease` is true for _all_ applies, which would be far too
  /// conservative here: a call which takes the copied value as a guaranteed argument is by far the
  /// most common thing to find in such a liverange.
  func mayReleaseAnyObject(_ calleeAnalysis: CalleeAnalysis) -> Bool {
    if let apply = self as? FullApplySite {
      return calleeAnalysis.getSideEffects(ofApply: apply).ownership.destroy
    }
    if let ownershipConversion = self as? UncheckedOwnershipConversionInst,
       ownershipConversion.ownership == .guaranteed
    {
      // Converting to guaranteed just re-interprets the ownership of the operand. Unlike a
      // conversion to owned or unowned it cannot release anything.
      return false
    }
    return mayRelease
  }

  /// True if this is a `destroy_value` of a value which is copied - directly or indirectly - from
  /// `copy`.
  ///
  /// Such a destroy is balanced by its own `copy_value`, so it cannot drop the reference which
  /// `copy` is about to give up. For example:
  /// ```
  ///   %1 = copy_value %0        // the copy to remove
  ///   %2 = copy_value %1
  ///   destroy_value %2          // balanced by `%2 = copy_value %1`
  ///   ...
  ///   destroy_value %1
  /// ```
  func isBalancedDestroy(of copy: CopyValueInst) -> Bool {
    guard let destroy = self as? DestroyValueInst else {
      return false
    }
    var value = destroy.destroyedValue
    while let intermediateCopy = value as? CopyValueInst {
      if intermediateCopy == copy {
        return true
      }
      value = intermediateCopy.fromValue
    }
    return false
  }
}
