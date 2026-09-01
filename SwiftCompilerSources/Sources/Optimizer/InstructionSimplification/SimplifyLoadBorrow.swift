//===--- SimplifyLoadBorrow.swift -----------------------------------------===//
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

extension LoadBorrowInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if uses.ignoreDebugUses.hasOnlyUsers(ofType: EndBorrowInst.self) {
      context.erase(instructionIncludingAllUsers: self)
      return
    }

    if tryCombineWithCopy(context) {
      return
    }

    if tryRemoveAddrCast(context) {
      return
    }

    if replaceLoadOfGlobalLet(context) {
      return
    }

    if replaceWithValueHint(context) {
      return
    }

    tryForwardStoreBorrow(context)
  }

  /// If RLE cannot eliminate a redundant `load_borrow` it records the available value in the value
  /// hint instead. If that value does not need to be kept alive by a borrow scope, the
  /// `load_borrow` can be replaced by it after all:
  /// ```
  ///   %1 = load_borrow %0, value_hint %v      // %v has "none" ownership
  ///   // ... uses of %1
  ///   end_borrow %1
  /// ```
  /// ->
  /// ```
  ///   // ... uses of %v
  /// ```
  ///
  /// The hinted value can lose its ownership long after RLE set the hint. For example when a
  /// `partial_apply` is turned into a `thin_to_thick_function` by the ConstantCapturePropagation.
  private func replaceWithValueHint(_ context: SimplifyContext) -> Bool {
    guard let valueHint, valueHint.hasGenuineNoneOwnership(context),
          // A reborrow cannot be replaced by a value which is not a borrow introducer.
          uses.endingLifetime.hasOnlyUsers(ofType: EndBorrowInst.self)
    else {
      return false
    }
    context.erase(instructions: uses.endingLifetime.users(ofType: EndBorrowInst.self))
    replace(with: valueHint, context)
    return true
  }

  /// The load of a global let variable is replaced by its static initializer value.
  ///
  /// The cloned initializer value is not a borrow, so it is borrowed to keep the guaranteed uses
  /// unchanged and destroyed where the original borrow scope ended:
  /// ```
  ///   %1 = global_addr @g
  ///   %2 = load_borrow %1
  ///   // ... uses of %2
  ///   end_borrow %2
  /// ```
  /// ->
  /// ```
  ///   %1 = <cloned initializer of g>
  ///   %2 = begin_borrow %1
  ///   // ... uses of %2
  ///   end_borrow %2
  ///   destroy_value %1
  /// ```
  ///
  /// Usually the initializer of a global `let` is statically allocatable and therefore the cloned
  /// value has `.none` ownership. In that case SimplifyDestroyValue removes the `destroy_value`
  /// again.
  private func replaceLoadOfGlobalLet(_ context: SimplifyContext) -> Bool {
    guard let globalInitVal = getGlobalInitValue(address: address, context),
          globalInitVal.canBeCopied(into: parentFunction, context),
          // Only handle the simple case where the borrow scope is delimited by end_borrows.
          // Otherwise it's not clear where to destroy the cloned value.
          uses.endingLifetime.hasOnlyUsers(ofType: EndBorrowInst.self)
    else {
      return false
    }

    var cloner = Cloner(cloneBefore: self, context)
    defer { cloner.deinitialize() }
    let initVal = cloner.cloneRecursively(globalInitValue: globalInitVal)

    let builder = Builder(before: self, context)
    let beginBorrow = builder.createBeginBorrow(of: initVal)
    for endBorrow in uses.endingLifetime.users(ofType: EndBorrowInst.self) {
      Builder(after: endBorrow, context).createDestroyValue(operand: initVal)
    }
    replace(with: beginBorrow, context)
    return true
  }

  /// If the load_borrow is followed by a copy_value, combine both into a `load [copy]`:
  /// ```
  ///   %1 = load_borrow %0
  ///   %2 = some_forwarding_instruction %1 // zero or more forwarding instructions
  ///   %3 = copy_value %2
  ///   end_borrow %1
  /// ```
  /// ->
  /// ```
  ///   %1 = load [copy] %0
  ///   %3 = some_forwarding_instruction %1 // zero or more forwarding instructions
  /// ```
  ///
  private func tryCombineWithCopy(_ context: SimplifyContext) -> Bool {
    let forwardedValue = lookThroughOwnedConvertibaleForwardingChain()
    guard let singleUser = forwardedValue.uses.ignore(usersOfType: EndBorrowInst.self).singleUse?.instruction,
          let copy = singleUser as? CopyValueInst,
          copy.parentBlock == self.parentBlock else {
      return false
    }
    let builder = Builder(before: self, context)
    let loadCopy = builder.createLoad(fromAddress: address, ownership: .copy)
    let forwardedOwnedValue = replaceGuaranteed(value: self, withOwnedValue: loadCopy, context)
    copy.replace(with: forwardedOwnedValue, context)
    context.erase(instructionIncludingAllUsers: self)
    return true
  }

  /// Replaces address casts of heap objects
  /// ```
  ///   %1 = unchecked_addr_cast %0 : $*SomeClass to $*OtherClass
  ///   %2 = load_borrow %1
  ///   // ... uses of %2
  ///   end_borrow %2
  /// ```
  /// with ref-casts of the loaded value
  /// ```
  ///   %1 = load_borrow %0
  ///   %2 = unchecked_ref_cast %1 : $SomeClass to $OtherClass
  ///   // ... uses of %2
  ///   end_borrow %2
  /// ```
  /// Address casts are bad because they prevent alias analysis and AccessPath computation.
  /// It's always better to use the corresponding value casts instead.
  ///
  private func tryRemoveAddrCast(_ context: SimplifyContext) -> Bool {
    guard let addrCast = address.isAddressCastOfHeapObjects else {
      return false
    }
    let builder = Builder(before: self, context)
    let newLoad = builder.createLoadBorrow(fromAddress: addrCast.fromAddress)
    let cast = builder.createUncheckedRefCast(from: newLoad, to: addrCast.type.objectType)
    replace(with: newLoad, context)
    newLoad.uses.filter{ !$0.endsLifetime }.ignore(user: cast).replaceAll(with: cast, context)
    return true
  }

  /// Replaces a `load_borrow` of a `store_borrow` with a `begin_borrow`:
  /// ```
  ///   %1 = alloc_stack $T
  ///   %2 = store_borrow %0 to %1
  ///   ...
  ///   %3 = load_borrow %2
  ///   // ... uses of %3
  ///   end_borrow %3
  /// ```
  /// ->
  /// ```
  ///   %1 = alloc_stack $T
  ///   %2 = store_borrow %0 to %1
  ///   ...
  ///   %3 = begin_borrow %0
  ///   // ... uses of %3
  ///   end_borrow %3
  /// ```
  private func tryForwardStoreBorrow(_ context: SimplifyContext) {
    let accessPath = address.accessPath
    guard case .storeBorrow(let storeBorrow) = accessPath.base,
          accessPath.projectionPath.isMaterializable,
          uses.endingLifetime.hasOnlyUsers(ofType: EndBorrowInst.self)
    else {
      return
    }

    let builder = Builder(before: self, context)
    let beginBorrow = builder.createBeginBorrow(of: storeBorrow.source)
    uses.endingLifetime.replaceAll(with: beginBorrow, context)
    let v = beginBorrow.createProjection(path: accessPath.projectionPath, builder: builder)
    replace(with: v, context)
  }
}

private extension Value {
  /// True if the value has "none" ownership and does not derive it from an `unchecked_ownership`,
  /// which only fakes "none" ownership for a value which actually needs a lifetime.
  func hasGenuineNoneOwnership(_ context: SimplifyContext) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushIfNotVisited(self)
    while let value = worklist.pop() {

      if value.ownership != .none {
        // This also covers an `unchecked_ownership` of a value which still needs a lifetime,
        // because such an operand has ownership.
        return false
      }
      if let forwarding = value as? ForwardingInstruction {
        worklist.pushIfNotVisited(contentsOf: forwarding.forwardedOperands.values)
      } else if let phi = Phi(value) {
        worklist.pushIfNotVisited(contentsOf: phi.incomingValues)
      } else if let termResult = TerminatorResult(value) {
        // A terminator, e.g. a `switch_enum`, forwards its ownership to the block arguments of its
        // successors. Therefore the argument's "none" ownership can be faked by the terminator's
        // operand.
        guard let forwarding = termResult.terminator as? ForwardingInstruction else {
          return false
        }
        worklist.pushIfNotVisited(contentsOf: forwarding.forwardedOperands.values)
      }
    }
    return true
  }
}
