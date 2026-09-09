//===--- CopyToBorrowOptimization.swift ------------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

/// 1. replaces a `load [copy]` with a `load_borrow` if possible:
///
/// ```
///   %1 = load [copy] %0
///   // no writes to %0
///   destroy_value %1
/// ```
/// ->
/// ```
///   %1 = load_borrow %0
///   // no writes to %0
///   end_borrow %1
/// ```
///
/// 2. removes a `copy_value` where the source is a guaranteed value, if possible:
///
/// ```
///   %1 = copy_value %0   // %0 = a guaranteed value
///   // uses of %1
///   destroy_value %1     // borrow scope of %0 is still valid here
/// ```
/// ->
/// ```
///   // uses of %0
/// ```

/// The optimization can be done if:
/// * In case of a `load`: during the (forward-extended) lifetime of the loaded value the
///                       memory location is not changed.
/// * In case of a `copy_value`: the lifetime of the source operand extends - or can be extended to -
///                       the lifetime of the copied value.
/// * All (forward-extended) uses of the load or copy support guaranteed ownership. This includes stores
///     to stack locations which can be converted to `store_borrow`.
/// * The (forward-extended) lifetime of the load or copy ends with `destroy_value`(s).
///
/// As an additional related optimization, "dead" `copy_value` instructions are removed:
/// ```
///   %2 = copy_value %1
///   ...                // no deinit barriers here
///   destroy_value %2   // the only use of %2
/// ```
///
let copyToBorrowOptimization = FunctionPass(name: "copy-to-borrow-optimization") {
  (function: Function, context: FunctionPassContext) in

  if !function.hasOwnership {
    return
  }

  var changed = false

  // Replacing a `load [copy]` with a `load_borrow` turns its `destroy_value`s into `end_borrow`s,
  // which - other than a `destroy_value` - don't block the replacement of another `load [copy]` from
  // the same memory region (see `isEndOfLoadBorrowScope`). Therefore iterate until there is nothing
  // left to do. This is required to optimize all loads of e.g. a multi-field struct which is loaded
  // from an unsafe pointer, where all the loaded values are destroyed at the same place.
  var changedInIteration = true
  while changedInIteration {
    changedInIteration = false

    for inst in function.instructions {
      switch inst {
      case let load as LoadInst:
        if !context.continueWithNextSubpassRun(for: load) {
          return
        }
        if optimize(load: load, context) {
          changedInIteration = true
        }
      case let copy as CopyValueInst:
        if !context.continueWithNextSubpassRun(for: copy) {
          return
        }
        if optimize(copy: copy, context) {
          changedInIteration = true
          break
        }
        if removeDead(copy: copy, context) {
          changedInIteration = true
        }
      default:
        break
      }
    }
    changed = changed || changedInIteration
  }

  if changed {
    updateBorrowedFrom(in: function, context)
  }
}

private func optimize(load: LoadInst, _ context: FunctionPassContext) -> Bool {
  if load.loadOwnership != .copy {
    return false
  }

  var collectedUses = Uses(context)
  defer { collectedUses.deinitialize() }
  if !collectedUses.collectUses(of: load) {
    return false
  }

  // If the loaded address is a projection of a borrowed reference, the borrow scope of that reference
  // must enclose the whole lifetime of the new `load_borrow`.
  let baseBorrow = load.address.beginBorrowOfAddress

  //   %1 = load [copy] %addr      -+ liverangeOfLoadedValue  -+ loadBorrowLiverange
  //   cond_br %c, bb2, bb3         |                          | (= sub-range of liverangeOfLoadedValue)
  // bb2:                           |                          |
  //   use(%1)                      |                          |
  //   destroy_value %1             |                         -+
  //   br bb4                       |
  // bb3: // = exit block           |                         -+ memoryOverwrittenBlocks
  //   store %x to %addr            |                          |
  //   destroy_value %1            -+                          |
  //   br bb4                                                 -+
  //
  var liverangeOfLoadedValue = InstructionRange(begin: load, ends: collectedUses.ends, context)
  defer { liverangeOfLoadedValue.deinitialize() }

  var memoryOverwrittenBlocks = BasicBlockWorklist(context)
  defer { memoryOverwrittenBlocks.deinitialize() }

  collectInitialMemoryWriteBlocks(of: load,
                                  within: liverangeOfLoadedValue,
                                  into: &memoryOverwrittenBlocks,
                                  ignoreEndBorrowsOf: baseBorrow,
                                  ignoreDestroysOf: collectedUses.ends,
                                  context)

  memoryOverwrittenBlocks.propagateDown(toEndOf: liverangeOfLoadedValue)

  // Note that this must use exactly the same filters as the `loadBorrowLiverange` below. Otherwise
  // we could end up with a liverange without any ends, which would create a `load_borrow` without
  // `end_borrow`s and `undef` operands in `splitLiverange`.
  // It's not sufficient that an end is outside of `memoryOverwrittenBlocks`: it also has to be an
  // end of `liverangeOfLoadedValue`. For example, the `destroy_value` of a decomposed aggregate
  // field is not an end of the liverange if the other fields are destroyed later.
  let newEnds = collectedUses.ends.atEndOf(liverangeOfLoadedValue).outsideOf(memoryOverwrittenBlocks)
  if newEnds.isEmpty {
    return false
  }

  var loadBorrowLiverange = InstructionRange(begin: load, ends: newEnds, context)
  defer { loadBorrowLiverange.deinitialize() }

  guard canSplitLiveranges(of: load, atExitsOf: loadBorrowLiverange, context),
        collectedUses.forwardedValuesDontOverlap(exitsOf: loadBorrowLiverange)
  else {
    return false
  }

  if let baseBorrow {
    guard extendBorrowScope(of: baseBorrow.value, toOverlap: loadBorrowLiverange, context) else {
      return false
    }
  }

  load.replaceWithLoadBorrow(within: loadBorrowLiverange, collectedUses: collectedUses,
                             enclosingBorrow: baseBorrow?.value)
  return true
}

private func optimize(copy: CopyValueInst, _ context: FunctionPassContext) -> Bool {
  var collectedUses = Uses(context)
  defer { collectedUses.deinitialize() }
  if !collectedUses.collectUses(of: copy) {
    return false
  }

  var liverange = InstructionRange(begin: copy, context)
  defer { liverange.deinitialize() }
  liverange.insert(contentsOf: collectedUses.ends)

  if copy.fromValue.ownership == .owned {
    if !liverange.isFullyContainedIn(scopeOf: copy.fromValue) {
      return false
    }
  } else {
    guard extendBorrowScope(of: copy.fromValue, toOverlap: liverange, context) else {
      return false
    }
  }

  remove(copy: copy, collectedUses: collectedUses, liverange: liverange)
  return true
}

/// Removes a `copy_value` if the result is only destroyed and there are not deinit-barriers
/// between the copy and the `destroy_value`s.
private func removeDead(copy: CopyValueInst, _ context: FunctionPassContext) -> Bool {
  guard copy.uses.ignoreDebugUses.users.allSatisfy({ $0 is DestroyValueInst }) else {
    return false
  }
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for user in copy.users {
    worklist.pushPredecessors(of: user)
  }
  let calleeAnalysis = context.calleeAnalysis
  while let inst = worklist.pop() {
    if inst.isDeinitBarrier(calleeAnalysis) {
      return false
    }
    worklist.pushPredecessors(of: inst, ignoring: copy)
  }
  context.erase(instructionIncludingAllUsers: copy)
  return true
}

private struct Uses {
  let context: FunctionPassContext

  // Operand of all forwarding instructions, which - if possible - are converted from "owned" to "guaranteed"
  private(set) var forwardingUses: Stack<Operand>

  // All destroys of the load/copy_value and its forwarded values.
  // Also, first instructions of exit blocks of the load/copy_value's liverange which don't have a destroy.
  // Those are successor blocks of terminators, like `switch_enum`, which do _not_ forward the value.
  // E.g. the none-case of a switch_enum of an Optional.
  private(set) var ends: IterableInstructionSet

  init(_ context: FunctionPassContext) {
    self.context = context
    self.forwardingUses = Stack(context)
    self.ends = IterableInstructionSet(context)
  }

  mutating func collectUses(of initialValue: SingleValueInstruction) -> Bool {
    var worklist = ValueWorklist(context)
    defer { worklist.deinitialize() }

    // If the load/copy_value is immediately followed by a single `move_value`, use the moved value.
    // Note that `move_value` is _not_ a forwarding instruction.
    worklist.pushIfNotVisited(initialValue)

    while let value = worklist.pop() {
      for use in value.uses.endingLifetime {
        switch use.instruction {
        case let destroy as DestroyValueInst:
          ends.insert(destroy)
          forwardingUses.append(use)

        case let forwardingInst as ForwardingInstruction where forwardingInst.canChangeToGuaranteedOwnership:
          forwardingUses.append(use)
          findNonDestroyingLiverangeExits(of: forwardingInst)
          worklist.pushIfNotVisited(contentsOf: forwardingInst.forwardedResults.lazy.filter { $0.ownership == .owned})

        case let store as StoreInst:
          assert(use == store.sourceOperand)
          guard canConvertToStoreBorrow(store: store, ends: &ends, context) else {
            return false
          }
          forwardingUses.append(use)
        default:
          return false
        }
      }
    }
    return true
  }

  func forwardedValuesDontOverlap(exitsOf liverange: InstructionRange) -> Bool {
    for forwardingUse in forwardingUses where liverange.contains(forwardingUse.instruction) {
      switch forwardingUse.instruction {
      case let forwardingInst as ForwardingInstruction:
        for result in forwardingInst.forwardedResults where result.ownership == .owned {
          guard result.users.allSatisfy({liverange.inclusiveRangeContains($0)}) else {
            return false
          }
        }
      case let store as StoreInst:
        let allocStack = store.destination as! AllocStackInst
        guard allocStack.users.allSatisfy({liverange.contains($0)}) else {
          return false
        }
      case is DestroyValueInst:
        break
      default:
        fatalError("unknown forwarding instruction")
      }
    }
    return true
  }

  func changeOwnedToGuaranteed(outerScope: Value, within liverange: InstructionRange) {
    for forwardingUse in forwardingUses where liverange.inclusiveRangeContains(forwardingUse.instruction){
      switch forwardingUse.instruction {
      case let store as StoreInst:
        changeStoreToStoreBorrow(store: store, outerScope: outerScope)
      case let destroy as DestroyValueInst:
        context.erase(instruction: destroy)
      default:
        forwardingUse.changeOwnership(from: .owned, to: .guaranteed, context)
      }
    }
  }

  private func changeStoreToStoreBorrow(store: StoreInst, outerScope: Value) {
    let allocStack = store.destination
    let builder = Builder(before: store, context)
    let storeBorrow = builder.createStoreBorrow(source: store.source, destination: allocStack)

    for use in allocStack.uses {
      switch use.instruction {
      case storeBorrow, is DeallocStackInst:
        break
      case let destroy as DestroyAddrInst:
        if let prev = destroy.previous,
           let endBorrow = prev as? EndBorrowInst,
           endBorrow.borrow == outerScope
        {
          // If we already inserted new `end_borrow`s for an outer scope we need to make sure that the
          // `end_borrow`s for the `store_borrow` (= an inner scope) are inserted before the `end_borrow`s
          // of the outer scope.
          Builder(before: endBorrow, context).createEndBorrow(of: storeBorrow)
        } else {
          Builder(before: destroy, context).createEndBorrow(of: storeBorrow)
        }
        context.erase(instruction: destroy)
      case let debugValue as DebugValueInst:
        if debugValue.parentBlock != storeBorrow.parentBlock || !storeBorrow.strictlyDominatesInBlock(debugValue) {
          debugValue.move(before: storeBorrow.next!, context)
        }
        fallthrough
      default:
        use.set(to: storeBorrow, context)
      }
    }
    context.erase(instruction: store)

  }

  private mutating func findNonDestroyingLiverangeExits(of forwardingInst: ForwardingInstruction) {
    if let termInst = forwardingInst as? TermInst {
      // A terminator instruction can implicitly end the lifetime of its operand in a success block,
      // e.g. a `switch_enum` with a non-payload case block. Such success blocks need an `end_borrow`, though.
      for succ in termInst.successors where !succ.arguments.contains(where: {$0.ownership == .owned}) {
        ends.insert(succ.instructions.first!)
      }
    } else if !forwardingInst.forwardedResults.contains(where: { $0.ownership == .owned }) {
      // The forwarding instruction has no owned result, which means it ends the lifetime of its owned operand.
      // This can happen with an `unchecked_enum_data` which extracts a trivial payload out of a
      // non-trivial enum.
      ends.insert(forwardingInst.next!)
    }
  }

  mutating func deinitialize() {
    forwardingUses.deinitialize()
    ends.deinitialize()
  }
}

/// Checks if the `store` stores to an `alloc_stack` and that no other instructions (beside `destroy_addr`)
/// modify the stack location.
private func canConvertToStoreBorrow(store: StoreInst,
                                     ends: inout IterableInstructionSet,
                                     _ context: FunctionPassContext) -> Bool
{
  guard store.storeOwnership == .initialize,
        let allocStack = store.destination as? AllocStackInst
  else {
    return false
  }

  var walker = AllocStackUsesWalker(initialStore: store, context)
  defer { walker.deinitialize() }
  if walker.walkDownUses(ofAddress: allocStack, path: UnusedWalkingPath()) == .abortWalk {
    return false
  }

  guard isDestroyedOnAllPaths(allocStack: allocStack, destroys: walker.destroys, context) else {
    return false
  }

  ends.insert(contentsOf: walker.destroys)
  return true
}

private func isDestroyedOnAllPaths(allocStack: AllocStackInst,
                                   destroys: Stack<Instruction>,
                                   _ context: FunctionPassContext) -> Bool
{
  if destroys.isEmpty {
    return false
  }
  var liverange = BasicBlockRange(begin: allocStack.parentBlock, context)
  defer { liverange.deinitialize() }
  liverange.insert(contentsOf: destroys.lazy.map(\.parentBlock))
  return liverange.exits.isEmpty
}

private struct AllocStackUsesWalker : AddressDefUseWalker {
  let context: FunctionPassContext
  let initialStore: StoreInst
  var destroys: Stack<Instruction>

  init(initialStore: StoreInst, _ context: FunctionPassContext) {
    self.initialStore = initialStore
    self.context = context
    self.destroys = Stack(context)
  }

  mutating func deinitialize() {
    self.destroys.deinitialize()
  }

  mutating func leafUse(address: Operand, path: UnusedWalkingPath) -> WalkResult {
    switch address.instruction {
    case let load as LoadInst:
      if load.loadOwnership == .take {
        return .abortWalk
      }
      return .continueWalk
    case let store as StoreInst:
      if store != initialStore {
        return .abortWalk
      }
      return .continueWalk
    case let copy as SourceDestAddrInstruction:
      if address == copy.destinationOperand {
        return .abortWalk
      }
      if address == copy.sourceOperand && copy.isTakeOfSource {
        return .abortWalk
      }
      return .continueWalk
    case let apply as ApplySite:
      switch apply.convention(of: address) {
      case .indirectInGuaranteed:
        if let pa = apply as? PartialApplyInst, !pa.isOnStack {
          return .abortWalk
        }
        return .continueWalk
      default:
        return .abortWalk
      }
    case let destroy as DestroyAddrInst:
      if destroy.destroyedAddress == initialStore.destination {
        destroys.append(destroy)
        return .continueWalk
      }
      return .abortWalk
    case is DeallocStackInst, is DebugValueInst:
      return .continueWalk
    default:
      return .abortWalk
    }
  }
}

private func collectInitialMemoryWriteBlocks(of load: LoadInst,
                                             within liverange: InstructionRange,
                                             into memoryOverwrittenBlocks: inout BasicBlockWorklist,
                                             ignoreEndBorrowsOf baseBorrow: BeginBorrowValue?,
                                             ignoreDestroysOf ownEnds: IterableInstructionSet,
                                             _ context: FunctionPassContext
) {
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(load.next!)

  let aliasAnalysis = context.aliasAnalysis

  while let inst = worklist.pop() {
    guard liverange.contains(inst) else {
      continue
    }
    if inst.mayWrite(toAddress: load.address, aliasAnalysis),
       !inst.isEndBorrow(ofScope: baseBorrow),
       !inst.isEndOfLoadBorrowScope,
       // A `destroy_value` which ends the lifetime of the loaded value itself is replaced by an
       // `end_borrow` (or erased) by this optimization. Therefore it cannot write to the memory.
       // This happens when an aggregate is decomposed, e.g.
       //   %1 = load [copy] %0
       //   (%2, %3) = destructure_struct %1
       //   destroy_value %2   // not the final end of the liverange - but still not a write
       //   destroy_value %3
       !(inst is DestroyValueInst && ownEnds.contains(inst))
    {
      memoryOverwrittenBlocks.pushIfNotVisited(inst.parentBlock)
    } else {
      worklist.pushSuccessors(of: inst)
    }
  }
}

private func canSplitLiveranges(of load: LoadInst,
                                atExitsOf liverange: InstructionRange,
                                _ context: FunctionPassContext
) -> Bool {
  var walker = InteriorUseWalker(definingValue: load, ignoreEscape: false, visitInnerUses: true, context) {
      (operand: Operand) -> WalkResult in
    if operand.value == load {
      // We can always split the top-level "owned" liverange by inserting `copy_value`s at exit blocks.
      return .continueWalk
    } else if let beginBorrow = operand.value as? BeginBorrowInst, beginBorrow.borrowedValue == load {
      // We can split borrow scopes by inserting `begin_borrow`s (of the split "owned" liverange) at exit blocks.
      return .continueWalk
    } else {
      // Any other value which is derived from the load - e.g. a guaranteed value which is forwarded from
      // an inner borrow scope - cannot be re-created at the liverange exits. Therefore it must not be
      // used beyond the liverange.
      // Values which are _defined_ outside the liverange are fine: they are derived from operands which
      // are re-written to the new `copy_value` in the exit block.
      if !liverange.inclusiveRangeContains(operand.instruction),
         operand.value.isDefined(within: liverange)
      {
        return .abortWalk
      }
      return .continueWalk
    }
  }
  defer { walker.deinitialize() }

  return walker.visitUses() == .continueWalk
}

private func splitLiverange(of load: LoadInst,
                            replacedWith loadBorrow: LoadBorrowInst,
                            atExitsOf liverange: InstructionRange,
                            _ context: FunctionPassContext
) {
  var ssaUpdater = SSAUpdater(type: loadBorrow.type, ownership: .owned, context)
  defer { ssaUpdater.deinitialize() }
  for exitBlock in liverange.blockRange.exits {
    let builder = Builder(atBeginOf: exitBlock, context)
    let newCopy = builder.createCopyValue(operand: loadBorrow)
    ssaUpdater.addAvailableValue(newCopy, in: exitBlock)
    builder.createEndBorrow(of: loadBorrow)
  }
  for use in load.uses {
    if liverange.inclusiveRangeContains(use.instruction) {
      if let beginBorrow = use.instruction as? BeginBorrowInst {
        splitBorrowScope(of: beginBorrow, atExitsOf: liverange, ownedSsaUpdater: &ssaUpdater, context)
      }
    } else {
      use.set(to: ssaUpdater.getValue(atEndOf: use.instruction.parentBlock), context)
    }
  }
}

private func splitBorrowScope(of beginBorrow: BeginBorrowInst,
                              atExitsOf liverange: InstructionRange,
                              ownedSsaUpdater: inout SSAUpdater<FunctionPassContext>,
                              _ context: FunctionPassContext
) {
  var borrowScope = BasicBlockRange(begin: beginBorrow.parentBlock, context)
  defer { borrowScope.deinitialize() }
  borrowScope.insert(contentsOf: beginBorrow.uses.endingLifetime.map { $0.instruction.parentBlock })

  var ssaUpdater = SSAUpdater(type: beginBorrow.type, ownership: .guaranteed, context)
  defer { ssaUpdater.deinitialize() }

  for exitBlock in liverange.blockRange.exits where borrowScope.inclusiveRangeContains(exitBlock) {
    let previouslyInsertedNewCopy = ownedSsaUpdater.getValue(atEndOf: exitBlock) as! CopyValueInst
    let builder = Builder(after: previouslyInsertedNewCopy, context)
    let newBeginBorrow = builder.createBeginBorrow(of: previouslyInsertedNewCopy)
    ssaUpdater.addAvailableValue(newBeginBorrow, in: exitBlock)
  }
  for borrowUse in beginBorrow.uses {
    if !liverange.inclusiveRangeContains(borrowUse.instruction) {
      borrowUse.set(to: ssaUpdater.getValue(atEndOf: borrowUse.instruction.parentBlock), context)
    }
  }
  for exitBlock in liverange.blockRange.exits where borrowScope.inclusiveRangeContains(exitBlock) {
    Builder(atBeginOf: exitBlock, context).createEndBorrow(of: beginBorrow)
  }
  updateBorrowedFrom(for: ssaUpdater.insertedPhis, context)
}

private extension LoadInst {
  func replaceWithLoadBorrow(within liverange: InstructionRange,
                             collectedUses: Uses,
                             enclosingBorrow: Value? = nil
  ) {
    let context = collectedUses.context
    let builder = Builder(before: self, context)
    let loadBorrow = builder.createLoadBorrow(fromAddress: address)

    createEndBorrows(for: loadBorrow, atEndOf: liverange, collectedUses: collectedUses,
                     enclosingBorrow: enclosingBorrow)

    splitLiverange(of: self, replacedWith: loadBorrow, atExitsOf: liverange, context)

    uses.replaceAll(with: loadBorrow, context)
    context.erase(instruction: self)

    collectedUses.changeOwnedToGuaranteed(outerScope: loadBorrow, within: liverange)
  }
}

private func remove(copy: CopyValueInst, collectedUses: Uses, liverange: InstructionRange) {
  let context = collectedUses.context
  let fromValue = copy.fromValue

  switch fromValue.ownership {
  case .owned:
    let builder = Builder(before: copy, context)
    let beginBorrow = builder.createBeginBorrow(of: fromValue)
    copy.replace(with: beginBorrow, context)
    createEndBorrows(for: beginBorrow, atEndOf: liverange, collectedUses: collectedUses)
    collectedUses.changeOwnedToGuaranteed(outerScope: beginBorrow, within: liverange)
  case .guaranteed:
    copy.replace(with: fromValue, context)
    collectedUses.changeOwnedToGuaranteed(outerScope: fromValue.lookThroughForwardingInstructions, within: liverange)
  case .none, .unowned:
    fatalError("unexpected ownership of copy source")
  }
}

private func createEndBorrows(for beginBorrow: Value,
                              atEndOf liverange: InstructionRange,
                              collectedUses: Uses,
                              enclosingBorrow: Value? = nil
) {
  let context = collectedUses.context

  // There can be multiple destroys in a row in case of decomposing an aggregate, e.g.
  //   %1 = load [copy] %0
  //     ...
  //   (%2, %3) = destructure_struct %1
  //   destroy_value %2
  //   destroy_value %3  // The final destroy. Here we need to create the `end_borrow`(s)
  //
  for endInst in collectedUses.ends.atEndOf(liverange) {
    var insertionPoint = endInst
    if let enclosingBorrow {
      // If we already inserted `end_borrow`s for an enclosing scope - e.g. when the borrow scope of the
      // load's base was extended - we need to make sure that the `end_borrow`s for this (inner) scope are
      // inserted before the `end_borrow`s of the enclosing scope.
      while let prev = insertionPoint.previous as? EndBorrowInst, prev.borrow == enclosingBorrow {
        insertionPoint = prev
      }
    }
    let builder = Builder(before: insertionPoint, context)
    builder.createEndBorrow(of: beginBorrow)
  }
}

private extension Instruction {
  /// True if this is an `end_borrow` which ends the borrow scope of `beginBorrow`.
  func isEndBorrow(ofScope beginBorrow: BeginBorrowValue?) -> Bool {
    guard let beginBorrow, let endBorrow = self as? EndBorrowInst else {
      return false
    }
    return endBorrow.borrow == beginBorrow.value
  }

  /// True if this is an `end_borrow` of a `load_borrow`.
  ///
  /// Alias analysis conservatively reports a "write" for such an `end_borrow` to prevent other
  /// optimizations from moving stores into the borrow scope. But the `end_borrow` itself neither
  /// accesses memory nor releases anything. Therefore it cannot invalidate another - potentially
  /// overlapping - `load_borrow` scope and we can ignore it when looking for memory writes.
  ///
  /// This is important to let this optimization cascade: after one `load [copy]` is replaced by a
  /// `load_borrow`, its `destroy_value`s become `end_borrow`s, which must not block replacing
  /// another `load [copy]` from the same memory region.
  ///
  /// This is deliberately restricted to `load_borrow`:
  /// * An `end_borrow` of a `store_borrow` ends the initialization of the destination address.
  ///   Ignoring it lets the new `load_borrow` escape the `store_borrow` scope and creates ill
  ///   formed SIL (see `store_borrow_aliased` in copy-to-borrow-optimization.sil).
  /// * An `end_borrow` of a `begin_borrow` models a real effect: it can let the borrowed value be
  ///   deallocated, which invalidates interior pointers into it. If the load's address is such an
  ///   interior pointer, `baseBorrow` handles it (and `extendBorrowScope` widens the scope);
  ///   otherwise alias analysis is only conservative because the address base is unidentified,
  ///   and we don't want to second-guess that here.
  var isEndOfLoadBorrowScope: Bool {
    if let endBorrow = self as? EndBorrowInst {
      return endBorrow.borrow is LoadBorrowInst
    }
    return false
  }
}

private extension ForwardingInstruction {
  var canChangeToGuaranteedOwnership: Bool {
    if !preservesReferenceCounts {
      return false
    }
    if !canForwardGuaranteedValues {
      return false
    }
    // For simplicity only support a single owned operand. Otherwise we would have to check if the other
    // owned operands stem from `load_borrow`s, too, which we can convert, etc.
    let numOwnedOperands = operands.lazy.filter({ $0.value.ownership == .owned }).count
    if numOwnedOperands > 1 {
      return false
    }
    return true
  }
}

private extension BasicBlockWorklist {
  mutating func propagateDown(toEndOf liverange: InstructionRange) {
    while let block = pop() {
      pushIfNotVisited(contentsOf: block.successors.lazy.filter { succ in
        liverange.inclusiveRangeContains(succ.instructions.first!)
      })
    }
  }
}

private extension Value {
  var beginBorrowOfAddress: BeginBorrowValue? {
    if let baseReference = accessBase.reference {
      return BeginBorrowValue(baseReference.lookThroughForwardingInstructions)
    }
    return nil
  }

  /// True if this value is defined inside `range`. Note that for a terminator result - e.g. a `switch_enum`
  /// payload argument - the defining instruction is the terminator in the predecessor block.
  func isDefined(within range: InstructionRange) -> Bool {
    guard let def = definingInstructionOrTerminator else {
      // A phi argument: be conservative.
      return true
    }
    return range.inclusiveRangeContains(def)
  }
}

private extension Sequence where Element == Instruction {
  func outsideOf(_ blocks: BasicBlockWorklist) -> LazyFilterSequence<Self> {
    self.lazy.filter { !blocks.hasBeenPushed($0.parentBlock) }
  }

  func atEndOf(_ liverange: InstructionRange) -> LazyFilterSequence<Self> {
    self.lazy.filter { liverange.isEnd($0) }
  }
}
