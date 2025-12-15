//===--- RedundantLoadElimination.swift ------------------------------------==//
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

/// Replaces redundant `load` or `copy_addr` instructions with already available values.
///
/// A load is redundant if the loaded value is already available at that point.
/// This can be via a preceding store to the same address:
///
///     store %1 to %addr
///     ...               // no writes to %addr
///     %2 = load %addr
/// ->
///     store %1 to %addr
///     ...               // no writes to %addr
///     // replace uses of %2 with the available value %1
///
/// or a preceding load from the same address:
///
///     %1 = load %addr
///     ...               // no writes to %addr
///     %2 = load %addr
/// ->
///     %1 = load %addr
///     ...               // no writes to %addr
///     // replace uses of %2 with the available value %1
///
/// In case of a partial redundant load, the load is split so that some of the new
/// individual loads can be eliminated in the next round of the optimization:
///
///     %fa1 = struct_element_addr %addr, #field1
///     store %1 to %fa1
///     ...               // no writes to %fa1
///     %2 = load %addr   // partially redundant
/// ->
///     %fa1 = struct_extract %addr, #field1
///     store %1 to %fa1
///     ...               // no writes to %fa1
///     %fa1 = struct_element_addr %addr, #field1
///     %f1 = load %fa1                              // this load is redundant now
///     %fa2 = struct_element_addr %addr, #field2
///     %f2 = load %fa2
///     %2 = struct (%f1, %f2)
///
/// This works in a similar fashion for `copy_addr`. If the source value of the `copy_addr` is
/// already available, the `copy_addr` is replaced by a `store` of the available value.
///
/// The algorithm is a data flow analysis which starts at the original load and searches
/// for preceding stores or loads by following the control flow in backward direction.
/// The preceding stores and loads provide the "available values" with which the original
/// load can be replaced.
///
/// If the function is in OSSA, redundant loads are replaced in a way that no additional
/// copies of the loaded value are introduced. If this is not possible, the redundant load
/// is not replaced.
///
let redundantLoadElimination = FunctionPass(name: "redundant-load-elimination") {
    (function: Function, context: FunctionPassContext) in
  _ = eliminateRedundantLoads(in: function, variant: .regular, context)
}

// Early RLE does not touch loads from Arrays. This is important because later array optimizations,
// like ABCOpt, get confused if an array load in a loop is converted to a pattern with a phi argument.
let earlyRedundantLoadElimination = FunctionPass(name: "early-redundant-load-elimination") {
    (function: Function, context: FunctionPassContext) in
  _ = eliminateRedundantLoads(in: function, variant: .early, context)
}

let mandatoryRedundantLoadElimination = FunctionPass(name: "mandatory-redundant-load-elimination") {
    (function: Function, context: FunctionPassContext) in
  _ = eliminateRedundantLoads(in: function, variant: .mandatory, context)
}

enum RedundantLoadEliminationVariant {
  case mandatory, mandatoryInGlobalInit, early, regular
}

func eliminateRedundantLoads(in function: Function,
                             variant: RedundantLoadEliminationVariant,
                             _ context: FunctionPassContext) -> Bool
{
  // FIXME: this skip is a hack for ManualOwnership prototyping, to workaround rdar://161359163
  if function.performanceConstraints == .manualOwnership && variant == .mandatory {
    return false
  }

  // Avoid quadratic complexity by limiting the number of visited instructions.
  // This limit is sufficient for most "real-world" functions, by far.
  var complexityBudget = 50_000
  var changed = false

  for block in function.blocks.reversed() {

    // We cannot use for-in iteration here because if the load is split, the new
    // individual loads are inserted right before and they would be ignored by a for-in iteration.
    var inst = block.instructions.reversed().first
    while let i = inst {
      defer { inst = i.previous }

      if let load = inst as? LoadingInstruction {
        if !context.continueWithNextSubpassRun(for: load) {
          return changed
        }
        if complexityBudget < 20 {
          complexityBudget = 20
        }
        if !load.isEligibleForElimination(in: variant, context) {
          continue;
        }
        changed = tryEliminate(load: load, complexityBudget: &complexityBudget, context) || changed
      }
    }
  }
  return changed
}

private enum LoadKind {
  case unqualified, trivial, take, copy, borrow

  init(loadOwnership: LoadInst.LoadOwnership) {
    switch loadOwnership {
      case .unqualified: self = .unqualified
      case .trivial:     self = .trivial
      case .take:        self = .take
      case .copy:        self = .copy
    }
  }
}

/// Either a `load` or a `copy_addr` (which is equivalent to a load+store).
private protocol LoadingInstruction: Instruction {
  var address: Value { get }
  var type: Type { get }
  var ownership: Ownership { get }
  var kind: LoadKind { get }
  var canLoadValue: Bool { get }
  func trySplit(_ context: FunctionPassContext) -> Bool
  func replace(withAvailableValue value: Value, _ context: FunctionPassContext)
}

extension LoadInst : LoadingInstruction {
  // We know that the type is loadable because - well - this is a load.
  var canLoadValue: Bool { true }

  fileprivate var kind: LoadKind { LoadKind(loadOwnership: loadOwnership) }

  func replace(withAvailableValue value: Value, _ context: FunctionPassContext) {
    replace(with: value, context)
  }
}

extension CopyAddrInst : LoadingInstruction {
  var address: Value { source }
  var type: Type { address.type.objectType }
  var typeIsLoadable: Bool { type.isLoadable(in: parentFunction) }

  var ownership: Ownership {
    if !parentFunction.hasOwnership || type.isTrivial(in: parentFunction) {
      return .none
    }
    // Regardless of if the copy is taking or copying, the loaded value is an owned value.
    return .owned
  }

  fileprivate var kind: LoadKind { LoadKind(loadOwnership: loadOwnership) }

  var canLoadValue: Bool {
    if !source.type.isLoadable(in: parentFunction) {
      // Although the original load's type is loadable (obviously), it can be projected-out
      // from the copy_addr's type which might be not loadable.
      return false
    }
    if !parentFunction.hasOwnership {
      if !isTakeOfSource || !isInitializationOfDestination {
        // For simplicity, bail if we would have to insert compensating retains and releases.
        return false
      }
    }
    return true
  }

  func replace(withAvailableValue value: Value, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    builder.createStore(source: value, destination: destination, ownership: storeOwnership)
    context.erase(instruction: self)
  }
}

extension DestroyAddrInst : LoadingInstruction {
  var address: Value { destroyedAddress }
  var type: Type { address.type.objectType }
  var typeIsLoadable: Bool { type.isLoadable(in: parentFunction) }
  fileprivate var kind: LoadKind { .take }

  var ownership: Ownership {
    if !parentFunction.hasOwnership {
      return .none
    }
    // Regardless of if the copy is taking or copying, the loaded value is an owned value.
    return .owned
  }

  var canLoadValue: Bool {
    destroyedAddress.type.isLoadable(in: parentFunction) &&
    !type.isTrivial(in: parentFunction)
  }

  func replace(withAvailableValue value: Value, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    builder.createDestroyValue(operand: value)
    context.erase(instruction: self)
  }

  func trySplit(_ context: FunctionPassContext) -> Bool { false }
}

extension LoadBorrowInst : LoadingInstruction {
  // We know that the type is loadable because - well - this is a load.
  var canLoadValue: Bool {
    endInstructions.allSatisfy { $0 is EndBorrowInst }
  }

  fileprivate var kind: LoadKind { .borrow }

  func replace(withAvailableValue value: Value, _ context: FunctionPassContext) {
    replace(with: value, context)
  }

  func trySplit(_ context: FunctionPassContext) -> Bool { false }
}


private func tryEliminate(load: LoadingInstruction, complexityBudget: inout Int, _ context: FunctionPassContext) -> Bool {
  switch load.isRedundant(complexityBudget: &complexityBudget, context) {
  case .notRedundant:
    return false
  case .redundant(let availableValues, let liverangeExits):
    replace(load: load, with: availableValues, liverangeExits: liverangeExits, context)
    return true
  case .maybePartiallyRedundant(let subPath):
    // Check if the a partial load would really be redundant to avoid unnecessary splitting.
    switch load.isRedundant(at: subPath, complexityBudget: &complexityBudget, context) {
      case .notRedundant, .maybePartiallyRedundant:
        return false
      case .redundant:
        // The new individual loads are inserted right before the current load and
        // will be optimized in the following loop iterations.
        return load.trySplit(context)
    }
  }
}

private extension LoadingInstruction {

  func isEligibleForElimination(in variant: RedundantLoadEliminationVariant, _ context: FunctionPassContext) -> Bool {
    if !canLoadValue {
      return false
    }
    switch variant {
    case .mandatory, .mandatoryInGlobalInit:
      if kind == .take {
        // load [take] would require to shrinkMemoryLifetime. But we don't want to do this in the mandatory
        // pipeline to not shrink or remove an alloc_stack which is relevant for debug info.
        return false
      }
      switch address.accessBase {
      case .box, .stack, .global:
        break
      default:
        return false
      }
    case .early:
      // See the comment of `earlyRedundantLoadElimination`.
      if let nominal = self.type.nominal, nominal == context.swiftArrayDecl {
        return false
      }
    case .regular:
      break
    }
    // Check if the type can be expanded without a significant increase to code size.
    // We block redundant load elimination because it might increase register pressure for large values.
    // Furthermore, this pass also splits values into its projections (e.g shrinkMemoryLifetimeAndSplit).
    // But: it is required to remove loads, even of large structs, in global init functions to ensure
    // that globals (containing large structs) can be statically initialized.
    if variant != .mandatoryInGlobalInit, !self.type.shouldExpand(context) {
       return false
    }
    return true
  }

  func isRedundant(complexityBudget: inout Int, _ context: FunctionPassContext) -> DataflowResult {
    return isRedundant(at: address.constantAccessPath, complexityBudget: &complexityBudget, context)
  }

  func isRedundant(at accessPath: AccessPath, complexityBudget: inout Int, _ context: FunctionPassContext) -> DataflowResult {

    if self.previous == nil, self.parentBlock.predecessors.isEmpty {
      return .notRedundant
    }

    var blockEndsInLiverange = Stack<BasicBlock>(context)
    defer { blockEndsInLiverange.deinitialize() }
    var inLiverange = InstructionSet(context)
    defer { inLiverange.deinitialize() }

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }

    worklist.pushPredecessors(of: self)
    inLiverange.insert(self)

    var potentiallyRedundantSubpath: AccessPath? = nil
    var availableValues = [AvailableValue]()
    let storageDefBlock = accessPath.base.reference?.referenceRoot.parentBlock

    while let inst = worklist.pop() {

      complexityBudget -= 1
      if complexityBudget <= 0 {
        return .notRedundant
      }

      switch visit(instruction: inst,
                   load: self,
                   accessPath: accessPath,
                   potentiallyRedundantSubpath: &potentiallyRedundantSubpath,
                   context)
      {
      case .transparent:
        inLiverange.insert(inst)
        if inst.previous == nil {
          // We reached the function entry without finding an available value.
          if inst.parentBlock.predecessors.isEmpty ||
            // Abort if we find the storage definition of the access in case of a loop, e.g.
            //
            //   bb1:
            //     %storage_root = apply
            //     %2 = ref_element_addr %storage_root
            //     %3 = load %2
            //     cond_br %c, bb1, bb2
            //
            // The storage root is different in each loop iteration. Therefore the load in a
            // successive loop iteration does not load from the same address as in the previous iteration.
            inst.parentBlock == storageDefBlock
          {
            return DataflowResult(notRedundantWith: potentiallyRedundantSubpath)
          }
        }
        if inst.next == nil {
          blockEndsInLiverange.append(inst.parentBlock)
        }
        worklist.pushPredecessors(of: inst)
      case .bad:
        return DataflowResult(notRedundantWith: potentiallyRedundantSubpath)
      case .available(let availableValue):
        availableValues.append(availableValue)
      }
    }

    switch self.kind {
    case .unqualified, .trivial:
      break

    case .copy, .take:
      let deadEndBlocks = context.deadEndBlocks

      // The liverange of the value has an "exit", i.e. a path which doesn't lead to the load,
      // it means that we would have to insert a destroy on that exit to satisfy ownership rules.
      // But an inserted destroy also means that we would need to insert copies of the value which
      // were not there originally. For example:
      //
      //     store %1 to [init] %addr
      //     cond_br bb1, bb2
      //   bb1:
      //     %2 = load [take] %addr
      //   bb2:                      // liverange exit
      //
      // TODO: we could extend OSSA to transfer ownership to support liverange exits without copying. E.g.:
      //
      //     %b = store_and_borrow %1 to [init] %addr   // %b is borrowed from %addr
      //     cond_br bb1, bb2
      //   bb1:
      //     %o = borrowed_to_owned %b take_ownership_from %addr
      //     // replace %2 with %o
      //   bb2:
      //     end_borrow %b
      //
      for block in blockEndsInLiverange {
        if block.successors.contains(where: { succ in
            !inLiverange.contains(succ.instructions.first!) && !deadEndBlocks.isDeadEnd(succ)
          })
        {
          return DataflowResult(notRedundantWith: potentiallyRedundantSubpath)
        }
      }

      // Handle a corner case: if the load is in an infinite loop, the liverange doesn't have an exit,
      // but we still would need to insert a copy. For example:
      //
      //     store %1 to [init] %addr
      //     br bb1
      //   bb1:
      //     %2 = load [copy] %addr   // would need to insert a copy here
      //    br bb1                    // no exit from the liverange
      //
      // For simplicity, we don't handle this in OSSA.
      if deadEndBlocks.isDeadEnd(parentBlock) {
        return DataflowResult(notRedundantWith: potentiallyRedundantSubpath)
      }
    case .borrow:
      var liverangeExits = [Instruction]()
      for block in blockEndsInLiverange {
        for succ in block.successors {
          let succInst = succ.instructions.first!
          if !inLiverange.contains(succInst) && !context.deadEndBlocks.isDeadEnd(succ) {
            liverangeExits.append(succInst)
          }
        }
      }
      return .redundant(availableValues, liverangeExits: liverangeExits)
    }

    return .redundant(availableValues, liverangeExits: [])
  }
}

private enum InstructionKind {
  case transparent
  case bad
  case available(AvailableValue)
}

private func visit(instruction: Instruction,
                   load: LoadingInstruction,
                   accessPath: AccessPath,
                   potentiallyRedundantSubpath: inout AccessPath?,
                   _ context: FunctionPassContext) -> InstructionKind
{
  if instruction == load {
    // We need to stop the data flow analysis when we visit the original load again.
    // This happens if the load is in a loop.
    return .transparent
  }

  switch instruction {
  case let endBorrow as EndBorrowInst:
    if let loadBorrow = load as? LoadBorrowInst, endBorrow.borrow == loadBorrow {
      return .bad
    }
    fallthrough
  case is FixLifetimeInst, is BeginAccessInst, is EndAccessInst:
    // Those scope-ending instructions are only irrelevant if the preceding load is not changed.
    // If it is changed from `load [copy]` -> `load [take]` the memory effects of those scope-ending
    // instructions prevent that the `load [take]` will illegally mutate memory which is protected
    // from mutation by the scope.
    if load.kind != .take {
      return .transparent
    }
    
  case let precedingLoad as LoadInst:
    let precedingLoadPath = precedingLoad.address.constantAccessPath
    if let projection = precedingLoadPath.getMaterializableProjection(to: accessPath) {
      if load.kind == .borrow, !projection.isEmpty {
        return .bad
      }
      return .available(.viaLoad(precedingLoad))
    }
    if accessPath.getMaterializableProjection(to: precedingLoadPath) != nil,
       potentiallyRedundantSubpath == nil
    {
      potentiallyRedundantSubpath = precedingLoadPath
    }
    if load.kind != .take {
      return .transparent
    }

  case let precedingStore as StoreInst:
    if precedingStore.source is Undef {
      return .bad
    }
    let precedingStorePath = precedingStore.destination.constantAccessPath
    if let projection = precedingStorePath.getMaterializableProjection(to: accessPath) {
      if load.kind == .borrow, !projection.isEmpty {
        return .bad
      }
      return .available(.viaStore(precedingStore))
    }
    if accessPath.getMaterializableProjection(to: precedingStorePath) != nil,
       potentiallyRedundantSubpath == nil
    {
      potentiallyRedundantSubpath = precedingStorePath
    }

  case let preceedingCopy as CopyAddrInst where preceedingCopy.canLoadValue:
    let copyPath = preceedingCopy.destination.constantAccessPath
    if copyPath.getMaterializableProjection(to: accessPath) != nil {
      return .available(.viaCopyAddr(preceedingCopy))
    }
    if accessPath.getMaterializableProjection(to: copyPath) != nil, potentiallyRedundantSubpath == nil {
      potentiallyRedundantSubpath = copyPath
    }

  default:
    break
  }
  if load.kind == .take {
    // In case of `take`, don't allow reading instructions in the liverange.
    // Otherwise we cannot shrink the memory liverange afterwards.
    if instruction.mayReadOrWrite(address: load.address, context.aliasAnalysis) {
      return .bad
    }
  } else {
    if instruction.mayWrite(toAddress: load.address, context.aliasAnalysis) {
      return .bad
    }
  }
  return .transparent
}

private func replace(load: LoadingInstruction,
                     with availableValues: [AvailableValue],
                     liverangeExits: [Instruction],
                     _ context: FunctionPassContext)
{
  var ssaUpdater = SSAUpdater(function: load.parentFunction,
                              type: load.type, ownership: load.ownership, context)

  for availableValue in availableValues.replaceCopyAddrsWithLoadsAndStores(context) {
    let block = availableValue.instruction.parentBlock
    let availableValue = provideValue(for: load, from: availableValue, context)
    ssaUpdater.addAvailableValue(availableValue, in: block)
  }

  let newValue: Value
  if availableValues.count == 1 {
    // A single available value means that this available value is located _before_ the load. E.g.:
    //
    //     store %1 to %addr   // a single available value
    //     ...
    //     %2 = load %addr     // The load
    //
    newValue = ssaUpdater.getValue(atEndOf: load.parentBlock)
  } else {
    // In case of multiple available values, if an available value is defined in the same basic block
    // as the load, this available is located _after_ the load. E.g.:
    //
    //     store %1 to %addr   // an available value
    //     br bb1
    //   bb1:
    //     %2 = load %addr     // The load
    //     store %3 to %addr   // another available value
    //     cond_br bb1, bb2
    //
    newValue = ssaUpdater.getValue(inMiddleOf: load.parentBlock)
  }

  let needUpdateBorrowedFrom = load.ownership == .guaranteed
  
  // Make sure to keep dependencies valid after replacing the load
  let updatedNewValue = copyMarkDependencies(for: newValue, from: load, context)

  if load.kind == .borrow {
    for exitInst in liverangeExits {
      let builder = Builder(before: exitInst, context)
      builder.createEndBorrow(of: ssaUpdater.getValue(inMiddleOf: exitInst.parentBlock))
    }
  }

  load.replace(withAvailableValue: updatedNewValue, context)

  if needUpdateBorrowedFrom {
    updateBorrowedFrom(for: ssaUpdater.insertedPhis, context)
  }
}

private func provideValue(
  for load: LoadingInstruction,
  from availableValue: AvailableValue,
  _ context: FunctionPassContext
) -> Value {
  let projectionPath = availableValue.address.constantAccessPath.getMaterializableProjection(to: load.address.constantAccessPath)!

  switch load.kind {
  case .unqualified:
    return availableValue.value.createProjection(path: projectionPath,
                                                 builder: availableValue.getBuilderForProjections(context))
  case .copy, .trivial:
    // Note: even if the load is trivial, the available value may be projected out of a non-trivial value.
    return availableValue.value.createProjectionAndCopy(path: projectionPath,
                                                        builder: availableValue.getBuilderForProjections(context))
  case .take:
    if projectionPath.isEmpty {
      return shrinkMemoryLifetime(to: availableValue, context)
    } else {
      return shrinkMemoryLifetimeAndSplit(to: availableValue, projectionPath: projectionPath, context)
    }
  case .borrow:
    switch availableValue {
    case .viaLoad(let load):
      assert(load.loadOwnership == .copy)
      let builder = Builder(before: load, context)
      let loadBorrow = builder.createLoadBorrow(fromAddress: load.address)
      let copy = builder.createCopyValue(operand: loadBorrow)
      load.replace(with: copy, context)
      return loadBorrow
    case .viaStore(let store):
      let builder = Builder(after: store, context)
      if store.storeOwnership == .assign {
        builder.createDestroyAddr(address: store.destination)
      }
      let storeAndBorrow = builder.createStoreAndBorrow(source: store.source, destination: store.destination)
      context.erase(instruction: store)
      return storeAndBorrow
    case .viaCopyAddr:
      fatalError("copy_addr must be lowered")
    }
  }
}

/// If the memory location depends on something, insert a dependency for the loaded value:
///
///     %2 = mark_dependence %1 on %0
///     %3 = load %2
/// ->
///     %2 = mark_dependence %1 on %0 // not needed anymore, can be removed eventually
///     %3 = load %2
///     %4 = mark_dependence %3 on %0
///     // replace %3 with %4
///
private func copyMarkDependencies(for newValue: Value, from load: LoadingInstruction, _ context: FunctionPassContext) -> Value {
  var inserter = MarkDependenceInserter(value: newValue, load: load, context: context)
  _ = inserter.walkUp(address: load.address, path: UnusedWalkingPath())
  return inserter.value
}

private struct MarkDependenceInserter : AddressUseDefWalker {
  var value: Value
  let load: LoadingInstruction
  let context: FunctionPassContext

  mutating func walkUp(address: Value, path: UnusedWalkingPath) -> WalkResult {
    if let mdi = address as? MarkDependenceInst {
      let builder = Builder(before: load, context)
      value = builder.createMarkDependence(value: value, base: mdi.base, kind: mdi.dependenceKind)
    }
    return walkUpDefault(address: address, path: path)
  }

  mutating func rootDef(address: Value, path: UnusedWalkingPath) -> WalkResult {
    return .continueWalk
  }
}

/// In case of a `load [take]` shrink lifetime of the value in memory back to the `availableValue`
/// and return the (possibly projected) available value. For example:
///
///     store %1 to [assign] %addr
///     ...
///     %2 = load [take] %addr
/// ->
///     destroy_addr %addr
///     ...
///     // replace %2 with %1
///
private func shrinkMemoryLifetime(to availableValue: AvailableValue, _ context: FunctionPassContext) -> Value {
  switch availableValue {
  case .viaLoad(let availableLoad):
    assert(availableLoad.loadOwnership == .copy)
    let builder = Builder(after: availableLoad, context)
    availableLoad.set(ownership: .take, context)
    return builder.createCopyValue(operand: availableLoad)
  case .viaStore(let availableStore):
    let builder = Builder(after: availableStore, context)
    let valueToAdd = availableStore.source
    switch availableStore.storeOwnership {
    case .assign:
      builder.createDestroyAddr(address: availableStore.destination)
      context.erase(instruction: availableStore)
    case .initialize,
         // It can be the case that e non-payload case is stored as trivial enum and the enum is loaded as [take], e.g.
         //   %1 = enum $Optional<Class>, #Optional.none
         //   store %1 to [trivial] %addr : $*Optional<Class>
         //   %2 = load [take] %addr : $*Optional<Class>
         .trivial:
      context.erase(instruction: availableStore)
    case .unqualified:
      fatalError("unqualified store in ossa function?")
    }
    return valueToAdd
  case .viaCopyAddr:
    fatalError("copy_addr must be lowered before shrinking lifetime")
  }
}

/// Like `shrinkMemoryLifetime`, but the available value must be projected.
/// In this case we cannot just shrink the lifetime and reuse the available value.
/// Therefore, we split the available load or store and load the projected available value.
/// The inserted load can be optimized with the split value in the next iteration.
///
///     store %1 to [assign] %addr
///     ...
///     %2 = struct_element_addr %addr, #field1
///     %3 = load [take] %2
/// ->
///     %f1 = struct_extract %1, #field1
///     %fa1 = struct_element_addr %addr, #field1
///     store %f1 to [assign] %fa1
///     %f2 = struct_extract %1, #field2
///     %fa2 = struct_element_addr %addr, #field2
///     store %f2 to [assign] %fa2
///     %1 = load [take] %fa1         // will be combined with `store %f1 to [assign] %fa1` in the next iteration
///     ...
///     // replace %3 with %1
///
private func shrinkMemoryLifetimeAndSplit(to availableValue: AvailableValue, projectionPath: SmallProjectionPath, _ context: FunctionPassContext) -> Value {
  switch availableValue {
  case .viaLoad(let availableLoad):
    assert(availableLoad.loadOwnership == .copy)
    let builder = Builder(after: availableLoad, context)
    let addr = availableLoad.address.createAddressProjection(path: projectionPath, builder: builder)
    let valueToAdd = builder.createLoad(fromAddress: addr, ownership: .take)
    availableLoad.trySplit(context)
    return valueToAdd
  case .viaStore(let availableStore):
    let builder = Builder(after: availableStore, context)
    let addr = availableStore.destination.createAddressProjection(path: projectionPath, builder: builder)
    let valueToAdd = builder.createLoad(fromAddress: addr, ownership: .take)
    availableStore.trySplit(context)
    return valueToAdd
  case .viaCopyAddr:
    fatalError("copy_addr must be lowered before shrinking lifetime")
  }
}

private enum DataflowResult {
  case notRedundant
  case redundant([AvailableValue], liverangeExits: [Instruction])
  case maybePartiallyRedundant(AccessPath)

  init(notRedundantWith subPath: AccessPath?) {
    if let subPath = subPath {
      self = .maybePartiallyRedundant(subPath)
    } else {
      self = .notRedundant
    }
  }
}

/// Either a `load` or `store` which is preceding the original load and provides the loaded value.
private enum AvailableValue {
  case viaLoad(LoadInst)
  case viaStore(StoreInst)
  case viaCopyAddr(CopyAddrInst)

  var value: Value {
    switch self {
    case .viaLoad(let load):   return load
    case .viaStore(let store): return store.source
    case .viaCopyAddr:         fatalError("copy_addr must be lowered")
    }
  }

  var address: Value {
    switch self {
    case .viaLoad(let load):         return load.address
    case .viaStore(let store):       return store.destination
    case .viaCopyAddr(let copyAddr): return copyAddr.destination
    }
  }

  var instruction: Instruction {
    switch self {
    case .viaLoad(let load):         return load
    case .viaStore(let store):       return store
    case .viaCopyAddr(let copyAddr): return copyAddr
    }
  }

  func getBuilderForProjections(_ context: FunctionPassContext) -> Builder {
    switch self {
    case .viaLoad(let load):   return Builder(after: load, context)
    case .viaStore(let store): return Builder(before: store, context)
    case .viaCopyAddr:         fatalError("copy_addr must be lowered")
    }
  }
}

private extension Array where Element == AvailableValue {
  func replaceCopyAddrsWithLoadsAndStores(_ context: FunctionPassContext) -> [AvailableValue] {
    return map {
      if case .viaCopyAddr(let copyAddr) = $0 {
        return .viaStore(copyAddr.replaceWithLoadAndStore(context).store)
      } else {
        return $0
      }
    }
  }
}
