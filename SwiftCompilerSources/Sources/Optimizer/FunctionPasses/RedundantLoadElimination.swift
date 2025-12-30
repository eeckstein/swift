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
  func trySplitLoad(_ context: FunctionPassContext) -> Bool
  func replace(withAvailableValue value: Value, hasSameAccessBase: Bool, _ context: FunctionPassContext)
}

extension LoadInst : LoadingInstruction {
  // We know that the type is loadable because - well - this is a load.
  var canLoadValue: Bool { true }

  fileprivate var kind: LoadKind { LoadKind(loadOwnership: loadOwnership) }

  func replace(withAvailableValue value: Value, hasSameAccessBase: Bool, _ context: FunctionPassContext) {
    if loadOwnership == .take {
      let builder = Builder(before: self, context)
      let ebat = builder.createEndBorrowAndTake(borrow: value, address: self.address)
      let v = copyMarkDependencies(for: ebat, address: address, using: Builder(after: ebat, context))
      replace(with: v, context)
    } else {
      let v = copyMarkDependencies(for: value, address: address, using: Builder(before: self, context))
      replace(with: v, context)
    }
  }

  func trySplitLoad(_ context: FunctionPassContext) -> Bool { trySplit(context) != nil }
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

  func replace(withAvailableValue value: Value, hasSameAccessBase: Bool, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    if loadOwnership == .take {
      let ebat = builder.createEndBorrowAndTake(borrow: value, address: source)
      let v = copyMarkDependencies(for: ebat, address: source, using: Builder(after: ebat, context))
      builder.createStore(source: v, destination: destination, ownership: storeOwnership)
    } else {
      let v = copyMarkDependencies(for: value, address: source, using: Builder(before: self, context))
      builder.createStore(source: v, destination: destination, ownership: storeOwnership)
    }
    context.erase(instruction: self)
  }

  func trySplitLoad(_ context: FunctionPassContext) -> Bool { trySplit(context) }
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

  func replace(withAvailableValue value: Value, hasSameAccessBase: Bool, _ context: FunctionPassContext) {
    let builder = Builder(before: self, context)
    let ebat = builder.createEndBorrowAndTake(borrow: value, address: address)
    let v = copyMarkDependencies(for: ebat, address: address, using: Builder(after: ebat, context))
    builder.createDestroyValue(operand: v)
    context.erase(instruction: self)
  }

  func trySplitLoad(_ context: FunctionPassContext) -> Bool { false }
}

extension LoadBorrowInst : LoadingInstruction {
  // We know that the type is loadable because - well - this is a load.
  var canLoadValue: Bool { true }

  fileprivate var kind: LoadKind { .borrow }

  func replace(withAvailableValue value: Value, hasSameAccessBase: Bool, _ context: FunctionPassContext) {
    let v = copyMarkDependencies(for: value, address: address, using: Builder(before: self, context))
    if !hasSameAccessBase {
      var worklist = SpecificInstructionWorklist<SingleValueInstruction>(context)
      defer { worklist.deinitialize() }
      worklist.pushIfNotVisited(self)
      while let svi = worklist.pop() {
        if !svi.uses.notEndingLifetime.isEmpty {
          let builder = Builder(after: svi, context)
          let md = builder.createMarkDependence(value: svi, base: self.address, kind: .Escaping)
          svi.uses.ignore(user: md).notEndingLifetime.replaceAll(with: md, context)
        }
        for use in svi.uses.endingLifetime {
          if let branch = use.instruction as? BranchInst {
            worklist.pushIfNotVisited(Phi(branch.getArgument(for: use))!.borrowedFrom!)
          }
        }
      }
    }
    replace(with: v, context)
  }

  func trySplitLoad(_ context: FunctionPassContext) -> Bool { false }
}


private func tryEliminate(load: LoadingInstruction, complexityBudget: inout Int, _ context: FunctionPassContext) -> Bool {
  switch load.isRedundant(complexityBudget: &complexityBudget, context) {
  case .notRedundant:
    return false
  case .redundant(let availableValues):
    replace(load: load, with: availableValues, context)
    return true
  case .maybePartiallyRedundant(let subPath):
    // Check if the a partial load would really be redundant to avoid unnecessary splitting.
    switch load.isRedundant(at: subPath, complexityBudget: &complexityBudget, context) {
      case .notRedundant, .maybePartiallyRedundant:
        return false
      case .redundant:
        // The new individual loads are inserted right before the current load and
        // will be optimized in the following loop iterations.
        return load.trySplitLoad(context)
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

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushPredecessors(of: self)

    var liverange = InstructionSet(context)
    defer { liverange.deinitialize() }
    liverange.insert(self)

    var blockEndsInLiverange = Stack<BasicBlock>(context)
    defer { blockEndsInLiverange.deinitialize() }

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
        liverange.insert(inst)
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
    case .unqualified, .trivial, .borrow, .take:
      break

    case .copy:

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
      let deadEndBlocks = context.deadEndBlocks
      for block in blockEndsInLiverange {
        for succ in block.successors {
          if !liverange.contains(succ.instructions.first!), !deadEndBlocks.isDeadEnd(succ) {
            return DataflowResult(notRedundantWith: potentiallyRedundantSubpath)
          }
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
    }

    if self is LoadBorrowInst || kind == .take {
      for availableValue in availableValues {
        switch availableValue {
        case .viaLoadBorrow, .viaStoreAndBorrow:
          worklist.pushIfNotVisited(contentsOf: availableValue.value.uses.endingLifetime.users)
        case .viaLoad, .viaStore, .viaCopyAddr:
          break
        }
      }
      if let loadBorrow = self as? LoadBorrowInst {
        worklist.pushIfNotVisited(contentsOf: loadBorrow.uses.endingLifetime.users)
      } else if kind == .take {
        worklist.pushIfNotVisited(self)
      }

      while let inst = worklist.pop() {
        liverange.insert(inst)
        worklist.pushPredecessors(of: inst)
      }

      let deadEndBlocks = context.deadEndBlocks

      for i in availableValues.indices {
        let av = availableValues[i]
        switch av {
        case .viaStoreAndBorrow(let store, _):
          if deadEndBlocks.isDeadEnd(self.parentBlock) {
            return .notRedundant
          }
          guard let bbs = getDeadEndBorrowBlocks(of: store, in: liverange) else {
            return .notRedundant
          }
          availableValues[i] = .viaStoreAndBorrow(store, deadEndBorrowBlocks: bbs)
        case .viaLoadBorrow(let load, _):
          if deadEndBlocks.isDeadEnd(self.parentBlock) {
            return .notRedundant
          }
          guard let bbs = getDeadEndBorrowBlocks(of: load, in: liverange) else {
            return .notRedundant
          }
          availableValues[i] = .viaLoadBorrow(load, deadEndBorrowBlocks: bbs)
        case .viaCopyAddr, .viaLoad, .viaStore:
          break
        }
      }

      var endBorrowsToInsert = [BasicBlock]()

      for block in blockEndsInLiverange {
        for succ in block.successors {
          if !liverange.contains(succ.instructions.first!), !deadEndBlocks.isDeadEnd(succ) {
            endBorrowsToInsert.append(succ)
          }
        }
      }
      if let loadBorrow = self as? LoadBorrowInst {
        guard let debs = getDeadEndBorrowBlocks(of: loadBorrow, in: liverange) else {
          return .notRedundant
        }
        return .redundant(AvailableValues(values: availableValues,
                                          endBorrowsToInsert: endBorrowsToInsert,
                                          deadEndBorrowBlocksOfLoad: debs))
      }
      return .redundant(AvailableValues(values: availableValues,
                                        endBorrowsToInsert: endBorrowsToInsert,
                                        deadEndBorrowBlocksOfLoad: []))
    }

    return .redundant(AvailableValues(values: availableValues, endBorrowsToInsert: [], deadEndBorrowBlocksOfLoad: []))
  }
}

private func getDeadEndBorrowBlocks(of value: Value, in liverange: InstructionSet) -> [BasicBlock]? {
  var deadEndBorrowBlocks = [BasicBlock]()
  for end in value.uses.endingLifetime.users {
    if liverange.contains(nextOf: end) {
      if end is EndBorrowInst {
        deadEndBorrowBlocks.append(end.parentBlock)
      } else {
        return nil
      }
    }
  }
  return deadEndBorrowBlocks
}

private extension InstructionSet {
  func contains(nextOf inst: Instruction) -> Bool {
    if let next = inst.next {
      return contains(next)
    } else {
      for succ in inst.parentBlock.successors {
        if contains(succ.instructions.first!) {
          return true
        }
      }
      return false
    }
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
    return .transparent

  case let precedingLoad as LoadInst:
    let precedingLoadPath = precedingLoad.address.constantAccessPath
    if let projection = precedingLoadPath.getMaterializableProjection(to: accessPath) {
      switch load.kind {
      case .borrow, .take:
        guard precedingLoad.canSplit(alongPath: projection) else {
          return .bad
        }
      default:
        break
      }
      return .available(.viaLoad(precedingLoad))
    }
    if accessPath.getMaterializableProjection(to: precedingLoadPath) != nil,
       potentiallyRedundantSubpath == nil
    {
      potentiallyRedundantSubpath = precedingLoadPath
    }
    // Note that if this is a `load [take]` it cannot alias, because it would destroy the value in memory
    // before the redundant load is loading it.
    return .transparent

  case let precedingLoad as LoadBorrowInst:
    let precedingLoadPath = precedingLoad.address.constantAccessPath
    if let projection = precedingLoadPath.getMaterializableProjection(to: accessPath) {
      switch load.kind {
      case .borrow, .take:
        guard precedingLoad.canSplit(alongPath: projection) else {
          return .bad
        }
      default:
        break
      }
      return .available(.viaLoadBorrow(precedingLoad, deadEndBorrowBlocks: []))
    }
    if accessPath.getMaterializableProjection(to: precedingLoadPath) != nil,
       potentiallyRedundantSubpath == nil
    {
      potentiallyRedundantSubpath = precedingLoadPath
    }
    return .transparent

  case let precedingStore as StoreInst:
    if precedingStore.source is Undef {
      return .bad
    }
    let precedingStorePath = precedingStore.destination.constantAccessPath
    if let projection = precedingStorePath.getMaterializableProjection(to: accessPath) {
      switch load.kind {
      case .borrow, .take:
        guard precedingStore.canSplit(alongPath: projection) else {
          return .bad
        }
      default:
        break
      }
      return .available(.viaStore(precedingStore))
    }
    if accessPath.getMaterializableProjection(to: precedingStorePath) != nil,
       potentiallyRedundantSubpath == nil
    {
      potentiallyRedundantSubpath = precedingStorePath
    }

  case let precedingStore as StoreAndBorrowInst:
    if precedingStore.source is Undef {
      return .bad
    }
    let precedingStorePath = precedingStore.destination.constantAccessPath
    if let projection = precedingStorePath.getMaterializableProjection(to: accessPath) {
      switch load.kind {
      case .borrow, .take:
        guard precedingStore.canSplit(alongPath: projection) else {
          return .bad
        }
      default:
        break
      }
      return .available(.viaStoreAndBorrow(precedingStore, deadEndBorrowBlocks: []))
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
  if instruction.mayWrite(toAddress: load.address, context.aliasAnalysis) {
    return .bad
  }
  return .transparent
}

private func replace(load: LoadingInstruction,
                     with availableValues: AvailableValues,
                     _ context: FunctionPassContext)
{
  var ssaUpdater = SSAUpdater(function: load.parentFunction,
                              type: load.type,
                              ownership: load.kind == .take ? .guaranteed : load.ownership,
                              context)

  var concreteAvailableValues = [Value]()
  var hasSameAccessBase = false
  let accessBase = load.address.accessBase
  for availableValue in availableValues.replaceCopyAddrsWithLoadsAndStores(context) {
    if availableValue.address.accessBase == accessBase {
      hasSameAccessBase = true
    }
    let block = availableValue.instruction.parentBlock
    let concreteValue = provideValue(for: load, from: availableValue, context)
    ssaUpdater.addAvailableValue(concreteValue, in: block)
    concreteAvailableValues.append(concreteValue)
  }

  for exitBlock in availableValues.endBorrowsToInsert {
    Builder(atBeginOf: exitBlock, context).createEndBorrow(of: ssaUpdater.getValue(inMiddleOf: exitBlock))
  }
  if let lb = load as? LoadBorrowInst {
    removeEndBorrows(of: lb, in: availableValues.deadEndBorrowBlocksOfLoad, context)
  }

  let newValue: Value
  if availableValues.values.count == 1 {
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

  load.replace(withAvailableValue: newValue, hasSameAccessBase: hasSameAccessBase, context)

  updateBorrowedFrom(for: ssaUpdater.insertedPhis, context)
}

private func provideValue(
  for redundantLoad: LoadingInstruction,
  from availableValue: AvailableValue,
  _ context: FunctionPassContext
) -> Value {
  let projectionPath = availableValue.address.constantAccessPath
                         .getMaterializableProjection(to: redundantLoad.address.constantAccessPath)!

  switch redundantLoad.kind {
  case .unqualified:
    return availableValue.value.createProjection(path: projectionPath,
                                                 builder: availableValue.getBuilderForProjections(context))
  case .copy, .trivial:
    // Note: even if the load is trivial, the available value may be projected out of a non-trivial value.
    return availableValue.value.createProjectionAndCopy(path: projectionPath,
                                                        builder: availableValue.getBuilderForProjections(context))
  case .borrow, .take:
    switch availableValue {
    case .viaLoad(let load):
      assert(load.loadOwnership == .copy)
      var value: Value? = nil
      load.split(alongPath: projectionPath, context) { splitLoad, isProjectedLoad in
        if isProjectedLoad {
          assert(value == nil)
          let builder = Builder(before: splitLoad, context)
          let loadBorrow = builder.createLoadBorrow(fromAddress: splitLoad.address)
          let copy = builder.createCopyValue(operand: loadBorrow)
          splitLoad.replace(with: copy, context)
          value = loadBorrow
        }
      }
      return value!
    case .viaLoadBorrow(let load, let deadEndBorrowBlocks):
      var value: Value? = nil
      load.split(alongPath: projectionPath, context) { splitLoad, isProjectedStore in
        if isProjectedStore {
          assert(value == nil)
          value = splitLoad
          removeEndBorrows(of: splitLoad, in: deadEndBorrowBlocks, context)
        }
      }
      return value!
    case .viaStore(let store):
      var value: Value? = nil
      store.split(alongPath: projectionPath, context) { splitStore, isProjectedStore in
        if isProjectedStore {
          assert(value == nil)
          let builder = Builder(after: splitStore, context)
          if splitStore.storeOwnership == .assign {
            builder.createDestroyAddr(address: splitStore.destination)
          }
          value = builder.createStoreAndBorrow(source: splitStore.source, destination: splitStore.destination)
          context.erase(instruction: splitStore)
        }
      }
      return value!
    case .viaStoreAndBorrow(let store, let deadEndBorrowBlocks):
      var value: Value? = nil
      store.split(alongPath: projectionPath, context) { splitStore, isProjectedStore in
        if isProjectedStore {
          assert(value == nil)
          let sab = splitStore as! StoreAndBorrowInst
          removeEndBorrows(of: sab, in: deadEndBorrowBlocks, context)
          value = sab
        }
      }
      return value!
    case .viaCopyAddr:
      fatalError("copy_addr must be lowered")
    }
  }
}

private func removeEndBorrows(of value: Value, in blocks: [BasicBlock], _ context: FunctionPassContext) {
  var toDeleteInBlocks = BasicBlockSet(context)
  defer { toDeleteInBlocks.deinitialize() }
  toDeleteInBlocks.insert(contentsOf: blocks)

  for endBorrow in value.uses.users(ofType: EndBorrowInst.self) {
    if toDeleteInBlocks.contains(endBorrow.parentBlock) {
      context.erase(instruction: endBorrow)
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
private func copyMarkDependencies(for newValue: Value, address: Value, using builder: Builder) -> Value {
  var inserter = MarkDependenceInserter(value: newValue, builder: builder)
  _ = inserter.walkUp(address: address, path: UnusedWalkingPath())
  return inserter.value
}

private struct MarkDependenceInserter : AddressUseDefWalker {
  var value: Value
  let builder: Builder

  mutating func walkUp(address: Value, path: UnusedWalkingPath) -> WalkResult {
    if let mdi = address as? MarkDependenceInst {
      value = builder.createMarkDependence(value: value, base: mdi.base, kind: mdi.dependenceKind)
    }
    return walkUpDefault(address: address, path: path)
  }

  mutating func rootDef(address: Value, path: UnusedWalkingPath) -> WalkResult {
    return .continueWalk
  }
}

private enum DataflowResult {
  case notRedundant
  case redundant(AvailableValues)
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
  case viaLoadBorrow(LoadBorrowInst, deadEndBorrowBlocks: [BasicBlock])
  case viaStore(StoreInst)
  case viaStoreAndBorrow(StoreAndBorrowInst, deadEndBorrowBlocks: [BasicBlock])
  case viaCopyAddr(CopyAddrInst)

  var value: Value {
    switch self {
    case .viaLoad(let load):               return load
    case .viaLoadBorrow(let load, _):      return load
    case .viaStore(let store):             return store.source
    case .viaStoreAndBorrow(let store, _): return store
    case .viaCopyAddr:                     fatalError("copy_addr must be lowered")
    }
  }

  var address: Value {
    switch self {
    case .viaLoad(let load):               return load.address
    case .viaLoadBorrow(let load, _):      return load.address
    case .viaStore(let store):             return store.destination
    case .viaStoreAndBorrow(let store, _): return store.destination
    case .viaCopyAddr(let copyAddr):       return copyAddr.destination
    }
  }

  var instruction: Instruction {
    switch self {
    case .viaLoad(let load):               return load
    case .viaLoadBorrow(let load, _):      return load
    case .viaStore(let store):             return store
    case .viaStoreAndBorrow(let store, _): return store
    case .viaCopyAddr(let copyAddr):       return copyAddr
    }
  }

  func getBuilderForProjections(_ context: FunctionPassContext) -> Builder {
    switch self {
    case .viaLoad(let load):               return Builder(after: load, context)
    case .viaLoadBorrow(let load, _):      return Builder(after: load, context)
    case .viaStore(let store):             return Builder(before: store, context)
    case .viaStoreAndBorrow(let store, _): return Builder(after: store, context)
    case .viaCopyAddr:                     fatalError("copy_addr must be lowered")
    }
  }
}

private struct AvailableValues {
  let values: [AvailableValue]
  let endBorrowsToInsert: [BasicBlock]
  let deadEndBorrowBlocksOfLoad: [BasicBlock]

  func replaceCopyAddrsWithLoadsAndStores(_ context: FunctionPassContext) -> [AvailableValue] {
    return values.map {
      if case .viaCopyAddr(let copyAddr) = $0 {
        return .viaStore(copyAddr.replaceWithLoadAndStore(context).store)
      } else {
        return $0
      }
    }
  }
}
