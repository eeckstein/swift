//===--- AccessScopeDestroySinking.swift ----------------------------------===//
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

/// Sinks destroys out of access scopes.
///
/// ```
///   %a = begin_access [modify] [dynamic] %addr
///   ...
///   destroy_value %x           // or a release in non-OSSA
///   ...
///   end_access %a
///   // destroy_value %x is moved here
/// ```
///
/// This extends the lifetime of the destroyed value, but might allow AccessEnforcementOpts and
/// AccessEnforcementWMO to mark the access scope as having no nested conflict, because a destroy
/// can call a deinit which can access anything.
///
/// In OSSA, a `store [assign]` and a `destroy_addr` of a loadable type also destroy a value.
/// Those are split into a `load [take]` and a `store [init]` (or nothing for `destroy_addr`) and
/// the destroy of the loaded value is sunk:
///
/// ```
///   %a = begin_access [modify] [dynamic] %addr
///   store %new to [assign] %a
///   end_access %a
/// ```
/// ->
/// ```
///   %a = begin_access [modify] [dynamic] %addr
///   %old = load [take] %a
///   store %new to [init] %a
///   end_access %a
///   destroy_value %old
/// ```
///
/// The `end_access`es of the scope can be in other blocks than the destroy:
///
/// ```
///   bb0:
///     %a = begin_access [modify] [dynamic] %addr
///     %old = load [take] %a
///     destroy_value %old
///     switch_enum %e, case #A: bb1, case #B: bb2
///   bb1:
///     br bb3(%1)
///   bb2:
///     br bb3(%2)
///   bb3(%new):
///     store %new to [init] %a
///     end_access %a
///     // destroy_value %old is moved here
/// ```
///
/// The destroy must be on all paths from the `begin_access` to the `end_access`es and there must
/// not be a "barrier" between the destroy and the `end_access`es.
///
/// Dead-end blocks in the access scope (blocks with an `unreachable` but without an `end_access`)
/// are handled like `end_access`es: there must not be a barrier on a path to the `unreachable`.
/// In OSSA, the lifetime of a sunk destroy is ended with a `destroy_value [dead_end]` before the
/// `unreachable`.
///
let accessScopeDestroySinking = FunctionPass(name: "access-scope-destroy-sinking") {
  (function: Function, context: FunctionPassContext) in

  // Process the scopes in reverse order to handle inner scopes before outer scopes. This lets a
  // destroy sink out of nested scopes.
  let accessScopes = function.instructions.compactMap { $0 as? BeginAccessInst }
  for beginAccess in accessScopes.reversed() {
    sinkDestroys(outOf: beginAccess, context)
  }
}

private func sinkDestroys(outOf beginAccess: BeginAccessInst, _ context: FunctionPassContext) {
  let scopeEnds = collectScopeEnds(of: beginAccess, context)
  if scopeEnds.isEmpty {
    return
  }
  for destroy in collectDestroys(inScopeOf: beginAccess, scopeEnds: scopeEnds, context) {
    if canSink(destroy, outOf: beginAccess, scopeEnds: scopeEnds, context) &&
       !isInLoop(destroy, scopeEnds: scopeEnds, context)
    {
      destroy.sink(to: scopeEnds, context)
    }
  }
}

/// Returns the `end_access`es of the scope and the `unreachable`s of dead-end blocks which are
/// reachable from the `begin_access` without passing an `end_access`.
/// Dead-end blocks are handled like `end_access`es: if a destroy is sunk, it is sunk to the
/// `unreachable` in such a block. In OSSA, this is a `destroy_value [dead_end]`.
private func collectScopeEnds(of beginAccess: BeginAccessInst, _ context: FunctionPassContext) -> [Instruction] {
  var scopeEnds: [Instruction] = Array(beginAccess.endAccessInstructions)

  var endAccessBlocks = BasicBlockSet(context)
  defer { endAccessBlocks.deinitialize() }
  endAccessBlocks.insert(contentsOf: scopeEnds.lazy.map { $0.parentBlock })

  var worklist = BasicBlockWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(beginAccess.parentBlock)

  while let block = worklist.pop() {
    if endAccessBlocks.contains(block) {
      continue
    }
    if let unreachable = block.terminator as? UnreachableInst {
      scopeEnds.append(unreachable)
      continue
    }
    worklist.pushIfNotVisited(contentsOf: block.successors)
  }
  return scopeEnds
}

private func collectDestroys(inScopeOf beginAccess: BeginAccessInst, scopeEnds: [Instruction],
                             _ context: FunctionPassContext) -> [Destroy] {
  var destroys = [Destroy]()
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for scopeEnd in scopeEnds {
    worklist.pushPredecessors(of: scopeEnd, ignoring: beginAccess)
  }
  while let inst = worklist.pop() {
    if let destroy = Destroy(inst) {
      destroys.append(destroy)
    }
    worklist.pushPredecessors(of: inst, ignoring: beginAccess)
  }
  return destroys
}

/// Walks up from the scope ends to the destroy and checks that there is no barrier in between
/// and that the destroy is on all paths from the `begin_access`.
private func canSink(_ destroy: Destroy, outOf beginAccess: BeginAccessInst, scopeEnds: [Instruction],
                     _ context: FunctionPassContext) -> Bool {
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }

  for scopeEnd in scopeEnds {
    worklist.pushPredecessors(of: scopeEnd, ignoring: destroy.instruction)
  }
  while let inst = worklist.pop() {
    if inst == beginAccess {
      // There is a path from the `begin_access` to a scope end which doesn't go through the destroy.
      return false
    }
    if inst.isBarrierForDestroySinking(outOf: beginAccess) {
      return false
    }
    // Don't sink a destroy over another destroy of the same value.
    if let destroyedValue = destroy.destroyedValue,
       let otherDestroyedValue = Destroy(inst)?.destroyedValue,
       otherDestroyedValue == destroyedValue
    {
      return false
    }
    worklist.pushPredecessors(of: inst, ignoring: destroy.instruction)
  }
  return true
}

/// Returns true if the destroy is in a loop within the access scope, i.e. it is executed multiple
/// times. Note that `canSink` already verified that the destroy dominates all scope ends.
private func isInLoop(_ destroy: Destroy, scopeEnds: [Instruction], _ context: FunctionPassContext) -> Bool {
  var range = BasicBlockRange(begin: destroy.instruction.parentBlock, context)
  defer { range.deinitialize() }

  range.insert(contentsOf: scopeEnds.map { $0.parentBlock })
  return !range.exits.isEmpty
}

/// An instruction which destroys a value.
private enum Destroy {
  /// A `destroy_value` or a release in non-OSSA.
  case destroy(Instruction)

  /// A `store [assign]` in OSSA.
  case storeAssign(StoreInst)

  /// A `destroy_addr` of a loadable type in OSSA.
  case destroyAddr(DestroyAddrInst)

  init?(_ inst: Instruction) {
    switch inst {
    case let destroy as DestroyValueInst:
      // A destroy must not be moved out of the access scope if the destroyed value depends on the
      // accessed memory.
      if destroy.isDeadEnd || destroy.destroyedValue is MarkDependenceInst {
        return nil
      }
      self = .destroy(destroy)
    case is ReleaseValueInst, is ReleaseValueAddrInst, is StrongReleaseInst, is UnmanagedReleaseValueInst:
      self = .destroy(inst)
    case let store as StoreInst:
      guard store.storeOwnership == .assign else {
        return nil
      }
      self = .storeAssign(store)
    case let destroyAddr as DestroyAddrInst:
      let function = destroyAddr.parentFunction
      guard function.hasOwnership,
            destroyAddr.destroyedAddress.type.isLoadable(in: function)
      else {
        return nil
      }
      self = .destroyAddr(destroyAddr)
    default:
      return nil
    }
  }

  var instruction: Instruction {
    switch self {
    case .destroy(let inst):              return inst
    case .storeAssign(let store):         return store
    case .destroyAddr(let destroyAddr):   return destroyAddr
    }
  }

  /// The destroyed value. Not available for destroys of values in memory.
  var destroyedValue: Value? {
    switch self {
    case .destroy(let inst):        return inst.operands[0].value
    case .storeAssign, .destroyAddr: return nil
    }
  }

  /// Moves the destroy to all `scopeEnds`.
  func sink(to scopeEnds: [Instruction], _ context: FunctionPassContext) {
    switch self {
    case .destroy(let inst):
      for scopeEnd in scopeEnds where scopeEnd is EndAccessInst {
        inst.copy(before: scopeEnd.next!, context)
      }
      createDeadEndDestroys(of: inst.operands[0].value, at: scopeEnds, context)
      context.erase(instruction: inst)

    case .storeAssign(let store):
      let builder = Builder(before: store, context)
      let oldValue = builder.createLoad(fromAddress: store.destination, ownership: .take)
      store.set(ownership: .initialize, context)
      createDestroys(of: oldValue, at: scopeEnds, context)

    case .destroyAddr(let destroyAddr):
      let builder = Builder(before: destroyAddr, context)
      let oldValue = builder.createLoad(fromAddress: destroyAddr.destroyedAddress, ownership: .take)
      context.erase(instruction: destroyAddr)
      createDestroys(of: oldValue, at: scopeEnds, context)
    }
  }
}

private func createDestroys(of value: Value, at scopeEnds: [Instruction], _ context: FunctionPassContext) {
  for scopeEnd in scopeEnds where scopeEnd is EndAccessInst {
    let builder = Builder(before: scopeEnd.next!, context)
    builder.createDestroyValue(operand: value)
  }
  createDeadEndDestroys(of: value, at: scopeEnds, context)
}

/// In OSSA, the lifetime of the destroyed value must be completed at `unreachable`s.
private func createDeadEndDestroys(of value: Value, at scopeEnds: [Instruction], _ context: FunctionPassContext) {
  guard value.parentFunction.hasOwnership else {
    return
  }
  for scopeEnd in scopeEnds where scopeEnd is UnreachableInst {
    let builder = Builder(before: scopeEnd, context)
    builder.createDestroyValue(operand: value, isDeadEnd: true)
  }
}

private extension Instruction {
  func isBarrierForDestroySinking(outOf beginAccess: BeginAccessInst) -> Bool {
    if isDeinitBarrier && !onlyAccessesMemory(of: beginAccess) {
      // This includes calls, which hide many dangers, from checking reference counts, to beginning
      // keypath access, to forcing memory to be live.
      return true
    }
    switch self {
    case is IsUniqueInst, is BeginCOWMutationInst, is DestroyNotEscapedClosureInst:
      // Don't extend lifetime past any sort of uniqueness check.
      return true
    case is Deallocation:
      // Don't extend object lifetime past deallocation.
      return true
    case is BeginAccessInst, is BeginUnpairedAccessInst:
      // Avoid introducing access conflicts.
      return true
    case let builtin as BuiltinInst:
      // Rare builtins may be sensitive to object lifetime or deinit side effects.
      return builtin.mayHaveSideEffects
    default:
      return false
    }
  }

  /// Returns true for loads and stores which only access the memory of the access scope, e.g.
  ///
  /// ```
  ///   %a = begin_access [modify] [dynamic] %global
  ///   %old = load [take] %a
  ///   destroy_value %old
  ///   store %new to [init] %a      // not a barrier, though it writes to a global
  ///   end_access %a
  /// ```
  ///
  /// Such an instruction is a deinit barrier if the memory is a global or a pointer. But a deinit
  /// cannot observe the memory of the access scope without violating exclusivity: within a
  /// `modify` scope, it must not access the memory at all, and within a `read` scope, it can
  /// only read it.
  private func onlyAccessesMemory(of beginAccess: BeginAccessInst) -> Bool {
    guard beginAccess.enforcement == .dynamic || beginAccess.enforcement == .static else {
      return false
    }
    let address: Value
    switch self {
    case let load as LoadInst:
      address = load.address
    case let loadBorrow as LoadBorrowInst:
      address = loadBorrow.address
    case let endBorrow as EndBorrowInst:
      guard let loadBorrow = endBorrow.borrow as? LoadBorrowInst else {
        return false
      }
      address = loadBorrow.address
    case let store as StoreInst:
      address = store.destination
    default:
      return false
    }
    if case .access(let scope) = address.enclosingAccessScope {
      return scope == beginAccess
    }
    return false
  }
}
