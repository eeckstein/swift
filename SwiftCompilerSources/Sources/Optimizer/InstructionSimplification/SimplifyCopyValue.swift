//===--- SimplifyCopyValue.swift ------------------------------------------===//
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

import AST
import SIL

extension CopyValueInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if fromValue.ownership == .none {
      uses.replaceAll(with: fromValue, context)
      context.erase(instruction: self)
      return
    }
    if !context.preserveDebugInfo {
      if tryRemoveCopyOfBorrow(copy: self, context) {
        return
      }
      tryRemoveProjectedCopy(copy: self, context)
    }
  }
}

/// Remove a `copy_value` of a borrowed owned value if the copy outlives the owned value.
///
/// ```
///   %2 = begin_borrow %1
///   %3 = copy_value %2                   // to be removed
///   %4 = some_forwarding_instructions %3
///   end_borrow %2                        // last use of the owned value
///   use %4
///   destroy_value %1                     // end of lifetime of the owned value
///   destroy_value %4
/// ```
/// ->
/// ```
///   %2 = begin_borrow %1
///   end_borrow %2
///   %4 = some_forwarding_instructions %1 // moved after the last use of the owned value
///   use %4
///   destroy_value %4
/// ```
///
/// This extends the owned value's lifetime to the end of the copy's lifetime. It never shortens
/// any lifetime.
///
/// Preconditions:
///   * The `destroy_value` of the owned value must be in the same basic block as the `copy_value`
///   * There are no uses of the copy or its forwarding instructions before the last use of the
///     owned value
///
private func tryRemoveCopyOfBorrow(copy: CopyValueInst, _ context: SimplifyContext) -> Bool {
  let block = copy.parentBlock

  guard let beginBorrow = copy.fromValue as? BeginBorrowInst else {
    return false
  }
  let ownedValue = beginBorrow.borrowedValue

  guard ownedValue.ownership == .owned,
        // Find the destroy_value of the owned value in the same block as copy.
        let destroy = ownedValue.uses.users(ofType: DestroyValueInst.self).first(where: { $0.parentBlock == block }),
        let lastUse = findLastUse(of: ownedValue, before: destroy),
        copy.strictlyDominatesInBlock(lastUse),
        let boundary = lastUse.next,
        checkForwardingChain(from: copy, outsideLiverangeEndingAt: boundary)
  else {
    return false
  }

  moveForwardingChain(from: copy, before: boundary, context)

  copy.replace(with: ownedValue, context)

  context.erase(instruction: destroy)
  return true
}

/// Returns the last instruction before `destroy` in the block which uses `ownedValue`, including
/// the ends of borrow scopes of `ownedValue`.
/// Returns nil if `ownedValue` has borrowing uses other than `begin_borrow`.
private func findLastUse(of ownedValue: Value, before destroy: DestroyValueInst) -> Instruction? {
  var lastUse: Instruction? = nil

  func update(with inst: Instruction) {
    // Uses in other blocks are located before the destroy's block or outside the value's lifetime.
    // Also some kind of uses, e.g. `debug_value`, can be located after the destroy.
    guard inst.parentBlock == destroy.parentBlock,
          inst.strictlyDominatesInBlock(destroy)
    else {
      return
    }
    if let current = lastUse, !current.strictlyDominatesInBlock(inst) {
      return
    }
    lastUse = inst
  }

  for use in ownedValue.uses where use.instruction != destroy {
    if let beginBorrow = use.instruction as? BeginBorrowInst {
      for endInst in beginBorrow.endInstructions {
        update(with: endInst)
      }
    } else if use.ownership == .borrow {
      // Other borrowing instructions (e.g. `begin_apply`, `partial_apply [on_stack]`,
      // `mark_dependence [nonescaping]`) keep `ownedValue` alive beyond the instruction itself.
      // For simplicity we don't compute the ends of their scopes.
      return nil
    }
    update(with: use.instruction)
  }
  return lastUse
}

/// Remove a `copy_value` from a projected owned value which outlives the enclosing owned value.
///
/// ```
///   %2 = begin_borrow %1
///   %3 = struct_extract %2, #S.a
///   %4 = copy_value %3                   // to be removed
///   %5 = some_forwarding_instructions %4
///   end_borrow %2
///   destroy_value %1                     // end of lifetime of the enclosing owned value
///   use %5                               // use of the copied value outside of the owned value's lifetime
/// ```

/// The `destroy_value` is replaced with destructure operations (`destructure_struct`, `destructure_tuple`).
/// The copied value can then be replaced by the corresponding element of the destructure.
/// The remaining elements are element-wise destroyed.
/// Any forwarding instructions of the copied value are moved out of the owned value's liferange.
/// Reference casts (`upcast`, `unchecked_ref_cast`) between the projections and the `copy_value`
/// are re-created on the destructured element.

/// ```
///   %2 = begin_borrow %1
///   end_borrow %2
///   (%3, %4) = destructure_struct %1
///   destroy_value %3                      // destroy the unused elements
///   %5 = some_forwarding_instructions %4  // moved out of %1's liferange
///   use %5
/// ```
///
/// Preconditions:
///   * The `destroy_value` must be in the same basic block as the `copy_value`
///   * There are no uses of the copy or its forwarding instructions inside the owned value's liverange
///   * The destructure creates at most `maxNumDestroysToCreate` element-wise destroys
///
private func tryRemoveProjectedCopy(copy: CopyValueInst, _ context: SimplifyContext) {
  let block = copy.parentBlock

  let (projectionPath, root) = getProjectionPath(of: copy.fromValue.lookThroughReferenceCasts)

  guard !projectionPath.isEmpty,
        projectionPath.isMaterializable,
        let beginBorrow = root as? BeginBorrowInst
  else {
    return
  }

  let ownedValue = beginBorrow.borrowedValue

  guard ownedValue.ownership == .owned,
        // Find the destroy_value of the owned value in the same block as copy.
        let destroy = ownedValue.uses.users(ofType: DestroyValueInst.self).first(where: { $0.parentBlock == block })
  else {
    return
  }

  guard checkForwardingChain(from: copy, outsideLiverangeEndingAt: destroy) else {
    return
  }

  // A destructure is just another way of expanding an aggregate operation into element-wise
  // operations, so it is bounded by the same limit: up to `maxNumFieldsToExpand` fields the
  // aggregate's `destroy_value` is expanded into element-wise destroys anyway, which makes the
  // destructure free and removing the `copy_value` a win. Beyond it the `destroy_value` is kept
  // whole and emitted as a single call to an outlined destroy helper, and destructuring would
  // trade that one call for a destroy of every single field.
  guard canDestructure(ownedValue.type, path: projectionPath,
                       creatingFewerDestroysThan: Type.maxNumFieldsToExpand,
                       in: copy.parentFunction)
  else {
    return
  }

  // ---- All checks passed. Perform the transformation. ----

  let builder = Builder(before: destroy, context)
  let finalFieldElement = createDestructureChain(of: ownedValue, path: projectionPath, builder)
  let castedElement = createReferenceCasts(of: copy.fromValue, on: finalFieldElement, builder)

  moveForwardingChain(from: copy, before: destroy, context)

  copy.replace(with: castedElement, context)

  context.erase(instruction: destroy)
}

/// Returns true if every non-forwarding, non-debug use of `copy` and its forwarding
/// chain is outside the owned value's liverange, i.e. at or after `boundary`.
/// Forwarding instructions with multiple operands, e.g. `struct`, are accepted if all other
/// operands are trivial, because those can be moved together with the instruction.
private func checkForwardingChain(from value: Value, outsideLiverangeEndingAt boundary: Instruction) -> Bool {
  for use in value.uses.ignoreDebugUses {
    let user = use.instruction
    if user.parentBlock != boundary.parentBlock || boundary.dominatesInBlock(user) {
      continue
    }
    guard let fwdInst = user as? (SingleValueInstruction & ForwardingInstruction),
          fwdInst.forwardedOperands.contains(where: { $0 == use }),
          fwdInst.operands.allSatisfy({ $0 == use || $0.value.ownership == .none }),
          checkForwardingChain(from: fwdInst, outsideLiverangeEndingAt: boundary)
    else {
      return false
    }
  }
  return true
}

/// Moves every forwarding instruction in the chain starting at `copy` to just
/// before `boundary`.
private func moveForwardingChain(from value: Value,
                                 before boundary: Instruction,
                                 _ context: SimplifyContext) {
  for use in value.uses {
    let user = use.instruction
    if user.parentBlock != boundary.parentBlock || boundary.dominatesInBlock(user) {
      continue
    }
    switch user {
    case let debugValue as DebugValueInst:
      debugValue.move(before: boundary, context)
    case let fwdInst as (SingleValueInstruction & ForwardingInstruction):
      fwdInst.move(before: boundary, context)
      moveForwardingChain(from: fwdInst, before: boundary, context)
    default:
      fatalError("unhandled user")
    }
  }
}

private extension Value {
  var lookThroughReferenceCasts: Value {
    switch self {
    case let upcast as UpcastInst:
      return upcast.fromInstance.lookThroughReferenceCasts
    case let refCast as UncheckedRefCastInst:
      return refCast.fromInstance.lookThroughReferenceCasts
    default:
      return self
    }
  }
}

/// Re-creates the reference casts of `value` (see `lookThroughReferenceCasts`) on `element`.
private func createReferenceCasts(of value: Value, on element: Value, _ builder: Builder) -> Value {
  switch value {
  case let upcast as UpcastInst:
    let operand = createReferenceCasts(of: upcast.fromInstance, on: element, builder)
    return builder.createUpcast(from: operand, to: upcast.type)
  case let refCast as UncheckedRefCastInst:
    let operand = createReferenceCasts(of: refCast.fromInstance, on: element, builder)
    return builder.createUncheckedRefCast(from: operand, to: refCast.type)
  default:
    return element
  }
}

private func getProjectionPath(of value: Value,
                               initialPath: SmallProjectionPath = SmallProjectionPath()
) -> (SmallProjectionPath, root: Value) {
  switch value {
  case let sei as StructExtractInst:
    let structType = sei.struct.type
    guard structType.getNominalFields(in: sei.parentFunction) != nil,
          (structType.nominal as! StructDecl).valueTypeDestructor == nil
    else {
      return (initialPath, root: sei)
    }
    return getProjectionPath(of: sei.struct, initialPath: initialPath.push(.structField, index: sei.fieldIndex))
  case let tei as TupleExtractInst:
    return getProjectionPath(of: tei.tuple, initialPath: initialPath.push(.tupleField, index: tei.fieldIndex))
  default:
    return (initialPath, root: value)
  }
}

/// Returns true if `createDestructureChain` creates fewer than `limit` `destroy_value`
/// instructions for `path`, and that number can be determined at all.
private func canDestructure(_ type: Type, path: SmallProjectionPath,
                            creatingFewerDestroysThan limit: Int, in function: Function) -> Bool {
  return areDestroysToCreateIsMoreThanLimit(for: type, path: path, limit: limit, in: function) != nil
}

/// Returns the number of `destroy_value` instructions which `createDestructureChain` creates for
/// `path`, or nil if that number cannot be determined or reaches `limit`.
private func areDestroysToCreateIsMoreThanLimit(for type: Type, path: SmallProjectionPath, limit: Int,
                                   in function: Function) -> Int? {
  let (kind, index, subPath) = path.pop()

  switch kind {
  case .root:
    return 0
  case .structField:
    guard let fields = type.getNominalFields(in: function) else {
      return nil
    }
    return areDestroysToCreateIsMoreThanLimit(for: fields, at: index, subPath: subPath,
                                              limit: limit, in: function)
  case .tupleField:
    return areDestroysToCreateIsMoreThanLimit(for: type.tupleElements, at: index, subPath: subPath,
                                              limit: limit, in: function)
  default:
    return nil
  }
}

private func areDestroysToCreateIsMoreThanLimit<Elements: RandomAccessCollection>(
  for elements: Elements, at index: Int, subPath: SmallProjectionPath, limit: Int,
  in function: Function
) -> Int? where Elements.Element == Type, Elements.Index == Int {
  guard index < elements.count,
        var num = areDestroysToCreateIsMoreThanLimit(for: elements[index], path: subPath,
                                                     limit: limit, in: function)
  else {
    return nil
  }
  for elementIdx in elements.indices
      where elementIdx != index && !elements[elementIdx].isTrivial(in: function) {
    num += 1
    if num >= limit {
      return nil
    }
  }
  return num
}

private func createDestructureChain(of value: Value, path: SmallProjectionPath, _ builder: Builder) -> Value {
  let (kind, index, subPath) = path.pop()

  let destructure: MultipleValueInstruction
  switch kind {
  case .root:
    return value
  case .structField:
    destructure = builder.createDestructureStruct(struct: value)
  case .tupleField:
    destructure = builder.createDestructureTuple(tuple: value)
  default:
    fatalError("unsupported projection kind")
  }
  for (i, element) in destructure.results.enumerated() {
    if i != index && element.ownership != .none {
      builder.createDestroyValue(operand: element)
    }
  }
  return createDestructureChain(of: destructure.results[index], path: subPath, builder)
}
