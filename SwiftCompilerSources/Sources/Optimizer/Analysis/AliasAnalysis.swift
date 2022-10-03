//===--- AliasAnalysis.swift - the alias analysis -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import OptimizerBridging
import SIL

struct AliasAnalysis {
  let bridged: BridgedAliasAnalysis

  func mayRead(_ inst: Instruction, fromAddress: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, fromAddress.bridged) {
      case MayReadBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  func mayWrite(_ inst: Instruction, toAddress: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, toAddress.bridged) {
      case MayWriteBehavior, MayReadWriteBehavior, MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  func mayReadOrWrite(_ inst: Instruction, address: Value) -> Bool {
    switch AliasAnalysis_getMemBehavior(bridged, inst.bridged, address.bridged) {
      case MayReadBehavior, MayWriteBehavior, MayReadWriteBehavior,
           MayHaveSideEffectsBehavior:
        return true
      default:
        return false
    }
  }

  /// Returns the correct path for address-alias functions.
  static func getPtrOrAddressPath(for value: Value) -> SmallProjectionPath {
    let ty = value.type
    if ty.isAddress {
      // This is the regular case: the path selects any sub-fields of an address.
      return SmallProjectionPath(.anyValueFields)
    }
    // Some optimizations use the address-alias APIs with non-address SIL values.
    // TODO: this is non-intuitive and we should eliminate those API uses.
    if ty.isClass {
    // If the value is a (non-address) reference it means: all addresses within the class instance.
      return SmallProjectionPath(.anyValueFields).push(.anyClassField)
    }
    // Any other non-address value means: all addresses of any referenced class instances within the value.
    return SmallProjectionPath(.anyValueFields).push(.anyClassField).push(.anyValueFields)
  }
  
  static func register() {
    AliasAnalysis_register(
      // getMemEffectsFn
      { (bridgedCtxt: BridgedPassContext, bridgedVal: BridgedValue, bridgedInst: BridgedInstruction) -> BridgedMemoryBehavior in
        let context = PassContext(_bridged: bridgedCtxt)
        let inst = bridgedInst.instruction
        let val = bridgedVal.value
        let path = AliasAnalysis.getPtrOrAddressPath(for: val)
        if let apply = inst as? ApplySite {
          let effect = getMemoryEffect(of: apply, for: val, path: path, context)
          switch (effect.read, effect.write) {
            case (false, false): return NoneBehavior
            case (true, false):  return MayReadBehavior
            case (false, true):  return MayWriteBehavior
            case (true, true):   return MayReadWriteBehavior
          }
        }
        if val.at(path).isAddressEscaping(using: EscapesToInstructionVisitor(target: inst), context) {
          return MayReadWriteBehavior
        }
        return NoneBehavior
      },

      // isObjReleasedFn
      { (bridgedCtxt: BridgedPassContext, bridgedObj: BridgedValue, bridgedInst: BridgedInstruction) -> Bool in
        let context = PassContext(_bridged: bridgedCtxt)
        let inst = bridgedInst.instruction
        let obj = bridgedObj.value
        let path = SmallProjectionPath(.anyValueFields)
        if let apply = inst as? ApplySite {
          let effect = getOwnershipEffect(of: apply, for: obj, path: path, context)
          return effect.destroy
        }
        return obj.at(path).isEscaping(using: EscapesToInstructionVisitor(target: inst), context)
      },

      // isAddrVisibleFromObj
      { (bridgedCtxt: BridgedPassContext, bridgedAddr: BridgedValue, bridgedObj: BridgedValue) -> Bool in
        let context = PassContext(_bridged: bridgedCtxt)
        let addr = bridgedAddr.value
        let obj = bridgedObj.value
        let path = SmallProjectionPath(.anyValueFields)
      
        // Is the `addr` within all reachable objects/addresses, when start walking from `obj`?
        // Try both directions: 1. from addr -> obj
        return addr.at(path).isAddressEscaping(using: EscapesToValueVisitor(target: obj.at(SmallProjectionPath())), context)
      },

      // canReferenceSameFieldFn
      { (bridgedCtxt: BridgedPassContext, bridgedLhs: BridgedValue, bridgedRhs: BridgedValue) -> Bool in
        let context = PassContext(_bridged: bridgedCtxt)
        let lhs = bridgedLhs.value.at(AliasAnalysis.getPtrOrAddressPath(for: bridgedLhs.value))
        let rhs = bridgedRhs.value.at(AliasAnalysis.getPtrOrAddressPath(for: bridgedRhs.value))
        return lhs.canAddressAlias(with: rhs, context)
      }
    )
  }
}

private func getMemoryEffect(of apply: ApplySite, for address: Value, path: SmallProjectionPath, _ context: PassContext) -> SideEffects.Memory {

  struct Visitor : EscapeVisitorWithResult {
    let apply: ApplySite
    let calleeAnalysis: CalleeAnalysis
    var result = SideEffects.Memory.worstEffects

    mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
      let user = operand.instruction
      if user is ReturnInst {
        // Anything which is returned cannot escape to an instruction inside the function.
        return .ignore
      }
      if user == apply {
        if let argIdx = apply.argumentIndex(of: operand) {
          let newEffects = calleeAnalysis.getMemoryEffect(of: apply, for: argIdx, path: path.projectionPath)
          result.merge(with: newEffects)
        }
      }
      return .continueWalk
    }
  }

  let visitor = Visitor(apply: apply, calleeAnalysis: context.calleeAnalysis)
  if let result = address.at(path).visitAddress(using: visitor, context) {
    return result
  }
  return .worstEffects
}

private func getOwnershipEffect(of apply: ApplySite, for value: Value, path: SmallProjectionPath, _ context: PassContext) -> SideEffects.Ownership {
  struct Visitor : EscapeVisitorWithResult {
    let apply: ApplySite
    let calleeAnalysis: CalleeAnalysis
    var result = SideEffects.Ownership.worstEffects

    mutating func visitUse(operand: Operand, path: EscapePath) -> UseResult {
      let user = operand.instruction
      if user is ReturnInst {
        // Anything which is returned cannot escape to an instruction inside the function.
        return .ignore
      }
      if user == apply {
        if let argIdx = apply.argumentIndex(of: operand) {
          let newEffects = calleeAnalysis.getOwnershipEffect(of: apply, for: argIdx, path: path.projectionPath)
          result.merge(with: newEffects)
        }
      }
      return .continueWalk
    }
  }

  let visitor = Visitor(apply: apply, calleeAnalysis: context.calleeAnalysis)
  if let result = value.at(path).visit(using: visitor, context) {
    return result
  }
  return .worstEffects
}
