//===--- CalleeAnalysis.swift - the callee analysis -----------------------===//
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

public struct CalleeAnalysis {
  let bridged: BridgedCalleeAnalysis

  public func getCallees(callee: Value) -> FunctionArray? {
    let bridgedFuncs = CalleeAnalysis_getCallees(bridged, callee.bridged)
    if bridgedFuncs.incomplete != 0 {
      return nil
    }
    return FunctionArray(bridged: bridgedFuncs)
  }

  public func getIncompleteCallees(callee: Value) -> FunctionArray {
    return FunctionArray(bridged: CalleeAnalysis_getCallees(bridged, callee.bridged))
  }

  public func getDestructor(ofExactType type: Type) -> Function? {
    let destructors = FunctionArray(bridged: CalleeAnalysis_getDestructors(bridged, type.bridged, /*isExactType*/ 1))
    if destructors.count == 1 {
      return destructors[0]
    }
    return nil
  }

  public func getDestructors(of type: Type) -> FunctionArray? {
    let bridgedDtors = CalleeAnalysis_getDestructors(bridged, type.bridged, /*isExactType*/ 0)
    if bridgedDtors.incomplete != 0 {
      return nil
    }
    return FunctionArray(bridged: bridgedDtors)
  }

  public func getMemoryEffect(of apply: ApplySite, for argIdx: Int, path: SmallProjectionPath) -> SideEffects.Memory {
    let calleeArgIdx = apply.calleeArgIndex(callerArgIndex: argIdx)
    let convention = apply.getArgumentConvention(calleeArgIndex: calleeArgIdx)

    guard let callees = getCallees(callee: apply.callee) else {
      return SideEffects.Memory(read: true, write: !convention.isIndirectIn)
    }
  
    var effects = SideEffects.Memory(read: false, write: false)

    for callee in callees {
      if let sideEffects = callee.effects.sideEffects {
        let argEffect = sideEffects.getArgumentEffects(for: calleeArgIdx)
        if let effectPath = argEffect.read, effectPath.mayOverlap(with: path) {
          effects.read = true
        }
        if let effectPath = argEffect.write, effectPath.mayOverlap(with: path) {
          effects.write = true
        }
      } else {
        let calleeEffects = callee.getDefinedSideEffects(forArgument: calleeArgIdx)
        effects.merge(with: calleeEffects.memory)
      }
      if effects == .worstEffects { break }
    }
    if convention.isIndirectIn {
      effects.write = false
    }
    return effects
  }

  public func getOwnershipEffect(of apply: ApplySite, for argIdx: Int, path: SmallProjectionPath) -> SideEffects.Ownership {
    let calleeArgIdx = apply.calleeArgIndex(callerArgIndex: argIdx)
    let convention = apply.getArgumentConvention(calleeArgIndex: calleeArgIdx)

    guard let callees = getCallees(callee: apply.callee) else {
      return SideEffects.Ownership(copy: true, destroy: !convention.isGuaranteed)
    }
  
    var effects = SideEffects.Ownership(copy: false, destroy: false)

    for callee in callees {
      if let sideEffects = callee.effects.sideEffects {
        let argEffect = sideEffects.getArgumentEffects(for: calleeArgIdx)
        if let effectPath = argEffect.copy, effectPath.mayOverlap(with: path) {
          effects.copy = true
        }
        if let effectPath = argEffect.destroy, effectPath.mayOverlap(with: path) {
          effects.destroy = true
        }
      } else {
        let calleeEffects = callee.getDefinedSideEffects(forArgument: calleeArgIdx)
        effects.merge(with: calleeEffects.ownership)
      }
      if effects == .worstEffects { break }
    }
    if convention.isGuaranteed {
      effects.destroy = false
    }
    return effects
  }
}

public struct FunctionArray : RandomAccessCollection, FormattedLikeArray {
  fileprivate let bridged: BridgedCalleeList

  public var startIndex: Int { 0 }
  public var endIndex: Int { BridgedFunctionArray_size(bridged) }

  public subscript(_ index: Int) -> Function {
    return BridgedFunctionArray_get(bridged, index).function
  }
}
