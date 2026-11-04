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

struct CalleeAnalysis {
  let bridged: BridgedCalleeAnalysis

  static func register() {
    BridgedCalleeAnalysis.registerAnalysis(
      // isDeinitBarrierFn:
      { (inst : BridgedInstruction, bca: BridgedCalleeAnalysis) -> Bool in
        return inst.instruction.isDeinitBarrier(bca.analysis)
      },
      // getMemBehaviorFn
      { (bridgedApply: BridgedInstruction, observeRetains: Bool, bca: BridgedCalleeAnalysis) -> BridgedMemoryBehavior in
        let apply = bridgedApply.instruction as! FullApplySite
        let e = bca.analysis.getSideEffects(ofApply: apply)
        return e.getMemBehavior(observeRetains: observeRetains)
      }
    )
  }

  func getCallees(callee: Value) -> FunctionArray? {
    let bridgedFuncs = bridged.getCallees(callee.bridged)
    if bridgedFuncs.isIncomplete() {
      return nil
    }
    return FunctionArray(bridged: bridgedFuncs)
  }

  func getIncompleteCallees(callee: Value) -> FunctionArray {
    return FunctionArray(bridged: bridged.getCallees(callee.bridged))
  }

  func getClassDestructors(of type: Type, isExactType: Bool) -> FunctionArray? {
    precondition(type.isClass)
    let destructors = FunctionArray(bridged: bridged.getDestructors(type.bridged, isExactType))
    if destructors.isIncomplete {
      return nil
    }
    return destructors
  }

  func getClassDestructor(ofExactType type: Type) -> Function? {
    guard let destructors = getClassDestructors(of: type, isExactType: true) else {
      return nil
    }
    return destructors.singleElement
  }

  /// Returns true if `predicate` is true for all destructors and deinits which may be called if a value
  /// of `type` is destroyed.
  ///
  /// The predicate is called with the destructor and the type of the destroyed (sub-)value which is passed
  /// as argument to the destructor:
  /// * For a class type these are the destructors of the class and all its sub-classes.
  ///   The argument of such a destructor is the class reference.
  /// * For a non-copyable value type with a deinit this is the deinit, which also destroys the fields.
  ///   The argument of such a deinit is the value itself.
  /// * For other aggregates (structs, tuples, enums) these are the destructors of the fields and payloads.
  ///
  /// If `deinitIsDropped` is true, the deinit of a non-copyable `type` is not called (e.g. because it's
  /// the result of a `drop_deinit`), but only the destructors of its fields.
  ///
  /// Returns false if not all destructors are known.
  func allDestructorsSatisfy(of type: Type, in function: Function,
                             deinitIsDropped: Bool = false,
                             _ predicate: (Function, Type) -> Bool,
                             recursionDepth: Int = 0,
                             _ context: some Context) -> Bool
  {
    // Destroying a weak or unowned reference doesn't run a destructor.
    if type.isTrivial(in: function) || type.isReferenceStorageType {
      return true
    }
    // Bail on deeply nested and recursive types.
    if recursionDepth >= 16 {
      return false
    }
    if type.isClass {
      guard let destructors = getClassDestructors(of: type, isExactType: false) else {
        return false
      }
      return destructors.allSatisfy { predicate($0, type) }
    }
    if type.isValueTypeWithDeinit && !deinitIsDropped {
      // Don't deserialize the deinit: an analysis must not change the module.
      guard let deinitFunc = context.lookupSpecializedDeinit(ofType: type) ??
                             context.lookupDeinit(ofNominal: type.nominal!, deserializeLazily: false)
      else {
        return false
      }
      return predicate(deinitFunc, type)
    }
    if type.isStruct {
      guard let fields = type.getNominalFields(in: function) else {
        return false
      }
      return fields.allSatisfy {
        allDestructorsSatisfy(of: $0, in: function, predicate, recursionDepth: recursionDepth + 1, context)
      }
    }
    if type.isTuple {
      return type.tupleElements.allSatisfy {
        allDestructorsSatisfy(of: $0, in: function, predicate, recursionDepth: recursionDepth + 1, context)
      }
    }
    if type.isEnum {
      guard let enumCases = type.getEnumCases(in: function) else {
        return false
      }
      return enumCases.allSatisfy {
        if let payload = $0.payload {
          return allDestructorsSatisfy(of: payload, in: function, predicate, recursionDepth: recursionDepth + 1, context)
        }
        return true
      }
    }
    // Existentials, archetypes, closures, boxes, bridge objects, etc.
    return false
  }

  /// Returns the global (i.e. not argument specific) side effects of an apply.
  func getSideEffects(ofApply apply: FullApplySite) -> SideEffects.GlobalEffects {
    return getSideEffects(ofCallee: apply.callee)
  }

  func getSideEffects(ofCallee callee: Value) -> SideEffects.GlobalEffects {
    guard let callees = getCallees(callee: callee) else {
      return .worstEffects
    }

    var result = SideEffects.GlobalEffects()
    for callee in callees {
      let calleeEffects = callee.getSideEffects()
      result.merge(with: calleeEffects)
    }
    return result
  }

  /// Returns the argument specific side effects of an apply.
  func getSideEffects(of apply: FullApplySite, operand: Operand, path: SmallProjectionPath) -> SideEffects.GlobalEffects {
    var result = SideEffects.GlobalEffects()
    guard let calleeArgIdx = apply.calleeArgumentIndex(of: operand) else {
      return result
    }
    let convention = apply.convention(of: operand)!
    let argument = operand.value.at(path)

    guard let callees = getCallees(callee: apply.callee) else {
      return .worstEffects.restrictedTo(argument: argument, withConvention: convention)
    }
  
    for callee in callees {
      let calleeEffects = callee.getSideEffects(forArgument: argument,
                                                atIndex: calleeArgIdx,
                                                withConvention: convention)
      result.merge(with: calleeEffects)
    }
    return result.restrictedTo(argument: argument, withConvention: convention)
  }
}

extension Value {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    guard let callees = analysis.getCallees(callee: self) else {
      return true
    }
    return callees.contains { $0.isDeinitBarrier }
  }
}

extension FullApplySite {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    return callee.isBarrier(analysis)
  }
}

extension EndApplyInst {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    return (operand.value.definingInstruction as! FullApplySite).isBarrier(analysis)
  }
}

extension AbortApplyInst {
  fileprivate func isBarrier(_ analysis: CalleeAnalysis) -> Bool {
    return (operand.value.definingInstruction as! FullApplySite).isBarrier(analysis)
  }
}

extension Instruction {
  /// Whether lifetime ends of lexical values may safely be hoisted over this
  /// instruction.
  ///
  /// Deinitialization barriers constrain variable lifetimes. Lexical
  /// end_borrow, destroy_value, and destroy_addr cannot be hoisted above them.
  final func isDeinitBarrier(_ analysis: CalleeAnalysis) -> Bool {
    if self.isFullApplySite {
      return (self as! FullApplySite).isBarrier(analysis)
    }
    if let eai = self as? EndApplyInst {
      return eai.isBarrier(analysis)
    }
    if let aai = self as? AbortApplyInst {
      return aai.isBarrier(analysis)
    }
    return isDeinitBarrier
  }
}

struct FunctionArray : RandomAccessCollection, FormattedLikeArray {
  fileprivate let bridged: BridgedCalleeAnalysis.CalleeList

  var startIndex: Int { 0 }
  var endIndex: Int { bridged.getCount() }

  var isIncomplete: Bool { bridged.isIncomplete() }

  subscript(_ index: Int) -> Function {
    return bridged.getCallee(index).function
  }
}
// Bridging utilities

extension BridgedCalleeAnalysis {
  var analysis: CalleeAnalysis { .init(bridged: self) }
}

