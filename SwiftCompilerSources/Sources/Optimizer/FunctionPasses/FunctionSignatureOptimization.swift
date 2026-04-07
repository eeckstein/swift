//===--- FunctionSignatureOptimization.swift -------------------------------==//
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

let functionSignatureOptimization = ModulePass(name: "function-signature-optimization") {
  (moduleContext: ModulePassContext) in

  var functionSpecializations = Dictionary<Function, [ArgumentSpecialization]>()

  for function in moduleContext.functions {
    for inst in function.instructions {
      switch inst {
      case let apply as ApplyInst:
        trySpecialize(apply: apply, cacheIn: &functionSpecializations, moduleContext)
      case let tryApply as TryApplyInst:
        trySpecialize(apply: tryApply, cacheIn: &functionSpecializations, moduleContext)
      default:
        break
      }
    }
  }
}

private func trySpecialize(apply: FullApplySite,
                           cacheIn functionSpecializations: inout Dictionary<Function, [ArgumentSpecialization]>,
                           _ moduleContext: ModulePassContext) {
  guard let callee = apply.referencedFunction else {
    return
  }

  let specializations: [ArgumentSpecialization]
  if let existingSpecializations = functionSpecializations[callee] {
    specializations = existingSpecializations
  } else {
    specializations = moduleContext.transform(function: callee) { context in
      getParameterSpecializations(for: callee, context)
    }
    functionSpecializations[callee] = specializations
  }

  let benefit = moduleContext.transform(function: apply.parentFunction) { context in
    apply.canBenefit(from: specializations, context)
  }
  guard benefit,
        callee.isCompatibleWithLifetimeDependencies(argumentSpecializations: specializations)
  else {
    return
  }
  specialize(function: callee, with: specializations, callerSiteApply: apply, moduleContext)
  moduleContext.transform(function: apply.parentFunction) { context in
    context.inlineFunction(apply: apply, mandatoryInline: false)
  }
}

private func getParameterSpecializations(for function: Function,
                                         _ context: FunctionPassContext
) -> [ArgumentSpecialization] {
  var specializations = [ArgumentSpecialization]()
  for (argIndex, arg) in function.arguments.enumerated() {
    if let specKind = getSpecializationKind(for: arg, context) {
      specializations.append(ArgumentSpecialization(argumentIndex: argIndex, kind: specKind))
    }
  }
  return specializations
}

private func getSpecializationKind(for argument: FunctionArgument,
                                   _ context: FunctionPassContext
) -> ArgumentSpecialization.Kind? {
  if argument.uses.ignoreDebugUses.isEmpty {
    return .dead
  }

  switch argument.convention {
  case .directOwned:
    if !argument.type.isMoveOnly,
       argument.isDestroyedAtFunctionExits(context)
    {
      return .ownedToGuaranteed
    }
  case .directGuaranteed:
    if argument.isCopiedAtFunctionEntry(context) {
      return .guaranteedToOwned
    }
  default:
    break
  }
  return nil
}

private extension FunctionArgument {
  func isDestroyedAtFunctionExits(_ context: FunctionPassContext) -> Bool {
    precondition(ownership == .owned)

    guard uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst }) else {
      return false
    }

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(contentsOf: uses.endingLifetime.users)

    let calleeAnalysis = context.calleeAnalysis

    while let inst = worklist.pop() {
      if inst.isDeinitBarrier(calleeAnalysis) {
        return false
      }
      worklist.pushSuccessors(of: inst)
    }
    return true
  }

  func isCopiedAtFunctionEntry(_ context: FunctionPassContext) -> Bool {
    precondition(ownership == .guaranteed)

    guard uses.ignoreDebugUses.allSatisfy({ $0.instruction is CopyValueInst }) else {
      return false
    }

    var entryToCopies = BasicBlockRange(begin: parentFunction.entryBlock, context)
    defer { entryToCopies.deinitialize() }
    
    entryToCopies.insert(contentsOf: uses.users(ofType: CopyValueInst.self).lazy.map { $0.parentBlock})

    guard entryToCopies.exits.isEmpty else {
      return false
    }

    guard uses.users(ofType: CopyValueInst.self).allSatisfy({ !entryToCopies.contains($0.parentBlock) }) else {
      return false
    }

    return true
  }
}

private extension FullApplySite {
  func canBenefit(from parameterSpecializations: [ArgumentSpecialization], _ context: FunctionPassContext) -> Bool {
    for spec in parameterSpecializations {
      if let arg = operand(forCalleeArgumentIndex: spec.argumentIndex),
         arg.canBenefit(from: spec.kind, context)
      {
        return true
      }
    }
    return false
  }
}

private extension Operand {
  func canBenefit(from specializationKind: ArgumentSpecialization.Kind, _ context: FunctionPassContext) -> Bool {
    switch specializationKind {
    case .ownedToGuaranteed:
      switch self.value {
      case is CopyValueInst:
        return true
      default:
        // TODO: check load_borrow
        return false
      }
    case .guaranteedToOwned:
      return self.value.isDestroyed(after: self.instruction, context)
    case .dead:
      return true
    case .explode:
      // TODO
      return false
    }
  }
}

private extension Value {
  func isDestroyed(after beginInstruction: Instruction, _ context: FunctionPassContext) -> Bool {
    guard uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst }) else {
      return false
    }

    var userSet = InstructionSet(context)
    defer { userSet.deinitialize() }
    userSet.insert(contentsOf: uses.filter{ !$0.endsLifetime }.users)

    var worklist = InstructionWorklist(context)
    defer { worklist.deinitialize() }
    worklist.pushIfNotVisited(contentsOf: uses.endingLifetime.users)

    let calleeAnalysis = context.calleeAnalysis

    while let inst = worklist.pop() {
      if inst.isDeinitBarrier(calleeAnalysis) {
        return false
      }
      if userSet.contains(inst) {
        return false
      }
      worklist.pushPredecessors(of: inst, ignoring: beginInstruction)
    }
    return true
  }
}

private func specialize(function: Function,
                        with argumentSpecializations: [ArgumentSpecialization],
                        callerSiteApply: FullApplySite,
                        _ moduleContext: ModulePassContext)
{
  let specializedFuncName = moduleContext.mangle(withSignatureSpecializedArguments: argumentSpecializations, from: function)

  if moduleContext.lookupFunction(name: specializedFuncName) != nil {
    return
  }

  var specializedParams = Array(function.convention.parameters)

  let argumentConventions = function.argumentConventions
  var offset = 0

  for spec in argumentSpecializations {
    let paramIdx = argumentConventions.parameterIndex(ofArgumentIndex: spec.argumentIndex)! + offset

    switch spec.kind {
    case .ownedToGuaranteed:
      assert(specializedParams[paramIdx].convention == .directOwned)
      specializedParams[paramIdx] = specializedParams[paramIdx].with(convention: .directGuaranteed)
    case .guaranteedToOwned:
      assert(specializedParams[paramIdx].convention == .directGuaranteed)
      specializedParams[paramIdx] = specializedParams[paramIdx].with(convention: .directOwned)
    case .dead:
      specializedParams.remove(at: paramIdx)
      offset -= 1
    case .explode:
      fatalError("todo")
    }
  }

  let specializedFunction = moduleContext.createSpecializedFunctionDeclaration(
      from: function, withName: specializedFuncName,
      withParams: specializedParams,
      makeBare: true)

  moduleContext.moveFunctionBody(from: function, to: specializedFunction)

  moduleContext.transform(function: function) { context in
    let newEntryBlock = context.appendNewBlock(in: function)
    var newApplyArgs = [Value]()
    for origArg in specializedFunction.arguments {
      newApplyArgs.append(newEntryBlock.addFunctionArgument(type: origArg.type, context))
    }
    var toDelete = [Value]()
    var offset = 0
    for spec in argumentSpecializations {
      let argIdx = spec.argumentIndex + offset
      switch spec.kind {
      case .ownedToGuaranteed:
        toDelete.append(newApplyArgs[argIdx])
      case .guaranteedToOwned:
        let builder = Builder(atEndOf: newEntryBlock, location: function.location, context)
        let copy = builder.createCopyValue(operand: newApplyArgs[argIdx])
        newApplyArgs[argIdx] = copy
      case .dead:
        newApplyArgs.remove(at: argIdx)
        offset -= 1
      case .explode:
        fatalError("todo")
      }
    }

    let builder = Builder(atEndOf: newEntryBlock, location: function.location, context)
    let fri = builder.createFunctionRef(specializedFunction)

    switch callerSiteApply {
    case let applyInst as ApplyInst:
      let newApply = builder.createApply(function: fri,
                                         function.forwardingSubstitutionMap,
                                         arguments: newApplyArgs,
                                         isNonThrowing: applyInst.isNonThrowing,
                                         isNonAsync: applyInst.isNonAsync)

      for v in toDelete {
        builder.createDestroyValue(operand: v)
      }
      // TODO: handle return_borrow
      builder.createReturn(of: newApply)
    case let tryApply as TryApplyInst:
      let normalBlock = context.appendNewBlock(in: function)
      let errorBlock = context.appendNewBlock(in: function)
      builder.createTryApply(function: fri,
                             function.forwardingSubstitutionMap,
                             arguments: newApplyArgs,
                             normalBlock: normalBlock, errorBlock: errorBlock,
                             isNonAsync: tryApply.isNonAsync)

      let retTy = function.convention.results[0].getReturnValueType(function: function)
      let returnVal = normalBlock.addArgument(type: retTy.loweredType(in: function),
                                              ownership: tryApply.normalBlock.arguments[0].ownership,
                                              context)
      Builder(atBeginOf: normalBlock, context).createReturn(of: returnVal)

      let errorTy = function.convention.errorResult!.getReturnValueType(function: function)
      let errorVal = errorBlock.addArgument(type: errorTy.loweredType(in: function),
                                            ownership: tryApply.errorBlock.arguments[0].ownership,
                                            context)
      Builder(atBeginOf: errorBlock, context).createThrow(of: errorVal)
    default:
      fatalError("unsupported apply")
    }
  }

  moduleContext.buildSpecializedFunction(specializedFunction: specializedFunction) {
      (specializedFunction, specializedContext) in
    var offset = 0
    for spec in argumentSpecializations {
      let arg = specializedFunction.arguments[spec.argumentIndex + offset]
      switch spec.kind {
      case .ownedToGuaranteed:
        specializedContext.erase(instructions: arg.uses.users(ofType: DestroyValueInst.self))
        arg.set(ownership: .guaranteed, specializedContext)
      case .guaranteedToOwned:
        for copy in arg.uses.users(ofType: CopyValueInst.self) {
          copy.replace(with: arg, specializedContext)
        }
        arg.set(ownership: .owned, specializedContext)
      case .dead:
        specializedFunction.entryBlock.eraseArgument(at: arg.index, specializedContext)
        offset -= 1
      case .explode:
        fatalError("todo")
      }
    }
  }
  moduleContext.notifyNewFunction(function: specializedFunction, derivedFrom: function)
}

private extension Function {
  func isCompatibleWithLifetimeDependencies(argumentSpecializations: [ArgumentSpecialization]) -> Bool {
    if convention.hasLifetimeDependencies() {
      for (argIndex, _) in arguments.enumerated() where argIndex >= argumentSpecializations.first!.argumentIndex {
        if argumentConventions.isLifetimeSourceOrTarget(index: argIndex) {
          return false
        }
      }
    }
    return true
  }
}

private extension ParameterInfo {
  func with(convention newConvention: ArgumentConvention) -> ParameterInfo {
    ParameterInfo(type: type, convention: newConvention, options: options, hasLoweredAddresses: hasLoweredAddresses)
  }
}
