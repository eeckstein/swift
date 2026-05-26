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

import AST
import SIL

let functionSignatureOptimization = ModulePass(name: "function-signature-optimization") {
  (moduleContext: ModulePassContext) in

  var functionSpecializations = Dictionary<Function, [ArgumentSpecialization]>()

  for function in moduleContext.functions {
    var changed: Bool
    repeat {
      changed = false
      for inst in function.instructions {
        switch inst {
        case let apply as ApplyInst:
          if trySpecialize(apply: apply, cacheIn: &functionSpecializations, moduleContext) {
            changed = true
          }
        case let tryApply as TryApplyInst:
          if trySpecialize(apply: tryApply, cacheIn: &functionSpecializations, moduleContext) {
            changed = true
          }
        default:
          break
        }
      }
    } while changed
  }
}

private func trySpecialize(apply: FullApplySite,
                           cacheIn functionSpecializations: inout Dictionary<Function, [ArgumentSpecialization]>,
                           _ moduleContext: ModulePassContext) -> Bool {
  guard let callee = apply.referencedFunction,
        callee.isDefinition,
        callee.blocks.contains(where: { $0.terminator.isFunctionExiting })
  else {
    return false
  }

  if callee.convention.hasLifetimeDependencies() {
    return false
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
  guard benefit else {
    return false
  }

  specialize(function: callee, with: specializations, callerSiteApply: apply, moduleContext)
  moduleContext.transform(function: apply.parentFunction) { context in
    context.inlineFunction(apply: apply, mandatoryInline: false)
  }
  return true
}

private func getParameterSpecializations(for function: Function,
                                         _ context: FunctionPassContext
) -> [ArgumentSpecialization] {
  var specializations = [ArgumentSpecialization]()
  for (argIndex, arg) in function.arguments.enumerated() {
    if function.argumentConventions.parameterIndex(ofArgumentIndex: argIndex) == nil {
      continue
    }
    if let specKind = getSpecializationKind(for: arg, context) {
      specializations.append(ArgumentSpecialization(argumentIndex: argIndex, kind: specKind))
    }
  }
  return specializations
}

private func getSpecializationKind(for argument: FunctionArgument,
                                   _ context: FunctionPassContext
) -> ArgumentSpecialization.Kind? {
  if argument.type.isMetatype, argument.type.representationOfMetatype == .thin {
    return nil
  }
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
    if argument.isPartiallyUsed(context) {
      return .explode
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

    for copy in uses.users(ofType: CopyValueInst.self) {
      if entryToCopies.inclusiveRangeContains(copy.parentBlock) {
        return false
      }
      entryToCopies.insert(copy.parentBlock)
    }

    guard entryToCopies.exits.isEmpty else {
      return false
    }

    guard uses.users(ofType: CopyValueInst.self).allSatisfy({ !entryToCopies.contains($0.parentBlock) }) else {
      return false
    }

    return true
  }

  func isPartiallyUsed(_ context: FunctionPassContext) -> Bool {
    var usedFields = Stack<Int>(context)
    defer { usedFields.deinitialize() }
    var numUsedFields = 0

    var worklist = OperandWorklist(context)
    defer { worklist.deinitialize() }

    var structType: Type? = nil

    worklist.pushIfNotVisited(contentsOf: uses)

    while let use = worklist.pop() {
      switch use.instruction {
      case let structExtract as StructExtractInst:
        guard let fields = use.value.type.getNominalFields(in: parentFunction) else {
          return false
        }
        if fields.count == 1 {
          worklist.pushIfNotVisited(contentsOf: structExtract.uses)
        } else {
          if let structType = structType {
            assert(structType == use.value.type)
          } else {
            structType = use.value.type
          }
          if !usedFields.contains(structExtract.fieldIndex) {
            usedFields.append(structExtract.fieldIndex)
            numUsedFields += 1
          }
        }
      case is DebugValueInst:
        break
      default:
        return false
      }
    }
    guard let structType else {
      return false
    }
    if numUsedFields > 2 {
      return false
    }
    for (fieldIdx, field) in structType.getNominalFields(in: parentFunction)!.enumerated() {
      if !usedFields.contains(fieldIdx), !field.isTrivial(in: parentFunction) {
        return true
      }
    }
    return false
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
      switch self.value {
      case is CopyValueInst:
        return true
      default:
        return false
      }
    }
  }
}

private extension Value {
  func isDestroyed(after beginInstruction: Instruction, _ context: FunctionPassContext) -> Bool {
    guard !uses.users(ofType: DestroyValueInst.self).isEmpty,
          uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst })
    else {
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
    let origParamIdx = argumentConventions.parameterIndex(ofArgumentIndex: spec.argumentIndex)!
    let paramIdx = origParamIdx + offset

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
      let toExplode = specializedParams.remove(at: paramIdx)
      for field in toExplode.type.nominal!.storedProperties {
        let fieldType = toExplode.type.getTypeOf(member: field).canonical
        let pi = ParameterInfo(type: fieldType,
                               convention: fieldType.isTrivial(in: function) ? .directUnowned : .directGuaranteed,
                               options: toExplode.options,
                               hasLoweredAddresses: toExplode.hasLoweredAddresses)
        specializedParams.insert(pi, at: origParamIdx + offset)
        offset += 1
      }
      offset -= 1
    }
  }

  // We are removing arguments from the original function. If the removed argument is the
  // "self" argument, the specialized function cannot be a "method" anymore. It's in general
  // safe to make it a "thin" function (even if "self" was not removed).
  let representation = function.loweredFunctionType.functionTypeRepresentation
  let specializedRepresentation = representation == .method || representation == .witnessMethod ? .thin : representation

  let convention = FunctionConvention(for: function.loweredFunctionType,
                                      hasLoweredAddresses: moduleContext.moduleHasLoweredAddresses)

  let isGeneric = specializedParams.contains { $0.type.hasTypeParameter } ||
                  convention.resultsWithError.contains { $0.type.hasTypeParameter } ||
                  convention.errorResult?.type.hasTypeParameter ?? false ||
                  (function.isGeneric && function.implicitlyUsesGenericParameter)

  let specializedFunction = moduleContext.createSpecializedFunctionDeclaration(
      from: function, withName: specializedFuncName,
      withParams: specializedParams,
      withRepresentation: specializedRepresentation,
      preserveGenericSignature: isGeneric)

  moduleContext.moveFunctionBody(from: function, to: specializedFunction)

  moduleContext.transform(function: function) { context in
    let newEntryBlock = function.appendNewBlock(context)
    var newApplyArgs = [Value]()
    for origArg in specializedFunction.arguments {
      newApplyArgs.append(newEntryBlock.addFunctionArgument(type: origArg.type, context))
    }
    var toCleanup = [Value]()
    var offset = 0
    for spec in argumentSpecializations {
      let argIdx = spec.argumentIndex + offset
      switch spec.kind {
      case .ownedToGuaranteed:
        toCleanup.append(newApplyArgs[argIdx])
      case .guaranteedToOwned:
        let builder = Builder(atEndOf: newEntryBlock, location: function.location, context)
        let copy = builder.createCopyValue(operand: newApplyArgs[argIdx])
        newApplyArgs[argIdx] = copy
      case .dead:
        newApplyArgs.remove(at: argIdx)
        offset -= 1
      case .explode:
        let original = newApplyArgs.remove(at: argIdx)
        let builder = Builder(atEndOf: newEntryBlock, location: function.location, context)
        let borrow = builder.createBeginBorrow(of: original)
        let destructure = builder.createDestructureStruct(struct: borrow)
        toCleanup.append(borrow)
        for field in destructure.results {
          newApplyArgs.insert(field, at: spec.argumentIndex + offset)
          offset += 1
        }
        offset -= 1
      }
    }

    let builder = Builder(atEndOf: newEntryBlock, location: function.location, context)
    let fri = builder.createFunctionRef(specializedFunction)

    let newApplySite: Instruction

    switch callerSiteApply {
    case let applyInst as ApplyInst:
      let newApply = builder.createApply(function: fri,
                                         isGeneric ? function.forwardingSubstitutionMap : SubstitutionMap(),
                                         arguments: newApplyArgs,
                                         isNonThrowing: applyInst.isNonThrowing,
                                         isNonAsync: applyInst.isNonAsync)

      // TODO: handle return_borrow
      builder.createReturn(of: newApply)
      newApplySite = newApply

    case let tryApply as TryApplyInst:
      let normalBlock = function.appendNewBlock(context)
      let errorBlock = function.appendNewBlock(context)
      newApplySite = builder.createTryApply(function: fri,
                                             isGeneric ? function.forwardingSubstitutionMap : SubstitutionMap(),
                                             arguments: newApplyArgs,
                                             normalBlock: normalBlock, errorBlock: errorBlock,
                                             isNonAsync: tryApply.isNonAsync)

      let retTy = function.mapTypeIntoEnvironment(specializedFunction.resultType)
      let returnVal = normalBlock.addArgument(type: retTy,
                                              ownership: tryApply.normalBlock.arguments[0].ownership,
                                              context)
      Builder(atEndOf: normalBlock, location: newApplySite.location, context).createReturn(of: returnVal)

      if tryApply.errorBlock.arguments.isEmpty {
        Builder(atEndOf: errorBlock, location: newApplySite.location, context).createThrowAddr()
      } else {
        let errorInterfaceTy = function.convention.errorResult!.getReturnValueType(function: function)
        let errorTy = function.mapTypeIntoEnvironment(errorInterfaceTy.rawType)
        let errorVal = errorBlock.addArgument(type: errorTy.loweredType(in: function),
                                              ownership: tryApply.errorBlock.arguments[0].ownership,
                                              context)
        Builder(atEndOf: errorBlock, location: newApplySite.location, context).createThrow(of: errorVal)
      }
    default:
      fatalError("unsupported apply")
    }
    Builder.insert(after: newApplySite, context) { builder in
      for v in toCleanup {
        if v is BeginBorrowInst {
          builder.createEndBorrow(of: v)
        } else {
          builder.createDestroyValue(operand: v)
        }
      }
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

        for dv in arg.uses.users(ofType: DebugValueInst.self) {
          let firstInst = specializedFunction.instructions.first!
          if dv != firstInst {
            dv.move(before: firstInst, specializedContext)
          }
        }
      case .dead:
        assert(arg.users.allSatisfy { $0 is DebugValueInst })
        specializedContext.erase(instructions: arg.users)
        specializedFunction.entryBlock.eraseArgument(at: arg.index, specializedContext)
        offset -= 1
      case .explode:
        var elements = [Value]()
        for field in arg.type.getNominalFields(in: function)! {
          let fieldArg = specializedFunction.entryBlock.insertFunctionArgument(
            atPosition: spec.argumentIndex + offset + 1,
            type: field,
            ownership: field.isTrivial(in: specializedFunction) ? .none : .guaranteed,
            specializedContext)
          elements.append(fieldArg)
          offset += 1
        }
        for user in arg.users {
          switch user {
          case is DebugValueInst:
            specializedContext.erase(instruction: user)
          case let structExtract as StructExtractInst:
            structExtract.replace(with: elements[structExtract.fieldIndex], specializedContext)
          default:
            fatalError("unknown argument use")
          }
        }
        specializedFunction.entryBlock.eraseArgument(at: arg.index, specializedContext)
        offset -= 1
      }
    }
  }
  moduleContext.notifyNewFunction(function: specializedFunction, derivedFrom: function)
}

private extension ParameterInfo {
  func with(convention newConvention: ArgumentConvention) -> ParameterInfo {
    ParameterInfo(type: type, convention: newConvention, options: options, hasLoweredAddresses: hasLoweredAddresses)
  }
}

private extension Function {
  var implicitlyUsesGenericParameter: Bool {
    for inst in instructions {
      if inst.results.contains(where: { $0.type.hasPrimaryArchetype }) {
        return true
      }
      switch inst {
      case let apply as ApplySite:
        if apply.substitutionMap.usesGenericParameter {
          return true
        }
      case let builtin as BuiltinInst:
        if builtin.substitutionMap.usesGenericParameter {
          return true
        }
      default:
        break
      }
    }
    return false
  }
}

private extension SubstitutionMap {
  var usesGenericParameter: Bool {
    return replacementTypes.contains { $0.hasPrimaryArchetype }
  }
}
