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

  var functionSpecializations = Dictionary<Function, FunctionSpecialization>()

  for function in moduleContext.functions {
    guard function.hasOwnership,
          function.shouldOptimize
    else {
      continue
    }

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
                           cacheIn functionSpecializations: inout Dictionary<Function, FunctionSpecialization>,
                           _ moduleContext: ModulePassContext) -> Bool {
  guard let callee = apply.referencedFunction,
        callee.hasOwnership,
        callee.shouldOptimize,
        callee.isDefinition,
        callee.blocks.contains(where: { $0.terminator.isFunctionExiting })
  else {
    return false
  }

  if callee.convention.hasLifetimeDependencies() {
    return false
  }

  let specialization: FunctionSpecialization
  if let existingSpecialization = functionSpecializations[callee] {
    specialization = existingSpecialization
  } else {
    specialization = moduleContext.transform(function: callee) { context in
      getSpecialization(for: callee, context)
    }
    functionSpecializations[callee] = specialization
  }
  if specialization.isEmpty {
    return false
  }

  let benefit = moduleContext.transform(function: apply.parentFunction) { context in
    apply.canBenefit(from: specialization, context)
  }
  guard benefit else {
    return false
  }

  let specializedFunction = specialize(function: callee, with: specialization, callerSiteApply: apply, moduleContext)
  moduleContext.transform(function: apply.parentFunction) { context in
    if specialization.resultOwnedToGuaranteed {
      let applyInst = apply as! ApplyInst
      let builder = Builder(before: applyInst, context)
      let calleeRef = builder.createFunctionRef(specializedFunction)
      let newApply = builder.createApply(function: calleeRef, applyInst.substitutionMap,
                                         arguments: Array(applyInst.arguments),
                                         isNonThrowing: applyInst.isNonThrowing,
                                         isNonAsync: applyInst.isNonAsync)
      context.erase(instructions: applyInst.uses.filter(usersOfType: DestroyValueInst.self).users)
      applyInst.replace(with: newApply, context)
    } else {
      context.inlineFunction(apply: apply, mandatoryInline: false)
    }
  }
  return true
}

private func getSpecialization(for function: Function,
                               _ context: FunctionPassContext) -> FunctionSpecialization {
  var specialization = FunctionSpecialization()
  specialization.arguments = getParameterSpecializations(for: function, context)

  // TODO: combine the result conversion with argument specializations. Currently they can
  // interfere with each other: e.g. a `guaranteedToOwned` argument can be the borrow source of
  // the returned value, and self-recursive calls can only be re-directed to the specialized
  // function if the argument list is unchanged.
  if specialization.arguments.isEmpty {
    specialization.resultBorrowedFromArguments = getResultBorrowSources(of: function, context)
  }
  return specialization
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
    // TODO: should we also explode partially dead trivial structs?
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
      // TODO: should we also explode partially dead trivial structs?
      if !usedFields.contains(fieldIdx), !field.isTrivial(in: parentFunction) {
        return true
      }
    }
    return false
  }
}

//===----------------------------------------------------------------------===//
//                    Owned -> guaranteed result conversion
//===----------------------------------------------------------------------===//

/// Returns the indices of the arguments from whose memory - or value - the single direct `@owned`
/// result of `function` is borrowed. If the result is empty, the result cannot be returned
/// `@guaranteed`.
///
/// The result can be returned `@guaranteed` - i.e. without the callee retaining it - if the returned
/// value is, on all paths, borrowed from something which the caller keeps alive anyway, e.g. loaded
/// from an indirect argument:
/// ```
///   sil @f : $(@inout Array<Int>) -> @owned Array<Int> {
///   bb0(%0 : $*Array<Int>):
///     %1 = load [copy] %0
///     return %1
/// ```
/// ->
/// ```
///   sil @f : $(@inout Array<Int>) -> @guaranteed Array<Int> {
///   bb0(%0 : $*Array<Int>):
///     %1 = load_borrow %0
///     return_borrow %1 from_scopes (%1)
/// ```
/// The returned argument indices are the contract for callers: the borrow is only valid as long as
/// the memory of those arguments is not modified (see `borrowedResultStaysValid`).
private func getResultBorrowSources(of function: Function, _ context: FunctionPassContext) -> [Int] {
  guard let returnInst = function.returnInstruction as? ReturnInst,
        returnInst.returnedValue.ownership == .owned
  else {
    return []
  }

  var borrowingInstructions = Stack<SingleValueInstruction>(context)
  defer { borrowingInstructions.deinitialize() }

  guard collectBorrowingInstructions(of: returnInst.returnedValue, in: &borrowingInstructions, context) else {
    return []
  }

  var borrowedArguments = IterableArgumentSet(context)
  defer { borrowedArguments.deinitialize() }

  guard collectBorrowedArguments(of: borrowingInstructions, in: &borrowedArguments, context),
        !mayModifyMemory(after: borrowingInstructions, borrowedArguments: borrowedArguments, context) else {
    return []
  }

  return borrowedArguments.map { $0.index }
}

private func collectBorrowingInstructions(of returnedValue: Value,
                                          in results: inout Stack<SingleValueInstruction>,
                                          _ context: FunctionPassContext) -> Bool
{
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(returnedValue)
  while let value = worklist.pop() {
    switch value {
    case let load as LoadInst where load.loadOwnership == .copy:
      results.append(load)

    case let copy as CopyValueInst:
      results.append(copy)

    case let apply as ApplyInst where apply.referencedFunction == apply.parentFunction:
      results.append(apply)

    case let argument as Argument:
      guard let phi = Phi(argument) else {
        return false
      }
      worklist.pushIfNotVisited(contentsOf: phi.incomingValues)

    default:
      return false
    }
  }
  return true
}

private func collectBorrowedArguments(of borrowingInstructions: Stack<SingleValueInstruction>,
                                      in borrowedArguments: inout IterableArgumentSet,
                                      _ context: FunctionPassContext) -> Bool
{
  for borrowingInst in borrowingInstructions {
    switch borrowingInst {
    case let load as LoadInst:
      guard let argument = load.address.baseArgumentOfAddress else {
        return false
      }
      borrowedArguments.insert(argument)

    case let copy as CopyValueInst:
      if borrowedArguments.insertBorrowIntroducers(of: copy.fromValue, context) == .unknownBorrowIntroducer {
        return false
      }

    default:
      break
    }
  }

  var argumentAdded: Bool

  repeat {
    argumentAdded = false
    for case let apply as ApplyInst in borrowingInstructions {
      for argument in borrowedArguments {
        let argumentValue = apply.operand(forCalleeArgumentIndex: argument.index)!.value
        if argumentValue.type.isAddress {
          guard let callerArgument = argumentValue.baseArgumentOfAddress else {
            return false
          }
          if borrowedArguments.insert(callerArgument) {
            argumentAdded = true
          }
        } else {
          switch borrowedArguments.insertBorrowIntroducers(of: argumentValue, context) {
          case .unknownBorrowIntroducer: return false
          case .insertedNewArgument:     argumentAdded = true
          case .nothingInserted:         break
          }
        }
      }
    }
  } while argumentAdded

  return true
}

private extension IterableArgumentSet {
  enum InsertResult {
    case nothingInserted, insertedNewArgument, unknownBorrowIntroducer
  }

  mutating func insertBorrowIntroducers(of value: Value, _ context: FunctionPassContext) -> InsertResult {
    guard value.ownership == .guaranteed else {
      return .unknownBorrowIntroducer
    }
    var result = InsertResult.nothingInserted
    for introducer in value.getBorrowIntroducers(context) {
      guard let argument = introducer.value as? FunctionArgument else {
        return .unknownBorrowIntroducer
      }
      if insert(argument) {
        result = .insertedNewArgument
      }
    }
    return result
  }
}

private func mayModifyMemory(after borrowingInstructions: Stack<SingleValueInstruction>,
                             borrowedArguments: IterableArgumentSet,
                             _ context: FunctionPassContext) -> Bool
{
  for borrowingInst in borrowingInstructions {
    switch borrowingInst {
    case let load as LoadInst:
      if mayModifyMemory(load.address, after: load, until: nil, context) {
        return true
      }

    case let apply as ApplyInst:
      for argument in borrowedArguments {
        let argumentValue = apply.operand(forCalleeArgumentIndex: argument.index)!.value
        if argumentValue.type.isAddress,
           mayModifyMemory(argumentValue, after: apply, until: nil, context)
        {
          return true
        }
      }

    default:
      break
    }
  }
  return false
}

private extension ApplyInst {
  var canConvertResultFromOwnedToGuaranteed: Bool {
    uses.ignore(usersOfType: DestroyValueInst.self).allSatisfy { $0.canAccept(ownership: .guaranteed) }
  }

  func argumentScopesOverlapReturnLifetime(of argumentIndices: [Int], _ context: FunctionPassContext) -> Bool {
    var returnLifetime = InstructionRange(begin: self, ends: uses.endingLifetime.users, context)
    defer { returnLifetime.deinitialize() }

    for arg in argumentIndices.lazy.map({ self.arguments[$0] }) {
      if arg.type.isAddress {
        if mayModifyMemory(arg, after: self, until: returnLifetime.insertedInstructions, context) {
          return false
        }
      } else {
        switch arg.ownership {
        case .unowned:
          return false
        case .none:
          break
        case .owned:
          if arg.uses.endingLifetime.users.contains(where: { returnLifetime.contains($0) }) {
            return false
          }
        case .guaranteed:
          for borrowIntroducer in arg.getBorrowIntroducers(context) {
            if borrowIntroducer.scopeEndingOperands.users.contains(where: { returnLifetime.contains($0) }) {
              return false
            }
          }
        }
      }
    }
    return true
  }
}

/// Returns true if any instruction from `startInst` (exclusive) to `ends` (exclusive) may write to
/// `address`. If `ends` is nil, all paths to the function exits are checked.
private func mayModifyMemory(_ address: Value, after startInst: Instruction,
                             until ends: InstructionSet?,
                             _ context: FunctionPassContext) -> Bool {
  let aliasAnalysis = context.aliasAnalysis
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushSuccessors(of: startInst)

  while let inst = worklist.pop() {
    if let ends, ends.contains(inst) {
      continue
    }
    if inst.mayWrite(toAddress: address, aliasAnalysis) {
      return true
    }
    worklist.pushSuccessors(of: inst)
  }
  return false
}

private extension LoadInst {
  /// The index of the function argument whose memory this load reads, or nil if the loaded memory
  /// is not owned by the caller.
  var borrowSourceArgumentIndex: Int? {
    switch address.enclosingAccessScope {
    case .access, .dependence:
      // A `load_borrow` must not be used after the end of its enclosing access scope, but the
      // returned borrow escapes to the caller.
      return nil
    case .base(let accessBase):
      if case .argument(let argument) = accessBase {
        return argument.index
      }
      return nil
    }
  }
}

private extension FullApplySite {
  func canBenefit(from specialization: FunctionSpecialization, _ context: FunctionPassContext) -> Bool {
    if specialization.resultOwnedToGuaranteed {
      let applyInst = self as! ApplyInst
      guard applyInst.canConvertResultFromOwnedToGuaranteed,
            applyInst.argumentScopesOverlapReturnLifetime(of: specialization.resultBorrowedFromArguments, context)
      else {
        return false
      }
    }

    for spec in specialization.arguments {
      if let arg = operand(forCalleeArgumentIndex: spec.argumentIndex),
         arg.canBenefit(from: spec.kind, context)
      {
        return true
      }
    }
    if specialization.resultOwnedToGuaranteed,
       let applyInst = self as? ApplyInst,
       // A `@guaranteed` result only helps if the caller doesn't have to own the returned value
       // anyway. If it does, the removed retain in the callee is just moved to the caller.
       !applyInst.uses.endingLifetime.isEmpty,
       applyInst.uses.endingLifetime.allSatisfy({ $0.instruction is DestroyValueInst })
    {
      return true
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

  var baseArgumentOfAddress: Argument? {
    switch enclosingAccessScope {
    case .access, .dependence:
      // A `load_borrow` must not be used after the end of its enclosing access scope, but the
      // returned borrow escapes to the caller.
      return nil
    case .base(let accessBase):
      guard case .argument(let argument) = accessBase else {
        return nil
      }
      return argument
    }
  }

}

private func specialize(function: Function,
                        with specialization: FunctionSpecialization,
                        callerSiteApply: FullApplySite,
                        _ moduleContext: ModulePassContext) -> Function
{
  let argumentSpecializations = specialization.arguments
  let specializedFuncName = moduleContext.mangle(
      withSignatureSpecializedArguments: argumentSpecializations,
      resultOwnedToGuaranteed: specialization.resultOwnedToGuaranteed,
      from: function)

  if let existingFunction = moduleContext.lookupFunction(name: specializedFuncName) {
    return existingFunction
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
                                      hasLoweredAddresses: function.hasLoweredAddresses)

  let isGeneric = specializedParams.contains { $0.type.hasTypeParameter } ||
                  convention.resultsWithError.contains { $0.type.hasTypeParameter } ||
                  convention.errorResult?.type.hasTypeParameter ?? false ||
                  (function.isGeneric && function.implicitlyUsesGenericParameter)

  var specializedResults: [ResultInfo]? = nil
  if specialization.resultOwnedToGuaranteed {
    specializedResults = convention.formalResults.map {
      ResultInfo(type: $0.type, convention: .guaranteed, options: $0.options,
                 hasLoweredAddresses: $0.hasLoweredAddresses)
    }
  }

  let specializedFunction = moduleContext.createSpecializedFunctionDeclaration(
      from: function, withName: specializedFuncName,
      withParams: specializedParams,
      withResults: specializedResults,
      withRepresentation: specializedRepresentation,
      preserveGenericSignature: isGeneric)

  moduleContext.moveFunctionBody(from: function, to: specializedFunction)

  moduleContext.transform(function: function) { context in

    function.set(thunkKind: .signatureOptimizedThunk, context)

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
    // The last instruction of the thunk which the argument cleanups must follow. If the result is
    // returned `@guaranteed`, it must be copied _before_ the arguments are released, because the
    // result's validity may depend on them.
    var insertCleanupsAfter: Instruction

    switch callerSiteApply {
    case let applyInst as ApplyInst:
      let newApply = builder.createApply(function: fri,
                                         isGeneric ? function.forwardingSubstitutionMap : SubstitutionMap(),
                                         arguments: newApplyArgs,
                                         isNonThrowing: applyInst.isNonThrowing,
                                         isNonAsync: applyInst.isNonAsync)

      newApplySite = newApply
      insertCleanupsAfter = newApply
      if specialization.resultOwnedToGuaranteed {
        // The thunk keeps its `@owned` result convention, so it has to take ownership of the
        // borrowed value returned by the specialized function.
        let copy = builder.createCopyValue(operand: newApply)
        insertCleanupsAfter = copy
        builder.createReturn(of: copy)
      } else {
        // TODO: handle return_borrow
        builder.createReturn(of: newApply)
      }

    case let tryApply as TryApplyInst:
      let normalBlock = function.appendNewBlock(context)
      let errorBlock = function.appendNewBlock(context)
      newApplySite = builder.createTryApply(function: fri,
                                             isGeneric ? function.forwardingSubstitutionMap : SubstitutionMap(),
                                             arguments: newApplyArgs,
                                             normalBlock: normalBlock, errorBlock: errorBlock,
                                             isNonAsync: tryApply.isNonAsync)
      insertCleanupsAfter = newApplySite

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
    Builder.insert(after: insertCleanupsAfter, context) { builder in
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
    if specialization.resultOwnedToGuaranteed {
      convertResultToGuaranteed(in: specializedFunction,
                                borrowedFrom: specialization.resultBorrowedFromArguments,
                                specializedContext)
    }
  }
  moduleContext.notifyNewFunction(function: specializedFunction, derivedFrom: function)
  return specializedFunction
}

/// Rewrites the body of `specializedFunction` so that it returns its result as a `@guaranteed`
/// value with a `return_borrow` instead of consuming it with a `return`.
private func convertResultToGuaranteed(in specializedFunction: Function,
                                       borrowedFrom argumentIndices: [Int],
                                       _ context: FunctionPassContext) {
  let returnInst = specializedFunction.returnInstruction as! ReturnInst

  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }

  worklist.pushIfNotVisited(returnInst.returnedValue)
  while let value = worklist.pop() {
    switch value {
    case let load as LoadInst where load.loadOwnership == .copy:
      let builder = Builder(before: load, context)
      let loadBorrow = builder.createLoadBorrow(fromAddress: load.address)
      load.replace(with: loadBorrow, context)

    case let copy as CopyValueInst:
      let builder = Builder(before: copy, context)
      let beginBorrow = builder.createBeginBorrow(of: copy.fromValue)
      copy.replace(with: beginBorrow, context)

    case let apply as ApplyInst:
      let builder = Builder(before: apply, context)
      let calleeRef = builder.createFunctionRef(specializedFunction)
      let newApply = builder.createApply(function: calleeRef, apply.substitutionMap,
                                         arguments: Array(apply.arguments),
                                         isNonThrowing: apply.isNonThrowing,
                                         isNonAsync: apply.isNonAsync)
      let beginBorrow = builder.createBeginBorrow(of: newApply)
      apply.replace(with: beginBorrow, context)

    case let argument as Argument:
      let phi = Phi(argument)!
      worklist.pushIfNotVisited(contentsOf: phi.incomingValues)
      phi.value.set(ownership: .guaranteed, context)
      phi.value.set(reborrow: true, context)

    default:
      fatalError("Unexpected value: \(value)")
    }
  }

  // `borrowed-from` instructions for the new guaranteed phis.
  updateGuaranteedPhis(in: specializedFunction, context)

  let enclosingScopes = Array<Value>(returnInst.returnedValue.getBorrowIntroducers(context).map {
    let value = $0.value
    if let phi = Phi(value), let bf = phi.borrowedFrom {
      return bf
    }
    return value
  })
  let builder = Builder(before: returnInst, context)
  builder.createReturnBorrow(of: returnInst.returnedValue, fromScopes: enclosingScopes)
  context.erase(instruction: returnInst)
}

private extension Value {
  /// True if this value introduces a borrow scope which ends within this function.
  var isLocalBorrowScope: Bool {
    switch self {
    case is BeginBorrowInst, is LoadBorrowInst:
      return true
    default:
      return false
    }
  }
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
      case let typeValue as TypeValueInst:
        if typeValue.paramType.hasPrimaryArchetype {
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
