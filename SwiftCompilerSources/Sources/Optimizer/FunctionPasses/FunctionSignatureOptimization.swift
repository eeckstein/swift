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

  for function in moduleContext.functions {
    for inst in function.instructions {
      if let apply = inst as? FullApplySite {
      }
    }
  }
}
/*
private func specialize(apply: FullApplySite, _ context: FunctionPassContext) {
  
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
                        _ context: FunctionPassContext
) -> Function {
  // If a function has lifetime dependencies, bailout if dead arguments precede lifetime sources or targets
  if callee.convention.hasLifetimeDependencies() {
    for (argIndex, _) in callee.arguments.enumerated() where argIndex >= deadArgIndices.first!.argumentIndex {
      if callee.argumentConventions.isLifetimeSourceOrTarget(index: argIndex) {
        return
      }
    }
  }

  let specializedFuncName = context.mangle(withSignatureSpecializedArguments: argumentSpecializations, from: function)

  if let existingSpecializedFunction = context.lookupFunction(name: specializedFuncName) {
    return existingSpecializedFunction
  }

  let specializedFunction =
    context.createSpecializedFunctionDeclaration(
      from: callee, withName: specializedFunctionName,
      withParams: specializedParameters,
      makeBare: true)

  context.buildSpecializedFunction(
    specializedFunction: specializedFunction,
    buildFn: { (specializedFunction, specializedContext) in
      var cloner = Cloner(cloneToEmptyFunction: specializedFunction, specializedContext)
      defer { cloner.deinitialize() }

      cloneAndSpecializeFunctionBody(using: &cloner)
      // Cloning a whole function, even if it contains an `unreachable`, doesn't require lifetime completion.
      specializedContext.setNeedCompleteLifetimes(to: false)
    })

  context.notifyNewFunction(function: specializedFunction, derivedFrom: callee)

  return specializedFunction

}
*/
