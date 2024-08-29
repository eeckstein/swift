//===--- PassManager.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL
import OptimizerBridging

final class PassManager {

  let _bridged: BridgedPassManager

  private init(bridged: BridgedPassManager) {
    self._bridged = bridged
  }

  func run(pipeline: ModulePassPipeline, _ context: ModulePassContext) {
    // TODO
  }

  var bridgedContext: BridgedPassContext { _bridged.getContext() }

  static func register() {
    BridgedPassManager.registerBridging(
      // executePassesFn
      { (bridgedPM: BridgedPassManager, bridgedPipelineKind: BridgedPassManager.PassPipelineKind) in
        let pm = PassManager(bridged: bridgedPM)
        let context = ModulePassContext(passManager: pm)
        let pipeline = getPassPipeline(ofKind: bridgedPipelineKind, options: context.options)
        pm.run(pipeline: pipeline, context)
      }
    )
  }
}

private func getPassPipeline(ofKind kind: BridgedPassManager.PassPipelineKind, options: Options) -> ModulePassPipeline {
  switch kind {
    case .SILGen:                        return getSILGenPassPipeline(options: options)
    case .Mandatory:                     return getMandatoryPassPipeline(options: options)
    case .Onone:                         return getOnonePassPipeline(options: options)
    case .Performance:                   return getPerformancePassPipeline(options: options)
    case .IRGenPrepare:                  return getIRGenPreparePassPipeline(options: options)
    case .OwnershipEliminator:           return getOwnershipEliminatorPassPipeline(options: options)
    case .InstCount:                     return getInstCountPassPipeline(options: options)
    case .Lowering:                      return getLoweringPassPipeline(options: options)
    case .SerializeSIL:                  return getSerializeSILPassPipeline(options: options)
    case .FromFile:                      return getFromFilePassPipeline(options: options)
    case .MandatoryDebugSerialization:   return getMandatoryDebugSerializationPassPipeline(options: options)
    case .PerformanceDebugSerialization: return getPerformanceDebugSerializationPassPipeline(options: options)
    default:
      fatalError("unknown pass pipeline kind")
  }
}

@resultBuilder
struct FunctionPassPipelineBuilder {
  static func buildExpression(_ pass: FunctionPass) -> [FunctionPass] {
    return [pass]
  }

  static func buildExpression(_ passKind: BridgedPass) -> [FunctionPass] {
    let pass = FunctionPass(name: "TODO") {
      (function: Function, context: FunctionPassContext) in

      context._bridged.getPassManager().runBridgedFunctionPass(passKind, function.bridged)
    }
    return [pass]
  }

  static func buildExpression(_ passes: [FunctionPass]) -> [FunctionPass] {
    return passes
  }

  static func buildOptional(_ passes: [FunctionPass]?) -> [FunctionPass] {
    return passes ?? []
  }

  static func buildEither(first passes: [FunctionPass]) -> [FunctionPass] {
    return passes
  }

  static func buildEither(second passes: [FunctionPass]) -> [FunctionPass] {
    return passes
  }

  static func buildBlock(_ passes: [FunctionPass]...) -> [FunctionPass] {
    return Array(passes.joined())
  }
}

typealias ModulePassPipeline = (passes: [ModulePass], name: String)

@resultBuilder
struct ModulePassPipelineBuilder {
  static func buildExpression(_ pass: ModulePass) -> [ModulePass] {
    return [pass]
  }

  static func buildExpression(_ passKind: BridgedModulePass) -> [ModulePass] {
    let pass = ModulePass(name: "TODO") {
      (context: ModulePassContext) in

      context._bridged.getPassManager().runBridgedModulePass(passKind)
    }
    return [pass]
  }

  static func buildExpression(_ functionPasses: [FunctionPass]) -> [ModulePass] {
    let pass = ModulePass(name: "function passes") {
      runFunctionPasses(passes: functionPasses, $0)
    }
    return [pass]
  }

  static func buildExpression(_ modulePasses: ModulePassPipeline) -> [ModulePass] {
    let pass = ModulePass(name: modulePasses.name) {
      runModulePasses(passes: modulePasses.passes, $0)
    }
    return [pass]
  }

  static func buildOptional(_ passes: [ModulePass]?) -> [ModulePass] {
    return passes ?? []
  }

  static func buildEither(first passes: [ModulePass]) -> [ModulePass] {
    return passes
  }

  static func buildEither(second passes: [ModulePass]) -> [ModulePass] {
    return passes
  }

  static func buildBlock(_ passes: [ModulePass]...) -> [ModulePass] {
    return Array(passes.joined())
  }
}

func functionPasses(@FunctionPassPipelineBuilder _ passes: () -> [FunctionPass]) -> [FunctionPass] {
  passes()
}

func modulePasses(_ name: String, @ModulePassPipelineBuilder _ passes: () -> [ModulePass]) -> ModulePassPipeline {
  ModulePassPipeline(passes: passes(), name: name)
}

func runFunctionPasses(passes: [FunctionPass], _ context: ModulePassContext) {
  fatalError("TODO")
}

func runModulePasses(passes: [ModulePass], _ context: ModulePassContext) {
  fatalError("TODO")
}
