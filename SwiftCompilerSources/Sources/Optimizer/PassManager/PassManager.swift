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

import OptimizerBridging

final class PassManager {

  private let bridged: BridgedPassManager

  private init(bridged: BridgedPassManager) {
    self.bridged = bridged
  }

  func run(pipeline: [ModulePass], _ context: ModulePassContext) {
    // TODO
  }

  var bridgedContext: BridgedPassContext { bridged.getContext() }

  static func register() {
    BridgedPassManager.registerBridging(
      // executePassesFn
      { (bridgedPM: BridgedPassManager, bridgedPipelineKind: BridgedPassManager.PassPipelineKind) in
        let pm = PassManager(bridged: bridgedPM)
        let context = ModulePassContext(_bridged: pm.bridgedContext)
        let pipeline = getPassPipeline(ofKind: bridgedPipelineKind, options: context.options)
        pm.run(pipeline: pipeline, context)
      }
    )
  }
}

private func getPassPipeline(ofKind kind: BridgedPassManager.PassPipelineKind, options: Options) -> [ModulePass] {
  switch kind {
    case .SILGen:              return getSILGenPassPipeline(options: options)
    case .Diagnostic:          return getDiagnosticPassPipeline(options: options)
    case .OwnershipEliminator: return getOwnershipEliminatorPassPipeline(options: options)
    case .Performance:         return getPerformancePassPipeline(options: options)
    case .Onone:               return getOnonePassPipeline(options: options)
    case .InstCount:           return getInstCountPassPipeline(options: options)
    case .Lowering:            return getLoweringPassPipeline(options: options)
    case .IRGenPrepare:        return getIRGenPreparePassPipeline(options: options)
    case .SerializeSIL:        return getSerializeSILPassPipeline(options: options)
    case .FromFile:            return getFromFilePassPipeline(options: options)
    default:
      fatalError("unknown pass pipeline kind")
  }
}

@resultBuilder
struct FunctionPassPipelineBuilder {
  typealias Component = [FunctionPass]
  typealias Expression = FunctionPass

  static func buildExpression(_ element: Expression) -> Component {
    return [element]
  }
  static func buildExpression(_ element: Int) -> Component {
    return []
  }
  static func buildOptional(_ component: Component?) -> Component {
    guard let component = component else { return [] }
    return component
  }
  static func buildEither(first component: Component) -> Component {
    return component
  }
  static func buildEither(second component: Component) -> Component {
    return component
  }
  static func buildArray(_ components: [Component]) -> Component {
    return Array(components.joined())
  }
  static func buildBlock(_ components: Component...) -> Component {
    return Array(components.joined())
  }
}

@resultBuilder
struct ModulePassPipelineBuilder {
  typealias Component = [ModulePass]
  typealias Expression = ModulePass

  static func buildExpression(_ element: ModulePass) -> Component {
    return [element]
  }
  static func buildExpression(_ element: [FunctionPass]) -> Component {
    let pass = ModulePass(name: "function passes") {
      runFunctionPasses(passes: element, $0)
    }
    return [pass]
  }
  static func buildOptional(_ component: Component?) -> Component {
    guard let component = component else { return [] }
    return component
  }
  static func buildEither(first component: Component) -> Component {
    return component
  }
  static func buildEither(second component: Component) -> Component {
    return component
  }
  static func buildArray(_ components: [Component]) -> Component {
    return Array(components.joined())
  }
  static func buildBlock(_ components: Component...) -> Component {
    return Array(components.joined())
  }
}

func passPipeline(@FunctionPassPipelineBuilder _ passes: () -> [FunctionPass]) -> [FunctionPass] {
  passes()
}

func passPipeline(@ModulePassPipelineBuilder _ passes: () -> [ModulePass]) -> [ModulePass] {
  passes()
}

func runFunctionPasses(passes: [FunctionPass], _ context: ModulePassContext) {

}
