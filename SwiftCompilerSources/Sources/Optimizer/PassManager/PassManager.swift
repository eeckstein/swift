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

  private var pipelineStages: [String] = []

  private var scheduledFunctionPasses: [FunctionPass] = []

  private var worklist = FunctionWorklist()

  private var currentPassIndex = 0

  private init(bridged: BridgedPassManager) {
    self._bridged = bridged
  }

  func runModulePasses(passes: [ModulePass]) {
    let context = createModulePassContext()
    for pass in passes {
      pass.runFunction(context)
    }
    if !scheduledFunctionPasses.isEmpty {
      runScheduledFunctionPasses(context)
      scheduledFunctionPasses = []
    }
  }

  func runScheduledFunctionPasses(_ context: ModulePassContext) {
    worklist.initialize(context)
    defer { worklist.clear() }

    while let (f, passIdx) = worklist.readyList.last {
      let endPassIdx = runFunctionPasses(on: f, initialPassIndex: passIdx, context)
      worklist.readyList[worklist.readyList.count - 1].passIndex = endPassIdx
      if endPassIdx == scheduledFunctionPasses.count {
        worklist.popLastFromReadyList()
      }
    }
  }

  func runFunctionPasses(on function: Function, initialPassIndex: Int, _ context: ModulePassContext) -> Int {
    let readyListSize = worklist.readyList.count
    let numPasses = scheduledFunctionPasses.count

    for passIdx in initialPassIndex..<numPasses {
      let pass = scheduledFunctionPasses[passIdx]
      context.transform(function: function) {
        pass.runFunction(function, $0)
      }
      if worklist.readyList.count > readyListSize {
        return passIdx + 1
      }
    }
    return numPasses
  }

  func notifyNewFunction(function: Function, referencedFrom: Function) {
    assert(worklist.unhandledCallees[function] == nil, "not a new function")
    worklist.readyList.append((function, 0))
  }

  func scheduleFunctionPassesForRunning(passes: [FunctionPass]) {
    precondition(scheduledFunctionPasses.isEmpty, "function passes not cleared")
    scheduledFunctionPasses = passes
  }

  var bridgedContext: BridgedPassContext { _bridged.getContext() }

  func beginPipelineStage(name: String) {
    pipelineStages.append(name)
  }

  func endPipelineStage(name: String) {
    let current = pipelineStages.removeLast()
    precondition(current == name)
  }

  static func register() {
    BridgedPassManager.registerBridging(
      // executePassesFn
      { (bridgedPM: BridgedPassManager, bridgedPipelineKind: BridgedPassManager.PassPipelineKind) in
        let pm = PassManager(bridged: bridgedPM)
        let options = Options(_bridged: bridgedPM.getContext())
        let pipeline = getPassPipeline(ofKind: bridgedPipelineKind, options: options)
        pm.runModulePasses(passes: pipeline)
      }
    )
  }
}

private func getPassPipeline(ofKind kind: BridgedPassManager.PassPipelineKind, options: Options) -> [ModulePass] {
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

func functionPasses(@FunctionPassPipelineBuilder _ passes: () -> [FunctionPass]) -> [FunctionPass] {
  passes()
}

private struct FunctionWorklist {

  var functionUses = FunctionUses()
  var unhandledCallees = Dictionary<Function, Int>()
  var readyList: [(Function, passIndex: Int)] = []

  mutating func initialize(_ context: ModulePassContext) {
    let calleeAnalysis = context.calleeAnalysis

    var onStack = Set<Function>()

    var functionCount = 0
    for f in context.functions {
      visit(f, &onStack, calleeAnalysis)
      functionCount += 1
    }

    precondition(unhandledCallees.count == functionCount, "not all functions added to worklist")
  }

  mutating func clear() {
    assert(readyList.isEmpty, "not all functions handled")
    functionUses.clear()
    unhandledCallees.removeAll(keepingCapacity: true)
  }

  private mutating func visit(_ f: Function, _ onStack: inout Set<Function>, _ calleeAnalysis: CalleeAnalysis) {
    guard unhandledCallees[f] == nil else {
      return
    }

    onStack.insert(f)

    var numCallees = 0
    for inst in f.instructions {
      switch inst {
      case let fas as FullApplySite:
        for callee in calleeAnalysis.getIncompleteCallees(callee: fas.callee) {
          if !onStack.contains(callee) {
            functionUses.addUse(of: callee, by: inst)
            numCallees += 1
            visit(callee, &onStack, calleeAnalysis)
          }
        }
      default:
        // TODO: handle builtin.once and destructors
        break
      }
    }
    unhandledCallees[f] = numCallees
    if numCallees == 0 {
      readyList.append((f, 0))
    }
    onStack.remove(f)
  }

  mutating func popLastFromReadyList() {
    let (f, _) = readyList.removeLast()
    for use in functionUses.getUses(of: f) {
      let caller = use.parentFunction
      if unhandledCallees[caller, default: 0].decrementAndCheckForZero() {
        readyList.append((caller, 0))
      }
    }
  }

}

private extension Int {
  mutating func decrementAndCheckForZero() -> Bool {
    assert(self > 0)
    self -= 1
    return self == 0
  }
}
