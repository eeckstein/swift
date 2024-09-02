//===--- PassRegistration.swift - Register optimization passes -------------===//
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

import SIL
import OptimizerBridging

@_cdecl("initializeSwiftModules")
public func initializeSwiftModules() {
  registerSILClasses()
  registerSwiftAnalyses()
  registerUtilities()
  PassManager.register()
  registerSwiftPasses()
  BridgedPassManager.registerBridgedPasses()
  registerOptimizerTests()
}

protocol SILCombineSimplifyable : Instruction {
  func simplify(_ context: SimplifyContext)
}

private func run<InstType: SILCombineSimplifyable>(_ instType: InstType.Type,
                                                   _ bridgedCtxt: BridgedInstructionPassCtxt) {
  let inst = bridgedCtxt.instruction.getAs(instType)
  let context = SimplifyContext(_bridged: bridgedCtxt.passContext,
                                notifyInstructionChanged: {inst in},
                                preserveDebugInfo: false)
  inst.simplify(context)
}

private func registerForSILCombine<InstType: SILCombineSimplifyable>(
      _ instType: InstType.Type,
      _ runFn: @escaping (@convention(c) (BridgedInstructionPassCtxt) -> ())) {
  String(describing: instType)._withBridgedStringRef { instClassStr in
    SILCombine_registerInstructionPass(instClassStr, runFn)
  }
}

private func registerSwiftPasses() {
  // Module passes
  PassManager.register(modulePass: mandatoryPerformanceOptimizations)
  PassManager.register(modulePass: readOnlyGlobalVariablesPass)
  PassManager.register(modulePass: stackProtection)
  PassManager.register(modulePass: asyncDemotion)

  // Function passes
  PassManager.register(functionPass: allocVectorLowering)
  PassManager.register(functionPass: booleanLiteralFolding)
  PassManager.register(functionPass: letPropertyLowering)
  PassManager.register(functionPass: mergeCondFails)
  PassManager.register(functionPass: computeEscapeEffects)
  PassManager.register(functionPass: computeSideEffects)
  PassManager.register(functionPass: initializeStaticGlobals)
  PassManager.register(functionPass: objCBridgingOptimization)
  PassManager.register(functionPass: objectOutliner)
  PassManager.register(functionPass: stackPromotion)
  PassManager.register(functionPass: functionStackProtection)
  PassManager.register(functionPass: simplification)
  PassManager.register(functionPass: ononeSimplification)
  PassManager.register(functionPass: lateOnoneSimplification)
  PassManager.register(functionPass: assumeSingleThreaded)
  PassManager.register(functionPass: releaseDevirtualizer)
  PassManager.register(functionPass: cleanupDebugSteps)
  PassManager.register(functionPass: namedReturnValueOptimization)
  PassManager.register(functionPass: stripObjectHeaders)
  PassManager.register(functionPass: deadStoreElimination)
  PassManager.register(functionPass: redundantLoadElimination)
  PassManager.register(functionPass: earlyRedundantLoadElimination)
  PassManager.register(functionPass: deinitDevirtualizer)
  PassManager.register(functionPass: lifetimeDependenceDiagnosticsPass)
  PassManager.register(functionPass: lifetimeDependenceInsertionPass)
  PassManager.register(functionPass: lifetimeDependenceScopeFixupPass)
  PassManager.register(functionPass: experimentalSwiftBasedClosureSpecialization)
  PassManager.register(functionPass: autodiffClosureSpecialization)

  // Instruction passes
  registerForSILCombine(BeginCOWMutationInst.self, { run(BeginCOWMutationInst.self, $0) })
  registerForSILCombine(GlobalValueInst.self,      { run(GlobalValueInst.self, $0) })
  registerForSILCombine(StrongRetainInst.self,     { run(StrongRetainInst.self, $0) })
  registerForSILCombine(StrongReleaseInst.self,    { run(StrongReleaseInst.self, $0) })
  registerForSILCombine(RetainValueInst.self,      { run(RetainValueInst.self, $0) })
  registerForSILCombine(ReleaseValueInst.self,     { run(ReleaseValueInst.self, $0) })
  registerForSILCombine(LoadInst.self,             { run(LoadInst.self, $0) })
  registerForSILCombine(CopyValueInst.self,        { run(CopyValueInst.self, $0) })
  registerForSILCombine(DestroyValueInst.self,     { run(DestroyValueInst.self, $0) })
  registerForSILCombine(DestructureStructInst.self, { run(DestructureStructInst.self, $0) })
  registerForSILCombine(DestructureTupleInst.self, { run(DestructureTupleInst.self, $0) })
  registerForSILCombine(TypeValueInst.self, { run(TypeValueInst.self, $0) })

  // Test passes
  PassManager.register(modulePass: functionUsesDumper)
  PassManager.register(modulePass: runUnitTests)
  PassManager.register(functionPass: silPrinterPass)
  PassManager.register(functionPass: aliasInfoDumper)
  PassManager.register(functionPass: escapeInfoDumper)
  PassManager.register(functionPass: addressEscapeInfoDumper)
  PassManager.register(functionPass: accessDumper)
  PassManager.register(functionPass: deadEndBlockDumper)
  PassManager.register(functionPass: memBehaviorDumper)
  PassManager.register(functionPass: rangeDumper)
  PassManager.register(functionPass: testInstructionIteration)
  PassManager.register(functionPass: updateBorrowedFromPass)
}

private func registerSwiftAnalyses() {
  AliasAnalysis.register()
  CalleeAnalysis.register()
}

private func registerUtilities() {
  registerVerifier()
  registerBorrowedFromUpdater()
}
