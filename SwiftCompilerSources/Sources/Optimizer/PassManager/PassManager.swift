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

  private let scheduledModulePasses: [ModulePass]

  private var scheduledFunctionPasses: [FunctionPass] = []

  private var worklist = FunctionWorklist()

  private var completedPasses: CompletedPasses

  /// Stores for each function the number of levels of specializations it is
  /// derived from an original function. E.g. if a function is a signature
  /// optimized specialization of a generic specialization, it has level 2.
  /// This is used to avoid an infinite amount of functions pushed on the
  /// worklist (e.g. caused by a bug in a specializing optimization).
  private var derivationLevels = Dictionary<Function, Int>()

  static let maxDeriveLevels = 10

  private var currentPassIndex = 0
  private var currentSubPassIndex = 0

  private var currentPassMadeChanges = false
  private var currentPassDepdendsOnOtherFunction = false

  private let maxNumPassesToRun: Int
  private let maxNumSubpassesToRun: Int

  private var isMandatory = false

  private var shouldPrintPassNames: Bool
  private var anyPassOptionSet: Bool

  static var passIndices = Dictionary<String, Int>()

  private init(bridged: BridgedPassManager, passPipeline: [ModulePass]) {
    self._bridged = bridged
    self.scheduledModulePasses = passPipeline
    self.completedPasses = CompletedPasses(numPasses: Self.passIndices.count)
    self.maxNumPassesToRun = _bridged.getMaxNumPassesToRun()
    self.maxNumSubpassesToRun = _bridged.getMaxNumSubpassesToRun()
    self.shouldPrintPassNames =  _bridged.shouldPrintPassNames()
    self.anyPassOptionSet = _bridged.anyPassOptionSet()
    bridged.setSwiftPassManager(SwiftObject(self))
  }

  deinit {
    _bridged.setSwiftPassManager(nil)
  }

  func run() {
    let context = createModulePassContext()

    for pass in scheduledModulePasses {
      if !shouldContinueTransforming {
        return
      }
      currentSubPassIndex = 0

      if shouldPrintBefore(pass: pass) {
        print(context)
      }

      pass.name._withBridgedStringRef() {
        _bridged.preModulePassRun($0, currentPassIndex)
      }

      pass.runFunction(context)

      _bridged.postModulePassRun()

      if shouldPrintAfter(pass: pass) {
        print(context)
      } else if currentPassMadeChanges && shouldPrintAnyFunction {
        for f in context.functions {
          if shouldPrint(function: f) {
            print(f)
          }
        }
      }

      currentPassIndex += 1

      if !scheduledFunctionPasses.isEmpty {
        runScheduledFunctionPasses(context)
        scheduledFunctionPasses = []
      }
    }
  }

  private func runScheduledFunctionPasses(_ context: ModulePassContext) {
    derivationLevels.removeAll(keepingCapacity: true)
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

  private func runFunctionPasses(on function: Function, initialPassIndex: Int, _ context: ModulePassContext) -> Int {
    let readyListSize = worklist.readyList.count
    let numPasses = scheduledFunctionPasses.count

    for passIdx in initialPassIndex..<numPasses {
      if !shouldContinueTransforming {
        return numPasses
      }
      context.transform(function: function) {
        runFunctionPass(on: function, passIndex: passIdx, $0)
      }
      currentPassIndex += 1

      if worklist.readyList.count > readyListSize {
        return passIdx + 1
      }
    }
    return numPasses
  }

  private func runFunctionPass(on function: Function, passIndex: Int, _ context: FunctionPassContext) {
    let pass = scheduledFunctionPasses[passIndex]
    if !completedPasses.needToRunPass(passIndex: pass.uniqueIndex, on: function) {
      printPassInfo("(Skipping)", pass.name, passIndex, function)
      return
    }
    if isPassDisabled(pass) {
      printPassInfo("(Disabled)", pass.name, passIndex, function)
      return
    }
    printPassInfo("Run", pass.name, passIndex, function)
    currentSubPassIndex = 0

    if shouldPrintBefore(pass: pass) || isLastPass(passIndex) {
      print(function)
    }

    pass.name._withBridgedStringRef() {
      _bridged.preFunctionPassRun(function.bridged, $0, currentPassIndex)
    }

    pass.runFunction(function, context)

    _bridged.postFunctionPassRun()

    if shouldPrintAfter(pass: pass) ||
       isLastPass(passIndex) ||
       (currentPassMadeChanges && shouldPrint(function: function))
    {
      print(function)
    }
  }

  private var shouldContinueTransforming: Bool {
    if isMandatory {
      return true
    }
    return currentPassIndex < maxNumPassesToRun
  }

  private func printPassInfo(_ title: String, _ passName: String, _ passIndex: Int, _ function: Function?) {
    if !shouldPrintPassNames {
      return
    }
    let pipelineInfo = pipelineStages.last ?? "?"
    let fnInfo: String
    if let function = function {
      fnInfo = ", Function: \(function.name)"
    } else {
      fnInfo = ""
    }
    print("  \(title) #\(currentPassIndex), stage \(pipelineInfo), pass \(passIndex): \(passName)\(fnInfo)")
  }

  func notifyNewFunction(function: Function, derivedFrom: Function?) {
    let newLevel: Int
    if let derivedFrom = derivedFrom {
      newLevel = derivationLevels[derivedFrom, default: 0] + 1
      if newLevel > Self.maxDeriveLevels {
        return
      }
    } else {
      newLevel = 1
    }
    derivationLevels[function] = newLevel
    worklist.pushNewFunctionToReadyList(function)
  }

  func notifyPassMadeChanges() {
    currentPassMadeChanges = true
  }

  func notifyPassDependsOnOtherFunction() {
    currentPassDepdendsOnOtherFunction = true
  }

  func continueWithNextSubpassRun(on function: Function, for inst: Instruction? = nil) -> Bool {
    let subPassIdx = currentSubPassIndex
    currentPassIndex += 1

    if isMandatory {
      return true
    }
    if (currentPassIndex != maxNumPassesToRun - 1) {
      return true
    }

    if isLastSubpass(subPassIdx) {
      print(function)
    }

    return subPassIdx < maxNumSubpassesToRun
  }

  private func isLastPass(_ passIndex: Int) -> Bool {
    return passIndex == maxNumPassesToRun - 1
  }

  private func isLastSubpass(_ subPassIndex: Int) -> Bool {
    return subPassIndex == maxNumSubpassesToRun - 1
  }

  func scheduleFunctionPassesForRunning(passes: [FunctionPass]) {
    precondition(scheduledFunctionPasses.isEmpty, "function passes not cleared")
    scheduledFunctionPasses = passes
  }

  private func isPassDisabled<P: Pass>(_ pass: P) -> Bool {
    guard anyPassOptionSet else {
      return false
    }
    return pass.name._withBridgedStringRef {
      _bridged.isPassDisabled($0)
    }
  }

  private func shouldPrintBefore<P: Pass>(pass: P) -> Bool {
    guard anyPassOptionSet else {
      return false
    }
    return pass.name._withBridgedStringRef {
      _bridged.shouldPrintBefore($0)
    }
  }

  private func shouldPrintAfter<P: Pass>(pass: P) -> Bool {
    guard anyPassOptionSet else {
      return false
    }
    return pass.name._withBridgedStringRef {
      _bridged.shouldPrintAfter($0)
    }
  }

  private var shouldPrintAnyFunction: Bool {
    guard anyPassOptionSet else {
      return false
    }
    return _bridged.shouldPrintAnyFunction()
  }

  private func shouldPrint(function: Function) -> Bool {
    guard anyPassOptionSet else {
      return false
    }
    return _bridged.shouldPrintFunction(function.bridged)
  }

  var bridgedContext: BridgedPassContext { _bridged.getContext() }

  func beginPipelineStage(name: String) {
    pipelineStages.append(name)
  }

  func endPipelineStage(name: String) {
    let current = pipelineStages.removeLast()
    precondition(current == name)
  }

  func setMandatory() {
    isMandatory = true
  }

  static func allocatePassIndex(passName: String) -> Int {
    if let idx = passIndices[passName] {
      return idx
    }
    let idx = passIndices.count
    passIndices[passName] = idx
    assert(passIndices.count == idx + 1)
    return idx
  }

  static func register() {
    BridgedPassManager.registerBridging(
      // executePassesFn
      { (bridgedPM: BridgedPassManager, bridgedPipelineKind: BridgedPassManager.PassPipelineKind) in
        let options = Options(_bridged: bridgedPM.getContext())
        let pipeline = getPassPipeline(ofKind: bridgedPipelineKind, options: options)
        do {
          let pm = PassManager(bridged: bridgedPM, passPipeline: pipeline)
          pm.run()
          assert(bridgedPM.getSwiftPassManager() != nil)
        }
        assert(bridgedPM.getSwiftPassManager() == nil)
      },
      // notifyNewFunctionFn
      {
        (bridgedPM: BridgedPassManager, function: BridgedFunction, derivedFrom: BridgedFunction) in
        let pm = bridgedPM.getSwiftPassManager().getAs(PassManager.self)!
        pm.notifyNewFunction(function: function.function, derivedFrom: derivedFrom.function)
      },
      // continueWithSubpassFn
      {
        (bridgedPM: BridgedPassManager, function: BridgedFunction, inst: OptionalBridgedInstruction) -> Bool in
        let pm = bridgedPM.getSwiftPassManager().getAs(PassManager.self)!
        return pm.continueWithNextSubpassRun(on: function.function, for: inst.instruction)
      },
      // notifyPassHasInvalidatedFn
      {
        (bridgedPM: BridgedPassManager) in
        let pm = bridgedPM.getSwiftPassManager().getAs(PassManager.self)!
        pm.notifyPassMadeChanges()
      },
      // notifyDepdendencyFn
      {
        (bridgedPM: BridgedPassManager) in
        let pm = bridgedPM.getSwiftPassManager().getAs(PassManager.self)!
        pm.notifyPassDependsOnOtherFunction()
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
    let pass = FunctionPass(name: StringRef(bridged: BridgedPassManager.getPassName(passKind)).string) {
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
  // TODO: make this an array, indexed by Function.uniqueIndex
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
      let callees: FunctionArray
      switch inst {
      case let fas as FullApplySite:
        callees = calleeAnalysis.getIncompleteCallees(callee: fas.callee)
      case let bi as BuiltinInst:
        switch bi.id {
        case .Once, .OnceWithContext:
          callees = calleeAnalysis.getIncompleteCallees(callee: bi.operands[0].value)
        default:
          continue
        }
      case is StrongReleaseInst, is ReleaseValueInst, is DestroyValueInst:
        callees = calleeAnalysis.getIncompleteDestructors(of: inst.operands[0].value.type)
      default:
        continue
      }
      for callee in callees {
        if !onStack.contains(callee) {
          functionUses.addUse(of: callee, by: inst)
          numCallees += 1
          visit(callee, &onStack, calleeAnalysis)
        }
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

  mutating func pushNewFunctionToReadyList(_ function: Function) {
    assert(unhandledCallees[function] == nil, "not a new function")
    readyList.append((function, 0))
  }
}

private extension Int {
  mutating func decrementAndCheckForZero() -> Bool {
    assert(self > 0)
    self -= 1
    return self == 0
  }
}

private struct CompletedPasses {
  private var completedPasses: [UInt64] = []
  private let numPasses: Int
  private let numIntsPerFunction: Int

  init(numPasses: Int) {
    self.numPasses = numPasses
    self.numIntsPerFunction = (numPasses + 63) / 64
  }

  mutating func passDidNotModify(passIndex: Int, function: Function) {
    let idx = arrayIndex(passIndex: passIndex, functionIndex: function.uniqueIndex)
    if idx > completedPasses.count {
      completedPasses += Array(repeating: 0, count: idx - completedPasses.count + 1)
    }
    completedPasses[idx] |= bitMask(for: passIndex)
  }

  mutating func needToRunPass(passIndex: Int, on function: Function) -> Bool {
    let idx = arrayIndex(passIndex: passIndex, functionIndex: function.uniqueIndex)
    if idx >= completedPasses.count {
      return true
    }
    return completedPasses[idx] & bitMask(for: passIndex) == 0
  }

  mutating func notifyFunctionModified(function: Function) {
    let startIdx = arrayIndex(passIndex: 0, functionIndex: function.uniqueIndex)
    if startIdx < completedPasses.count {
      for idx in startIdx..<startIdx + numIntsPerFunction {
        completedPasses[idx] = 0
      }
    }
  }

  mutating func notifyAllFunctionsModified() {
    for idx in 0..<completedPasses.count {
      completedPasses[idx] = 0
    }
  }

  private func arrayIndex(passIndex: Int, functionIndex: Int) -> Int {
    assert(passIndex < numPasses, "pass created during pass-manager run")
    return passIndex / 64 + functionIndex * numIntsPerFunction
  }

  private func bitMask(for passIndex: Int) -> UInt64 {
    UInt64(1) << (passIndex % 64)
  }
}

extension BridgedPassManager {
  var passManager: PassManager {
    getSwiftPassManager().getAs(PassManager.self)!
  }
}
