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

  private var scheduledFunctionPasses: [(FunctionPass, uniqueID: Int)] = []

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
  private var restartPipeline = false

  private let maxNumPassesToRun: Int
  private let maxNumSubpassesToRun: Int

  private var isMandatory = false

  private var shouldPrintPassNames: Bool
  private var anyPassOptionSet: Bool
  private var shouldVerifyAfterAllChanges: Bool

  private static var registeredFunctionPasses = Dictionary<String, (FunctionPass, uniqueID: Int)>()
  private static var registeredModulePasses = Dictionary<String, ModulePass>()

  private init(bridged: BridgedPassManager) {
    self._bridged = bridged
    let numRegisteredPasses = Self.registeredFunctionPasses.count + Self.registeredModulePasses.count
    self.completedPasses = CompletedPasses(numPasses: numRegisteredPasses)
    self.maxNumPassesToRun = _bridged.getMaxNumPassesToRun()
    self.maxNumSubpassesToRun = _bridged.getMaxNumSubpassesToRun()
    self.shouldPrintPassNames =  _bridged.shouldPrintPassNames()
    self.anyPassOptionSet = _bridged.anyPassOptionSet()
    self.shouldVerifyAfterAllChanges = _bridged.shouldVerifyAfterAllChanges()
    bridged.setSwiftPassManager(SwiftObject(self))
  }

  deinit {
    _bridged.setSwiftPassManager(nil)
  }

  static func register(functionPass: FunctionPass) {
    assert(registeredFunctionPasses[functionPass.name] == nil, "function pass registered a second time")
    registeredFunctionPasses[functionPass.name] = (functionPass, uniqueID: registeredFunctionPasses.count)
  }

  static func register(modulePass: ModulePass) {
    assert(registeredModulePasses[modulePass.name] == nil, "module pass registered a second time")
    registeredModulePasses[modulePass.name] = modulePass
  }

  static func lookupFunctionPass(withName name: String) -> FunctionPass {
    guard let (pass, _) = registeredFunctionPasses[name] else {
      fatalError("bridged pass \(name) is not registered")
    }
    return pass
  }

  static func lookupModulePass(withName name: String) -> ModulePass {
    guard let pass = registeredModulePasses[name] else {
      fatalError("bridged pass \(name) is not registered")
    }
    return pass
  }

  static func buildPassPipeline(fromPassNames passNames: [String]) -> [ModulePass] {
    var pipeline: [ModulePass] = []
    var i = 0
    while i < passNames.count {
      if let modulePass = registeredModulePasses[passNames[i]] {
        pipeline.append(modulePass)
        i += 1
        continue
      }
      var functionPasses: [FunctionPass] = []
      while i < passNames.count,
            let (functionPass, _) = registeredFunctionPasses[passNames[i]]
      {
        functionPasses.append(functionPass)
        i += 1
      }
      assert(!functionPasses.isEmpty, "unknown pass")
      let pass = ModulePass(name: "function passes") {
        $0.passManager.scheduleFunctionPassesForRunning(passes: functionPasses)
      }
      pipeline.append(pass)
    }
    return pipeline
  }

  func runPipeline(_ modulePasses: [ModulePass]) {
    let context = createModulePassContext()

    for (passIdx, pass) in modulePasses.enumerated() {
      if !shouldContinueTransforming {
        return
      }
      currentSubPassIndex = 0
      currentPassMadeChanges = false
      currentPassDepdendsOnOtherFunction = false
      restartPipeline = false

      if isPassDisabled(pass) {
        if shouldPrintPassNames {
          printPassInfo("(Disabled)", pass.name, passIdx)
        }
        continue
      }

      if shouldPrintPassNames {
        printPassInfo("Run module pass", pass.name, passIdx)
      }

      if shouldPrintBefore(pass: pass) {
        printPassInfo("*** SIL module before", pass.name, passIdx)
        print(context)
      }

      pass.name._withBridgedStringRef() {
        _bridged.preModulePassRun($0, currentPassIndex)
      }

      pass.runFunction(context)

      if currentPassMadeChanges {
        completedPasses.notifyAllFunctionsModified()
        if shouldVerifyAfterAllChanges {
          context.verifyModule()
        }
      }

      _bridged.postModulePassRun()

      if shouldPrintAfter(pass: pass) {
        printPassInfo("*** module after", pass.name, passIdx)
        print(context)
      }
      if currentPassMadeChanges && shouldPrintAnyFunction {
        printPassInfo("*** functions after", pass.name, passIdx)
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

    defer { worklist.clear() }

    var allFunctions = context.functions
    
    while let element = worklist.next(allFunctions: &allFunctions, numPasses: scheduledFunctionPasses.count,
                                      context.calleeAnalysis)
    {
      runFunctionPasses(on: element.function, initialPassIndex: element.completedPasses, context)
      if !shouldContinueTransforming {
        return
      }
    }
    worklist.verifyIsEmpty()
  }

  private func runFunctionPasses(on function: Function, initialPassIndex: Int, _ context: ModulePassContext) {
    let numPasses = scheduledFunctionPasses.count
    if !function.shouldOptimize && !isMandatory {
      worklist.updateNumberOfCompletedPasses(to: numPasses)
      return
    }

    var passIdx = initialPassIndex

    while passIdx < numPasses {
      if !shouldContinueTransforming {
        return
      }
      runFunctionPass(on: function, passIndex: passIdx, context)

      currentPassIndex += 1

      if restartPipeline && worklist.resetPassIndex() {
        passIdx = 0
      } else {
        passIdx += 1
      }
      worklist.updateNumberOfCompletedPasses(to: passIdx)

      if !worklist.continueRunning() {
        return
      }

    }
  }

  private func runFunctionPass(on function: Function, passIndex: Int, _ context: ModulePassContext) {
    currentSubPassIndex = 0
    currentPassMadeChanges = false
    currentPassDepdendsOnOtherFunction = false
    restartPipeline = false

    let (pass, uniqueID) = scheduledFunctionPasses[passIndex]
    if !completedPasses.needToRunPass(passID: uniqueID, on: function) {
      if shouldPrintPassNames {
        printPassInfo("(Skipping)", pass.name, passIndex, function)
      }
      return
    }
    if isPassDisabled(pass) {
      if shouldPrintPassNames {
        printPassInfo("(Disabled)", pass.name, passIndex, function)
      }
      return
    }
    if shouldPrintPassNames {
      printPassInfo("Run", pass.name, passIndex, function)
    }
    if shouldPrintBefore(pass: pass) || isLastPass(passIndex) {
      printPassInfo("*** function before", pass.name, passIndex, function)
      print(function)
    }

    pass.name._withBridgedStringRef() {
      _bridged.preFunctionPassRun(function.bridged, $0, currentPassIndex)
    }

    let functionPassContext = FunctionPassContext(_bridged: context._bridged)

    // TODO: rename runFunction -> run
    pass.runFunction(function, functionPassContext)

    if currentPassMadeChanges && shouldVerifyAfterAllChanges {
      verify(function: function, functionPassContext)
    }

    _bridged.postFunctionPassRun()

    if shouldPrintAfter(pass: pass) ||
       isLastPass(passIndex) ||
       (currentPassMadeChanges && shouldPrint(function: function))
    {
      printPassInfo("*** function after", pass.name, passIndex, function)
      print(function)
    }
    if currentPassMadeChanges || currentPassDepdendsOnOtherFunction {
      completedPasses.notifyFunctionModified(function: function)
    } else {
      completedPasses.passDidNotModify(passID: uniqueID, function: function)
    }
  }

  private var shouldContinueTransforming: Bool {
    if isMandatory {
      return true
    }
    return currentPassIndex < maxNumPassesToRun
  }

  private func printPassInfo(_ title: String, _ passName: String, _ passIndex: Int, _ function: Function? = nil) {
    let pipelineInfo = pipelineStages.last ?? "?"
    let fnInfo: String
    if let function = function {
      fnInfo = ", Function: \(function.name)"
    } else {
      fnInfo = ""
    }
    print("  \(title) #\(currentPassIndex), stage \(pipelineInfo), pass \(passIndex): \(passName)\(fnInfo)")
  }

  private func verify(function: Function, _ context: FunctionPassContext) {
    context._bridged.verifyFunction(function.bridged)
    function.verify(context)
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


    if !scheduledFunctionPasses.isEmpty {
      worklist.addCallee(function)
    }
  }

  func notifyPassMadeChanges() {
    currentPassMadeChanges = true
  }

  func notifyPassDependsOnOtherFunction() {
    currentPassDepdendsOnOtherFunction = true
  }

  func notifyRestartPipeline() {
    restartPipeline = true
  }

  func continueWithNextSubpassRun(on function: Function, for inst: Instruction? = nil) -> Bool {
    let subPassIdx = currentSubPassIndex
    currentSubPassIndex += 1

    if isMandatory {
      return true
    }
    if (currentPassIndex != maxNumPassesToRun - 1) {
      return true
    }

    if isLastSubpass(subPassIdx) {
      let instInfo: String
      if let inst = inst {
        instInfo = " for \(inst)"
      } else {
        instInfo = ""
      }
      print("*** function before sub-pass \(subPassIdx)\(instInfo)")
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
    scheduledFunctionPasses = passes.map {
      guard let passAndUniqueID = Self.registeredFunctionPasses[$0.name] else {
        fatalError("pass \($0.name) is not registered")
      }
      return passAndUniqueID
    }
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

  static func register() {
    BridgedPassManager.registerBridging(
      // executePassesFn
      { (bridgedPM: BridgedPassManager, bridgedPipelineKind: BridgedPassManager.PassPipelineKind) in
        let options = Options(_bridged: bridgedPM.getContext())
        let pipeline = getPassPipeline(ofKind: bridgedPipelineKind, options: options)
        do {
          let pm = PassManager(bridged: bridgedPM)
          pm.runPipeline(pipeline)
          assert(bridgedPM.getSwiftPassManager() != nil)
        }
        assert(bridgedPM.getSwiftPassManager() == nil)
      },
      // executePassesFromNameFn
      { (bridgedPM: BridgedPassManager, bridgedPassNames: BridgedArrayRef) in
        let passNames: [String] = bridgedPassNames.withElements(ofType: BridgedStringRef.self) {
            (buffer: UnsafeBufferPointer<BridgedStringRef>) -> [String] in
          buffer.map { String($0) }
        }
        let pipeline = PassManager.buildPassPipeline(fromPassNames: passNames)
        do {
          let pm = PassManager(bridged: bridgedPM)
          pm.runPipeline(pipeline)
          assert(bridgedPM.getSwiftPassManager() != nil)
        }
        assert(bridgedPM.getSwiftPassManager() == nil)
      },
      // registerBridgedModulePassFn
      { (bridgedPassName: BridgedStringRef, passKind: BridgedModulePass) in
        PassManager.register(modulePass: ModulePass(name: String(bridgedPassName)) {
          (context: ModulePassContext) in
          context._bridged.getPassManager().runBridgedModulePass(passKind)
        })
      },
      // registerBridgedFunctionPassFn
      { (bridgedPassName: BridgedStringRef, passKind: BridgedPass) in
        PassManager.register(functionPass: FunctionPass(name: String(bridgedPassName)) {
          (function: Function, context: FunctionPassContext) in
          context._bridged.getPassManager().runBridgedFunctionPass(passKind, function.bridged)
        })
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
      },
      // notifyRestartPipelineFn
      {
        (bridgedPM: BridgedPassManager) in
        let pm = bridgedPM.getSwiftPassManager().getAs(PassManager.self)!
        pm.notifyRestartPipeline()
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
    let passName = StringRef(bridged: BridgedPassManager.getPassName(passKind)).string
    return [PassManager.lookupFunctionPass(withName: passName)]
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

  struct Element {
    let function: Function
    var firstCalleeIndex: Int
    var completedPasses = 0
    var numRestarts = 0

    mutating func resetPassIndex() -> Bool{
      if numRestarts < FunctionWorklist.maxNumPipelineRestarts {
        completedPasses = 0
        numRestarts += 1
        return true
      }
      return false
    }
  }

  private var stack: [Element] = []
  private var callees: [Function] = []
  private var visited = Set<Function>()
  private var calleesInFunction = Set<Function>()

  static let maxNumPipelineRestarts = 20

  func verifyIsEmpty() {
    assert(stack.isEmpty && callees.isEmpty, "not all functions handled")
  }

  mutating func clear() {
    stack.removeAll(keepingCapacity: true)
    callees.removeAll(keepingCapacity: true)
    visited.removeAll(keepingCapacity: true)
  }

  mutating func next(allFunctions: inout ModulePassContext.FunctionList,
                     numPasses: Int,
                     _ calleeAnalysis: CalleeAnalysis
  ) -> Element? {
    while true {
      if let current = stack.last {
        if callees.count > current.firstCalleeIndex {
          push(function: callees.removeLast(), calleeAnalysis)
          continue
        }
        if current.completedPasses == numPasses {
          stack.removeLast()
          continue
        }
        return current
      }
      if let f = allFunctions.next() {
        push(function: f, calleeAnalysis)
        continue
      }
      return nil
    }
  }

  private mutating func push(function: Function, _ calleeAnalysis: CalleeAnalysis) {
    if !function.isDefinition {
      return
    }
    if !visited.insert(function).inserted {
      return
    }

    defer { calleesInFunction.removeAll(keepingCapacity: true) }

    stack.append(Element(function: function, firstCalleeIndex: callees.count))

    assert(function.isDefinition, "should only visit functions with bodies")

    for inst in function.instructions {
      let instCallees: FunctionArray
      switch inst {
      case let fas as FullApplySite:
        instCallees = calleeAnalysis.getIncompleteCallees(callee: fas.calleeOrigin)
      case let bi as BuiltinInst:
        switch bi.id {
        case .Once, .OnceWithContext:
          instCallees = calleeAnalysis.getIncompleteCallees(callee: bi.operands[0].value)
        default:
          continue
        }
      case is StrongReleaseInst, is ReleaseValueInst, is DestroyValueInst:
        instCallees = calleeAnalysis.getIncompleteDestructors(of: inst.operands[0].value.type)
      default:
        continue
      }
      for c in instCallees {
        if c.isDefinition && calleesInFunction.insert(c).inserted {
          callees.append(c)
        }
      }
    }
  }

  mutating func updateNumberOfCompletedPasses(to numPasses: Int) {
    stack[stack.count - 1].completedPasses = numPasses
  }

  mutating func resetPassIndex() -> Bool {
    return stack[stack.count - 1].resetPassIndex()
  }

  mutating func continueRunning() -> Bool {
    return stack.last!.firstCalleeIndex == callees.count
  }

  mutating func addCallee(_ callee: Function) {
    assert(!stack.isEmpty, "no currently transformed function")
    callees.append(callee)
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

  mutating func passDidNotModify(passID: Int, function: Function) {
    let idx = arrayIndex(passID: passID, functionIndex: function.uniqueIndex)
    if idx >= completedPasses.count {
      let endIndex = arrayIndex(passID: 0, functionIndex: function.uniqueIndex + 1)
      completedPasses += Array(repeating: 0, count: endIndex - completedPasses.count)
    }
    completedPasses[idx] |= bitMask(for: passID)
  }

  mutating func needToRunPass(passID: Int, on function: Function) -> Bool {
    let idx = arrayIndex(passID: passID, functionIndex: function.uniqueIndex)
    if idx >= completedPasses.count {
      return true
    }
    return completedPasses[idx] & bitMask(for: passID) == 0
  }

  mutating func notifyFunctionModified(function: Function) {
    let startIdx = arrayIndex(passID: 0, functionIndex: function.uniqueIndex)
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

  private func arrayIndex(passID: Int, functionIndex: Int) -> Int {
    assert(passID < numPasses, "pass created during pass-manager run")
    return passID / 64 + functionIndex * numIntsPerFunction
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
