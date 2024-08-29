//===--- Passes.swift ---- instruction and function passes ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import SIL
import OptimizerBridging

struct FunctionPass {

  let name: String
  let runFunction: (Function, FunctionPassContext) -> ()

  /// Unique identifier for vector indexing and deterministic sorting.
  let uniqueIndex: Int

  public init(name: String,
              _ runFunction: @escaping (Function, FunctionPassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
    self.uniqueIndex = PassManager.allocatePassIndex(passName: name)
  }

  func run(_ bridgedCtxt: BridgedFunctionPassCtxt) {
    let function = bridgedCtxt.function.function
    let context = FunctionPassContext(_bridged: bridgedCtxt.passContext)
    runFunction(function, context)
  }
}

struct ModulePass {

  let name: String
  let runFunction: (ModulePassContext) -> ()

  /// Unique identifier for vector indexing and deterministic sorting.
  let uniqueIndex: Int

  public init(name: String,
              _ runFunction: @escaping (ModulePassContext) -> ()) {
    self.name = name
    self.runFunction = runFunction
    self.uniqueIndex = PassManager.allocatePassIndex(passName: name)
  }

  func run(_ bridgedCtxt: BridgedPassContext) {
    fatalError("cannot run swift module pass from legacy SILPassManager")
  }
}
