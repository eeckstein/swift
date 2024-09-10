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

protocol Pass {
  var name: String { get }
}

struct FunctionPass: Pass {

  let name: String
  let run: (Function, FunctionPassContext) -> ()

  public init(name: String,
              _ run: @escaping (Function, FunctionPassContext) -> ()) {
    self.name = name
    self.run = run
  }
}

struct ModulePass: Pass {

  let name: String
  let run: (ModulePassContext) -> ()

  public init(name: String,
              _ run: @escaping (ModulePassContext) -> ()) {
    self.name = name
    self.run = run
  }
}
