//===--- PassPipelines.swift ----------------------------------------------===//
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

func getOnoneFunctionPipeline(_ b: Bool) -> [FunctionPass] {
  functionPassPipeline {
    allocVectorLowering
    booleanLiteralFolding
    BridgedPass.ReleaseHoisting
    if b {
      deadStoreElimination
    }
  }
}

func getSILGenPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getDiagnosticPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getOwnershipEliminatorPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getPerformancePassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getOnonePassPipeline(options: Options) -> [ModulePass] {
  modulePassPipeline {
    getOnoneFunctionPipeline(false)
    BridgedModulePass.PerformanceDiagnostics
    functionPassPipeline {
      allocVectorLowering
      booleanLiteralFolding
    }
    mandatoryPerformanceOptimizations
  }

}

func getInstCountPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getLoweringPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getIRGenPreparePassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getSerializeSILPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getFromFilePassPipeline(options: Options) -> [ModulePass] {
  return []
}
