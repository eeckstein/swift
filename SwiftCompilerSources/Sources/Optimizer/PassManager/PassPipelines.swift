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

func getSILGenPassPipeline(options: Options) -> [ModulePass] {
  modulePasses("SILGen Passes") {
    BridgedModulePass.SILGenCleanup
    if options.enableLifetimeDependenceDiagnostics {
      functionPasses {
        lifetimeDependenceInsertionPass
        lifetimeDependenceScopeFixupPass
      }
    }
  }
}

func getMandatoryPassPipeline(options: Options) -> [ModulePass] {
  modulePasses("Mandatory Passes") {
    functionPasses {
      BridgedPass.DiagnoseInvalidEscapingCaptures
      BridgedPass.ReferenceBindingTransform
    }
    BridgedModulePass.DiagnoseStaticExclusivity
    functionPasses {
      BridgedPass.NestedSemanticFunctionCheck
    }
    BridgedModulePass.CapturePromotion

    // Select access kind after capture promotion and before stack promotion.
    // This guarantees that stack-promotable boxes have [static] enforcement.
    BridgedModulePass.AccessEnforcementSelection

    functionPasses {
      BridgedPass.AllocBoxToStack
      BridgedPass.NoReturnFolding
      booleanLiteralFolding
      BridgedPass.DefiniteInitialization
      letPropertyLowering
      BridgedPass.RawSILInstLowering
    }
    BridgedModulePass.AddressLowering

    functionPasses {
      // Before we run later semantic optimizations, eliminate simple functions that
      // we specialized to ensure that we do not emit diagnostics twice.
      BridgedPass.DiagnosticDeadFunctionElimination

      BridgedPass.FlowIsolation

      //===---
      // Passes that depend on region analysis information
      //

      BridgedPass.TransferNonSendable

      // Now that we have completed running passes that use region analysis, clear
      // region analysis and emit diagnostics for unnecessary preconcurrency
      // imports.
      BridgedPass.RegionAnalysisInvalidationTransform
    }
    BridgedModulePass.DiagnoseUnnecessaryPreconcurrencyImports

    functionPasses {
      // Lower tuple addr constructor. Eventually this can be merged into later
      // passes. This ensures we do not need to update later passes for something
      // that is only needed by TransferNonSendable().
      BridgedPass.LowerTupleAddrConstructor
    }
    // Automatic differentiation: canonicalize all differentiability witnesses
    // and `differentiable_function` instructions.
    BridgedModulePass.Differentiation

    functionPasses {
      BridgedPass.ClosureLifetimeFixup

      //===---
      // Begin Ownership Optimizations
      //
      // These happen after ClosureLifetimeFixup because they depend on the
      // resolution of nonescaping closure lifetimes to correctly check the use
      // of move-only values as captures in nonescaping closures as borrows.

      // Check noImplicitCopy and move only types for objects and addresses.
      BridgedPass.MoveOnlyChecker

      // FIXME: rdar://122701694 (`consuming` keyword causes verification error on
      //        invalid SIL types)
      //
      // Lower move only wrapped trivial types.
      //   BridgedPass.TrivialMoveOnlyTypeEliminator

      // Check no uses after consume operator of a value in an address.
      BridgedPass.ConsumeOperatorCopyableAddressesChecker
      // No uses after consume operator of copyable value.
      BridgedPass.ConsumeOperatorCopyableValuesChecker

      // Check ~Escapable.
      if options.enableLifetimeDependenceDiagnostics {
        lifetimeDependenceDiagnosticsPass
      }

      // As a temporary measure, we also eliminate move only for non-trivial types
      // until we can audit the later part of the pipeline. Eventually, this should
      // occur before IRGen.
      BridgedPass.MoveOnlyTypeEliminator

      //
      // End Ownership Optimizations
      //===---

      if options.shouldOptimize {
        BridgedPass.DestroyAddrHoisting
      }
    }
    // Add a verification pass to check our work when skipping function bodies.
    BridgedModulePass.SILSkippingChecker

    BridgedModulePass.MandatoryInlining
    BridgedModulePass.MandatorySILLinker

    functionPasses {
      // Promote loads as necessary to ensure we have enough SSA formation to emit
      // SSA based diagnostics.
      BridgedPass.PredictableMemoryAccessOptimizations

      // This phase performs optimizations necessary for correct interoperation of
      // Swift os log APIs with C os_log ABIs.
      // Pass dependencies: this pass depends on MandatoryInlining and Mandatory
      // Linking happening before this pass and ConstantPropagation happening after
      // this pass.
      BridgedPass.OSLogOptimization

      // Diagnostic ConstantPropagation must be rerun on deserialized functions
      // because it is sensitive to the assert configuration.
      // Consequently, certain optimization passes beyond this point will also rerun.
      BridgedPass.DiagnosticConstantPropagation

      // Now that we have emitted constant propagation diagnostics, try to eliminate
      // dead allocations.
      BridgedPass.PredictableDeadAllocationElimination

      // Now that we have finished performing diagnostics that rely on lexical
      // scopes, if lexical lifetimes are not enabled, eliminate lexical lifetimes.
      if !options.enableLexicalLifetimes {
        BridgedPass.LexicalLifetimeEliminator
      }

      BridgedPass.OptimizeHopToExecutor

      // These diagnostic passes must run before OnoneSimplification because
      // they rely on completely unoptimized SIL.
      BridgedPass.DiagnoseUnreachable
      BridgedPass.DiagnoseInfiniteRecursion
      BridgedPass.YieldOnceCheck
      BridgedPass.EmitDFDiagnostics

      // Only issue weak lifetime warnings for users who select object lifetime
      // optimization. The risk of spurious warnings outweighs the benefits.
      if options.copyPropagation == .on {
        BridgedPass.DiagnoseLifetimeIssues
      }

      // Canonical swift requires all non cond_br critical edges to be split.
      BridgedPass.SplitNonCondBrCriticalEdges
    }
    // For embedded Swift: Specialize generic class vtables.
    BridgedModulePass.VTableSpecializer

    mandatoryPerformanceOptimizations

    functionPasses {
      ononeSimplificationPass
      allocVectorLowering
      initializeStaticGlobalsPass
    }

    // MandatoryPerformanceOptimizations might create specializations that are not
    // used, and by being unused they are might have unspecialized applies.
    // Eliminate them via the DeadFunctionAndGlobalElimination in embedded Swift
    // to avoid getting metadata/existential use errors in them. We don't want to
    // run this pass in regular Swift: Even unused functions are expected to be
    // available in debug (-Onone) builds for debugging and development purposes.
    if options.enableEmbeddedSwift {
      BridgedModulePass.DeadFunctionAndGlobalElimination
    }

    BridgedModulePass.PerformanceDiagnostics
  }
}

func getOnoneFunctionPipeline(_ b: Bool) -> [FunctionPass] {
  functionPasses {
    allocVectorLowering
    booleanLiteralFolding
    BridgedPass.ReleaseHoisting
    if b {
      deadStoreElimination
    }
  }
}

func getOwnershipEliminatorPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getPerformancePassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getOnonePassPipeline(options: Options) -> [ModulePass] {
  modulePasses {
    getOnoneFunctionPipeline(false)
    BridgedModulePass.PerformanceDiagnostics
    functionPasses {
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

func getMandatoryDebugSerializationPassPipeline(options: Options) -> [ModulePass] {
  return []
}

func getPerformanceDebugSerializationPassPipeline(options: Options) -> [ModulePass] {
  return []
}
