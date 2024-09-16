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

func getSILGenPassPipeline(options: Options) -> Pipeline {
  pipeline("SILGen Passes", isMandatory: true) {
    BridgedModulePass.SILGenCleanup
    if options.enableLifetimeDependenceDiagnostics {
      functionPasses {
        lifetimeDependenceInsertionPass
        lifetimeDependenceScopeFixupPass
      }
    }
  }
}

func getMandatoryPassPipeline(options: Options) -> Pipeline {
  pipeline("Mandatory Passes", isMandatory: true) {
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

      if options.shouldOptimize && options.enableDestroyHoisting {
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
      ononeSimplification
      allocVectorLowering
      initializeStaticGlobals
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

func getOnonePassPipeline(options: Options) -> Pipeline {
  // These are optimizations that we do not need to enable diagnostics (or
  // depend on other passes needed for diagnostics). Thus we can run them later
  // and avoid having SourceKit run these passes when just emitting diagnostics
  // in the editor.
  pipeline("Onone passes", isMandatory: true) {
    functionPasses {
      BridgedPass.ForEachLoopUnroll

      // TODO: MandatoryARCOpts should be subsumed by CopyPropagation. There should
      // be no need to run another analysis of copies at -Onone.
      BridgedPass.MandatoryARCOpts

      // Create pre-specializations.
      // This needs to run pre-serialization because it needs to identify native
      // inlinable functions from imported ones.
      BridgedPass.OnonePrespecializations
    }
    // For embedded Swift: CMO is used to serialize libraries.
    BridgedModulePass.CrossModuleOptimization

    // First serialize the SIL if we are asked to.
    BridgedModulePass.SerializeSILPass

    functionPasses {
      // Now that we have serialized, propagate debug info.
      BridgedPass.MovedAsyncVarDebugInfoPropagator

      BridgedPass.OwnershipModelEliminator

      // Has only an effect if the -assume-single-thread option is specified.
      assumeSingleThreaded

      // In Onone builds, do a function-local analysis in a function pass.
      functionStackProtection

      // This is mainly there to optimize `Builtin.isConcrete`, which must not be
      // constant folded before any generic specialization.
      lateOnoneSimplification

      cleanupDebugSteps
    }
    BridgedModulePass.UsePrespecialized

    if options.enableEmbeddedSwift {
      // For embedded Swift: Remove all unspecialized functions. This is important
      // to avoid having debuginfo references to these functions that we don't
      // want to emit in IRGen.
      BridgedModulePass.LateDeadFunctionAndGlobalElimination
    }

    // Has only an effect if the -sil-based-debuginfo option is specified.
    BridgedModulePass.SILDebugInfoGenerator
  }
}

func getPerformancePassPipeline(options: Options) -> Pipeline {
  pipeline("Optimization passes", isMandatory: false) {

    prepareOptimizationsPasses()

    // Eliminate immediately dead functions and then clone functions from the
    // stdlib.
    //
    // This also performs early OSSA based optimizations on *all* swift code.
    //
    // Then run an iteration of the high-level SSA passes.
    //
    highLevelOptimizations(options)

    BridgedModulePass.CrossModuleOptimization

    // It is important to serialize before any of the @_semantics
    // functions are inlined, because otherwise the information about
    // uses of such functions inside the module is lost,
    // which reduces the ability of the compiler to optimize clients
    // importing this module.
    BridgedModulePass.SerializeSILPass

    if options.stopOptimizationAfterSerialization {
      .abortPipeline
    }

    functionPasses {
      BridgedPass.OwnershipModelEliminator
    }

    // After serialization run the function pass pipeline to iteratively lower
    // high-level constructs like @_semantics calls.
    midLevelOptimizations(options)

    // Perform optimizations that specialize.
    closureSpecializationPasses(options)

    // Run another iteration of the SSA optimizations to optimize the
    // devirtualized inline caches and constants propagated into closures
    // (CapturePropagation).
    lowLevelOptimizations(options)

    // Has only an effect if the -sil-based-debuginfo option is specified.
    BridgedModulePass.SILDebugInfoGenerator
  }
}

private func prepareOptimizationsPasses() -> [ModulePass] {
  modulePasses("Prepare optimizations passes") {
    // Verify AccessStorage once in OSSA before optimizing.
    BridgedModulePass.AccessPathVerification

    functionPasses {
      BridgedPass.ForEachLoopUnroll
      simplification
    }
    BridgedModulePass.AccessMarkerElimination
  }
}

private func highLevelOptimizations(_ options: Options) -> [ModulePass] {
  modulePasses("High-level optimizations") {
    // Get rid of apparently dead functions as soon as possible so that
    // we do not spend time optimizing them.
    BridgedModulePass.DeadFunctionAndGlobalElimination

    functionPasses {
      // Cleanup after SILGen: remove trivial copies to temporaries.
      BridgedPass.TempRValueOpt
      // Cleanup after SILGen: remove unneeded borrows/copies.
      if options.copyPropagation == .on {
        computeSideEffects
        BridgedPass.CopyPropagation
      }
      BridgedPass.SemanticARCOpts

      // Devirtualizes differentiability witnesses into functions that reference them.
      // This unblocks many other passes' optimizations (e.g. inlining) and this is
      // not blocked by any other passes' optimizations, so do it early.
      BridgedPass.DifferentiabilityWitnessDevirtualizer
    }

    // Start by linking in referenced functions from other modules.
    BridgedModulePass.PerformanceSILLinker

    functionPasses {
      // Cleanup after SILGen: remove trivial copies to temporaries. This version of
      // temp-rvalue opt is here so that we can hit copies from non-ossa code that
      // is linked in from the stdlib.
      BridgedPass.TempRValueOpt

      // Add the outliner pass (Osize).
      BridgedPass.Outliner

      // Skip EagerSpecializer on embedded Swift, which already specializes
      // everything. Otherwise this would create metatype references for functions
      // with @_specialize attribute and those are incompatible with Emebdded Swift.
      if !options.enableEmbeddedSwift {
        BridgedPass.EagerSpecializer
      }

      objCBridgingOptimization

      commonFunctionPasses(options, .highLevel)

      // Perform classic SSA optimizations for cleanup.
      BridgedPass.LowerAggregateInstrs
      BridgedPass.SILCombine
      BridgedPass.EarlySROA
      BridgedPass.Mem2Reg
      BridgedPass.DCE
      BridgedPass.SILCombine
      simplifyCFGSILCombinePasses()

      // Run high-level loop opts.
      BridgedPass.LoopRotate

      // Cleanup.
      BridgedPass.DCE
      // Also CSE semantic calls.
      BridgedPass.HighLevelCSE
      BridgedPass.SILCombine
      BridgedPass.SimplifyCFG
      // Optimize access markers for better LICM: might merge accesses
      // It will also set the no_nested_conflict for dynamic accesses
      // AccessEnforcementReleaseSinking results in non-canonical OSSA.
      // It is only used to expose opportunities in AccessEnforcementOpts
      // before CanonicalOSSA re-hoists destroys.
      BridgedPass.AccessEnforcementReleaseSinking
      BridgedPass.AccessEnforcementOpts
      BridgedPass.HighLevelLICM
      // Simplify CFG after LICM that creates new exit blocks
      BridgedPass.SimplifyCFG
      // LICM might have added new merging potential by hoisting
      // we don't want to restart the pipeline - ignore the
      // potential of merging out of two loops
      // AccessEnforcementReleaseSinking results in non-canonical OSSA.
      // It is only used to expose opportunities in AccessEnforcementOpts
      // before CanonicalOSSA re-hoists destroys.
      BridgedPass.AccessEnforcementReleaseSinking
      BridgedPass.AccessEnforcementOpts
      // Start of loop unrolling passes.
      BridgedPass.ArrayCountPropagation
      // To simplify induction variable.
      BridgedPass.SILCombine
      BridgedPass.LoopUnroll
      BridgedPass.SimplifyCFG
      BridgedPass.PerformanceConstantPropagation
      BridgedPass.SimplifyCFG
      BridgedPass.ArrayElementPropagation
      // End of unrolling passes.
      BridgedPass.ABCOpt
      // Cleanup.
      BridgedPass.DCE
      BridgedPass.COWArrayOpts
      // Cleanup.
      BridgedPass.DCE
      BridgedPass.SwiftArrayPropertyOpt

      BridgedPass.StringOptimization
      BridgedPass.DeadObjectElimination
      computeEscapeEffects
      computeSideEffects
    }
    BridgedModulePass.DeadFunctionAndGlobalElimination
    BridgedModulePass.PerformanceSILLinker
    BridgedModulePass.GlobalPropertyOpt
    if options.enableAsyncDemotion {
      asyncDemotion
    }
    BridgedModulePass.LetPropertiesOpt


    functionPasses {
      computeEscapeEffects
      computeSideEffects

      // Do the first stack promotion on high-level SIL before serialization.
      //
      stackPromotion

      // Run one last copy propagation/semantic arc opts run before serialization/us
      // lowering ownership.
      if options.enableOSSAModules {
        if options.copyPropagation != .off {
          BridgedPass.CopyPropagation
        }
        BridgedPass.SemanticARCOpts
      }
    }
  }
}

private func midLevelOptimizations(_ options: Options) -> [ModulePass] {
  modulePasses("Mid-level optimizations") {
    functionPasses {
      autodiffClosureSpecialization

      commonFunctionPasses(options, .midLevel)

      // Specialize partially applied functions with dead arguments as a preparation
      // for CapturePropagation.
      BridgedPass.DeadArgSignatureOpt

      // A LICM pass at mid-level is mainly needed to hoist addressors of globals.
      // It needs to be before global_init functions are inlined.
      BridgedPass.LICM
      // Run loop unrolling after inlining and constant propagation, because loop
      // trip counts may have became constant.
      BridgedPass.LICM
      BridgedPass.LoopUnroll
    }
  }
}

private func closureSpecializationPasses(_ options: Options) -> [ModulePass] {
  modulePasses("Closure specialization passes") {
    BridgedModulePass.DeadFunctionAndGlobalElimination
    readOnlyGlobalVariablesPass
    functionPasses {
      autodiffClosureSpecialization

      deadStoreElimination
      BridgedPass.DeadObjectElimination

      // These few passes are needed to cleanup between loop unrolling and InitializeStaticGlobals.
      // This is needed to fully optimize static small String constants.
      BridgedPass.SimplifyCFG
      BridgedPass.SILCombine
      BridgedPass.PerformanceConstantPropagation
      BridgedPass.SimplifyCFG
      simplification

      initializeStaticGlobals

      // ComputeEffects should be done at the end of a function-pipeline. The next
      // pass (LetPropertiesOpt) is a module pass, so this is the end of a function-pipeline.
      computeEscapeEffects
      computeSideEffects
    }
    BridgedModulePass.LetPropertiesOpt

    functionPasses {
      // Propagate constants into closures and convert to static dispatch.  This
      // should run after specialization and inlining because we don't want to
      // specialize a call that can be inlined. It should run before
      // ClosureSpecialization, because constant propagation is more effective.  At
      // least one round of SSA optimization and inlining should run after this to
      // take advantage of static dispatch.
      BridgedPass.CapturePropagation

      // Specialize closure.
      if options.enableExperimentalSwiftBasedClosureSpecialization {
        experimentalSwiftBasedClosureSpecialization
      } else {
        BridgedPass.ClosureSpecializer
      }

      // Do the second stack promotion on low-level SIL.
      stackPromotion

      // Speculate virtual call targets.
      if options.enableSpeculativeDevirtualization {
        BridgedPass.SpeculativeDevirtualization
      }

      // There should be at least one SILCombine+SimplifyCFG between the
      // ClosureSpecializer, etc. and the last inliner. Cleaning up after these
      // passes can expose more inlining opportunities.
      simplifyCFGSILCombinePasses()

      computeEscapeEffects
      computeSideEffects
    }
  }
}

private func lowLevelOptimizations(_ options: Options) -> [ModulePass] {
  modulePasses("Low-level optimizations") {
    functionPasses {
      // Should be after FunctionSignatureOpts and before the last inliner.
      releaseDevirtualizer

      commonFunctionPasses(options, .lowLevel)

      // The NamedReturnValueOptimization shouldn't be done before serialization.
      // For details see the comment for `namedReturnValueOptimization`.
      namedReturnValueOptimization

      BridgedPass.DeadObjectElimination
      objectOutliner
      deadStoreElimination
      BridgedPass.DCE
      simplification
      initializeStaticGlobals

      // dead-store-elimination can expose opportunities for dead object elimination.
      BridgedPass.DeadObjectElimination

      // We've done a lot of optimizations on this function, attempt to FSO.
      BridgedPass.FunctionSignatureOpts
      computeEscapeEffects
      computeSideEffects
    }
    // Delete dead code and drop the bodies of shared functions.
    // Also, remove externally available witness tables. They are not needed
    // anymore after the last devirtualizer run.
    BridgedModulePass.LateDeadFunctionAndGlobalElimination

    functionPasses {
      // Perform the final lowering transformations.
      BridgedPass.CodeSinking
      // Optimize access markers for better LICM: might merge accesses
      // It will also set the no_nested_conflict for dynamic accesses
      BridgedPass.AccessEnforcementReleaseSinking
      BridgedPass.AccessEnforcementOpts
      BridgedPass.LICM
      BridgedPass.COWOpts
      // Simplify CFG after LICM that creates new exit blocks
      BridgedPass.SimplifyCFG
      // LICM might have added new merging potential by hoisting
      // we don't want to restart the pipeline - ignore the
      // potential of merging out of two loops
      BridgedPass.AccessEnforcementReleaseSinking
      BridgedPass.AccessEnforcementOpts

      // Sometimes stack promotion can catch cases only at this late stage of the
      // pipeline, after FunctionSignatureOpts.
      computeEscapeEffects
      computeSideEffects
      stackPromotion

      // Optimize overflow checks.
      BridgedPass.RedundantOverflowCheckRemoval
      mergeCondFails

      // Remove dead code.
      BridgedPass.DCE
      BridgedPass.SILCombine
      BridgedPass.SimplifyCFG
      stripObjectHeaders

      // Try to hoist all releases, including epilogue releases. This should be
      // after FSO.
      BridgedPass.LateReleaseHoisting

      // Optimize access markers for improved IRGen after all other optimizations.
      BridgedPass.OptimizeHopToExecutor
      BridgedPass.AccessEnforcementReleaseSinking
      BridgedPass.AccessEnforcementOpts
    }
    BridgedModulePass.AccessEnforcementWMO

    functionPasses {
      BridgedPass.AccessEnforcementDom
      // addAccessEnforcementDom might provide potential for LICM:
      // A loop might have only one dynamic access now, i.e. hoistable
      BridgedPass.LICM

      // Only has an effect if the -assume-single-thread option is specified.
      assumeSingleThreaded

      // Emits remarks on all functions with @_assemblyVision attribute.
      BridgedPass.AssemblyVisionRemarkGenerator

    }
    // In optimized builds, do the inter-procedural analysis in a module pass.
    stackProtection

    // FIXME: rdar://72935649 (Miscompile on combining PruneVTables with WMO)
    // BridgedPass.PruneVTables

    // Verify AccessStorage once again after optimizing and lowering OSSA.
    BridgedModulePass.AccessPathVerification
  }
}

private enum PipelineLevel {
  case highLevel
  case midLevel
  case lowLevel
}

private func commonFunctionPasses(_ options: Options, _ level: PipelineLevel) -> [FunctionPass] {
  functionPasses {
    // Promote box allocations to stack allocations.
    BridgedPass.AllocBoxToStack

    if options.enableDestroyHoisting {
      BridgedPass.DestroyAddrHoisting
    }

    // Propagate copies through stack locations.  Should run after
    // box-to-stack promotion since it is limited to propagating through
    // stack locations. Should run before aggregate lowering since that
    // splits up copy_addr.
    BridgedPass.CopyForwarding

    // This DCE pass is the only DCE on ownership SIL. It can cleanup OSSA related
    // dead code, e.g. left behind by the ObjCBridgingOptimization.
    BridgedPass.DCE

    // Optimize copies from a temporary (an "l-value") to a destination.
    BridgedPass.TempLValueOpt

    // Split up opaque operations (copy_addr, retain_value, etc.).
    BridgedPass.LowerAggregateInstrs

    // Split up operations on stack-allocated aggregates (struct, tuple).
    if level == .highLevel {
      BridgedPass.EarlySROA
    } else {
      BridgedPass.SROA
    }

    // Promote stack allocations to values.
    BridgedPass.Mem2Reg

    // Run the existential specializer Pass.
    BridgedPass.ExistentialSpecializer

    // Cleanup, which is important if the inliner has restarted the pass pipeline.
    BridgedPass.PerformanceConstantPropagation

    simplifyCFGSILCombinePasses()

    BridgedPass.ArrayElementPropagation

    // Perform a round of loop/array optimization in the mid-level pipeline after
    // potentially inlining semantic calls, e.g. Array append. The high level
    // pipeline only optimizes semantic calls *after* inlining (see
    // addHighLevelLoopOptPasses). For example, the high-level pipeline may
    // perform ArrayElementPropagation and after inlining a level of semantic
    // calls, the mid-level pipeline may handle uniqueness hoisting. Do this as
    // late as possible before inlining because it must run between runs of the
    // inliner when the pipeline restarts.
    if level == .midLevel {
      BridgedPass.HighLevelLICM
      BridgedPass.ArrayCountPropagation
      BridgedPass.ABCOpt
      BridgedPass.DCE
      BridgedPass.COWArrayOpts
      BridgedPass.DCE
      BridgedPass.SwiftArrayPropertyOpt

      // This string optimization can catch additional opportunities, which are
      // exposed once optimized String interpolations (from the high-level string
      // optimization) are cleaned up. But before the mid-level inliner inlines
      // semantic calls.
      BridgedPass.StringOptimization
    }

    // Run the devirtualizer, specializer, and inliner. If any of these
    // makes a change we'll end up restarting the function passes on the
    // current function (after optimizing any new callees).
    BridgedPass.Devirtualizer
    // MandatoryPerformanceOptimizations already took care of all specializations
    // in embedded Swift mode, running the generic specializer might introduce
    // more generic calls from non-generic functions, which breaks the assumptions
    // of embedded Swift.
    if !options.enableEmbeddedSwift {
      BridgedPass.GenericSpecializer
      // Run devirtualizer after the specializer, because many
      // class_method/witness_method instructions may use concrete types now.
      BridgedPass.Devirtualizer
    }
    BridgedPass.ARCSequenceOpts

    deinitDevirtualizer

    // We earlier eliminated ownership if we are not compiling the stdlib. Now
    // handle the stdlib functions, re-simplifying, eliminating ARC as we do.
    if options.copyPropagation != .off {
      BridgedPass.CopyPropagation
    }
    BridgedPass.SemanticARCOpts

    if !options.enableOSSAModules {
      BridgedPass.NonTransparentFunctionOwnershipModelEliminator
    }

    switch level {
    case .highLevel:
      // Does not inline functions with defined semantics or effects.
      BridgedPass.EarlyPerfInliner
    case .midLevel, .lowLevel:
      // Inlines everything
      BridgedPass.PerfInliner
    }

    // Clean up Semantic ARC before we perform additional post-inliner opts.
    if options.enableOSSAModules {
      if options.copyPropagation != .off {
        BridgedPass.CopyPropagation
      }
      BridgedPass.SemanticARCOpts
    }

    // Promote stack allocations to values and eliminate redundant
    // loads.
    BridgedPass.Mem2Reg
    BridgedPass.PerformanceConstantPropagation
    //  Do a round of CFG simplification, followed by peepholes, then
    //  more CFG simplification.

    // Jump threading can expose opportunity for SILCombine (enum -> is_enum_tag->
    // cond_br).
    BridgedPass.JumpThreadSimplifyCFG
    BridgedPass.PhiExpansion
    BridgedPass.SILCombine
    // SILCombine can expose further opportunities for SimplifyCFG.
    BridgedPass.SimplifyCFG

    BridgedPass.CSE
    if level == .highLevel {
      // Early RLE does not touch loads from Arrays. This is important because
      // later array optimizations, like ABCOpt, get confused if an array load in
      // a loop is converted to a pattern with a phi argument.
      earlyRedundantLoadElimination
    } else {
      redundantLoadElimination
    }
    // Optimize copies created during RLE.
    BridgedPass.SemanticARCOpts

    BridgedPass.COWOpts
    BridgedPass.PerformanceConstantPropagation
    // Remove redundant arguments right before CSE and DCE, so that CSE and DCE
    // can cleanup redundant and dead instructions.
    BridgedPass.RedundantPhiElimination
    BridgedPass.CSE
    BridgedPass.DCE

    // Perform retain/release code motion and run the first ARC optimizer.
    BridgedPass.EarlyCodeMotion
    BridgedPass.ReleaseHoisting
    BridgedPass.ARCSequenceOpts
    BridgedPass.TempRValueOpt

    BridgedPass.SimplifyCFG
    if level == .lowLevel {
      // Only hoist releases very late.
      BridgedPass.LateCodeMotion
    } else {
      BridgedPass.EarlyCodeMotion
    }

    BridgedPass.RetainSinking
    // Retain sinking does not sink all retains in one round.
    // Let it run one more time time, because it can be beneficial.
    // FIXME: Improve the RetainSinking pass to sink more/all
    // retains in one go.
    BridgedPass.RetainSinking
    BridgedPass.ReleaseHoisting
    BridgedPass.ARCSequenceOpts

    // Run a final round of ARC opts when ownership is enabled.
    if options.enableOSSAModules {
      if options.copyPropagation != .off {
        BridgedPass.CopyPropagation
      }
      BridgedPass.SemanticARCOpts
    }
  }
}

private func simplifyCFGSILCombinePasses() -> [FunctionPass] {
  functionPasses {
    BridgedPass.SimplifyCFG
    BridgedPass.ConditionForwarding
    // Jump threading can expose opportunity for silcombine (enum -> is_enum_tag->
    // cond_br).
    BridgedPass.SILCombine
    // Which can expose opportunity for simplifycfg.
    BridgedPass.SimplifyCFG
  }
}

func getOwnershipEliminatorPassPipeline(options: Options) -> Pipeline {
  pipeline("OwnershipModelEliminator", isMandatory: true) {
    BridgedModulePass.AddressLowering
    functionPasses {
      BridgedPass.OwnershipModelEliminator
    }
  }
}

func getInstCountPassPipeline(options: Options) -> Pipeline {
  pipeline("InstCount", isMandatory: false) {
    functionPasses {
      BridgedPass.InstCount
    }
  }
}

func getLoweringPassPipeline(options: Options) -> Pipeline {
  pipeline("Lowering", isMandatory: true) {
    functionPasses {
      BridgedPass.LowerHopToActor
      // Re-run the OwnershipModelEliminator for functions with `@_optimize(none)` or if -sil-opt-pass-count is used.
      BridgedPass.OwnershipModelEliminator
    }
    BridgedModulePass.AlwaysEmitConformanceMetadataPreservation
    functionPasses {
      BridgedPass.IRGenPrepare
    }
  }
}

func getIRGenPreparePassPipeline(options: Options) -> Pipeline {
  pipeline("IRGen prepare", isMandatory: true) {
    functionPasses {
      /*
      // Simplify partial_apply instructions by expanding box construction into
      // component operations.
      BridgedPass.PartialApplySimplification
       */
      // Hoist generic alloc_stack instructions to the entry block to enable better
      // llvm-ir generation for dynamic alloca instructions.
      BridgedPass.AllocStackHoisting

      if options.enablePackMetadataStackPromotion {
        // Insert marker instructions indicating where on-stack pack metadata
        // deallocation must occur.
        //
        // No code motion may occur after this pass: alloc_pack_metadata must
        // directly precede the instruction on behalf of which metadata will
        // actually be emitted (e.g. apply).
        BridgedPass.PackMetadataMarkerInserter
      }
    }
    // Change large loadable types to be passed indirectly across function
    // boundaries as required by the ABI.
    BridgedModulePass.LoadableByAddress
  }
}

func getSerializeSILPassPipeline(options: Options) -> Pipeline {
  pipeline("Serialize SIL", isMandatory: true) {
    BridgedModulePass.SerializeSILPass
  }
}

func getFromFilePassPipeline(options: Options) -> Pipeline {
  fatalError("TODO")
}

func getMandatoryDebugSerializationPassPipeline(options: Options) -> Pipeline {
  pipeline("Mandatory Debug Serialization", isMandatory: true) {
    BridgedModulePass.AddressLowering
    functionPasses {
      BridgedPass.OwnershipModelEliminator
    }
    BridgedModulePass.MandatoryInlining
  }
}

func getPerformanceDebugSerializationPassPipeline(options: Options) -> Pipeline {
  pipeline("Performance Debug Serialization", isMandatory: false) {
    BridgedModulePass.PerformanceSILLinker
  }
}
