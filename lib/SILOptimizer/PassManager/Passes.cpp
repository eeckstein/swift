
//===--- Passes.cpp - Swift Compiler SIL Pass Entrypoints -----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
///  \file
///  This file provides implementations of a few helper functions
///  which provide abstracted entrypoints to the SILPasses stage.
///
///  \note The actual SIL passes should be implemented in per-pass source files,
///  not in this file.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-optimizer"

#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Module.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/OptimizerBridging.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorOr.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/YAMLParser.h"

using namespace swift;

bool swift::runSILMandatoryPasses(SILModule &Module) {
  auto &opts = Module.getOptions();

  // Verify the module, if required.
  // OSSA lifetimes are incomplete until the SILGenCleanup pass runs.
  if (opts.VerifyAll)
    Module.verifyIncompleteOSSA();

  // If we parsed a .sil file that is already in canonical form, don't rerun
  // the diagnostic passes.
  if (Module.getStage() != SILStage::Raw)
    return false;

  executePassPipelinePlan(&Module,
                          PassPipelineKind::SILGen,
                          /*isMandatory*/ true);

  if (opts.VerifyAll && opts.OSSAVerifyComplete)
    Module.verifyOwnership();

  auto kind = opts.DebugSerialization ?
    PassPipelineKind::MandatoryDebugSerialization :
    PassPipelineKind::Mandatory;

  executePassPipelinePlan(&Module, kind, /*isMandatory*/ true);

  // If we were asked to debug serialization, exit now.
  auto &Ctx = Module.getASTContext();
  if (opts.DebugSerialization)
    return Ctx.hadError();

  // Generate diagnostics.
  Module.setStage(SILStage::Canonical);

  // Verify the module, if required.
  if (opts.VerifyAll)
    Module.verify();
  else {
    LLVM_DEBUG(Module.verify());
  }

  // If errors were produced during SIL analysis, return true.
  return Ctx.hadError();
}

bool swift::runSILOwnershipEliminatorPass(SILModule &Module) {
  auto &Ctx = Module.getASTContext();

  executePassPipelinePlan(&Module, PassPipelineKind::OwnershipEliminator);

  return Ctx.hadError();
}

void swift::runSILOptimizationPasses(SILModule &Module) {
  auto &opts = Module.getOptions();

  // Verify the module, if required.
  if (opts.VerifyAll)
    Module.verify();

  if (opts.DisableSILPerfOptimizations) {
    // If we are not supposed to run SIL perf optzns, we may still need to
    // serialize. So serialize now.
    executePassPipelinePlan(
        &Module, PassPipelineKind::SerializeSIL,
        /*isMandatory*/ true);
    return;
  }

  auto kind = opts.DebugSerialization ?
    PassPipelineKind::PerformanceDebugSerialization :
    PassPipelineKind::Performance;

  executePassPipelinePlan(&Module, kind);

  // Check if we actually serialized our module. If we did not, serialize now.
  if (!Module.isSerialized()) {
    executePassPipelinePlan(
        &Module, PassPipelineKind::SerializeSIL,
        /*isMandatory*/ true);
  }

  // If we were asked to debug serialization, exit now.
  if (opts.DebugSerialization)
    return;

  // Verify the module, if required.
  if (opts.VerifyAll)
    Module.verify();
  else {
    LLVM_DEBUG(Module.verify());
  }
}

void swift::runSILPassesForOnone(SILModule &Module) {
  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();

  // We want to run the Onone passes also for function which have an explicit
  // Onone attribute.
  executePassPipelinePlan(
      &Module, PassPipelineKind::Onone,
      /*isMandatory*/ true);

  // Verify the module, if required.
  if (Module.getOptions().VerifyAll)
    Module.verify();
  else {
    LLVM_DEBUG(Module.verify());
  }
}

void swift::runSILOptimizationPassesWithFileSpecification(SILModule &M) {
  executePassPipelinePlan(&M, PassPipelineKind::FromFile);
}

/// Get an ID string for the given pass Kind.
/// This is useful for tools that identify a pass
/// by its type name.
StringRef swift::PassKindID(PassKind Kind) {
  switch (Kind) {
#define PASS(ID, TAG, DESCRIPTION)                                             \
  case PassKind::ID:                                                           \
    return #ID;
#include "swift/SILOptimizer/PassManager/Passes.def"
  default:
    llvm_unreachable("Invalid pass kind?!");
  }

  llvm_unreachable("Unhandled PassKind in switch.");
}

/// Get a tag string for the given pass Kind.
/// This format is useful for command line options.
StringRef swift::PassKindTag(PassKind Kind) {
  switch (Kind) {
#define PASS(ID, TAG, DESCRIPTION)                                             \
  case PassKind::ID:                                                           \
    return TAG;
#define SWIFT_PASS(ID, TAG, DESCRIPTION)                                       \
  case PassKind::ID:                                                           \
    return TAG;
#include "swift/SILOptimizer/PassManager/Passes.def"
  case PassKind::invalidPassKind:
    llvm_unreachable("Invalid pass kind?!");
  }

  llvm_unreachable("Unhandled PassKind in switch.");
}

// During SIL Lowering, passes may see partially lowered SIL, which is
// inconsistent with the current (canonical) stage. We don't change the SIL
// stage until lowering is complete. Consequently, any pass added to this
// PassManager needs to be able to handle the output of the previous pass. If
// the function pass needs to read SIL from other functions, it may be best to
// convert it to a module pass to ensure that the SIL input is always at the
// same stage of lowering.
void swift::runSILLoweringPasses(SILModule &Module) {
  executePassPipelinePlan(&Module,
                          PassPipelineKind::Lowering,
                          /*isMandatory*/ true);

  Module.setStage(SILStage::Lowered);
}
