//===--- PassManager.h  - Swift Pass Manager --------------------*- C++ -*-===//
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

#include "swift/SIL/Notifications.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/NodeBits.h"
#include "swift/SIL/OperandBits.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/PassManager/PassPipeline.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/PrettyStackTrace.h"
#include "swift/SILOptimizer/Utils/SILSSAUpdater.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/ErrorHandling.h"
#include <vector>
#include <bitset>

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_PASSMANAGER_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_PASSMANAGER_H

namespace swift {

class SILFunction;
class SILFunctionTransform;
class SILModule;
class SILModuleTransform;
class SILOptions;
class SILTransform;
class SILPassManager;
class SILCombiner;

namespace irgen {
class IRGenModule;
class IRGenerator;
}

/// The main entrypoint for executing a pipeline pass on a SIL module.
void executePassPipelinePlan(SILModule *SM, PassPipelineKind kind,
                             bool isMandatory = false,
                             irgen::IRGenModule *IRMod = nullptr);

/// Only to be used for testing (sil-opt) and non-compiler utilities.
void executePasses(ArrayRef<PassKind> passKinds,
                   SILModule *mod, irgen::IRGenModule *IRMod = nullptr,
                   bool isMandatory = false);

/// Utility class to invoke Swift passes.
class SwiftPassInvocation {
  /// Backlink to the pass manager.
  SILPassManager *passManager;

  /// The currently optimized function.
  SILFunction *function = nullptr;

  /// Non-null if this is an instruction pass, invoked from SILCombine.
  SILCombiner *silCombiner = nullptr;

  bool isTransformingFunction = false;

  /// Change notifications, collected during a pass run.
  SILAnalysis::InvalidationKind changeNotifications =
      SILAnalysis::InvalidationKind::Nothing;

  /// All slabs, allocated by the pass.
  SILModule::SlabList allocatedSlabs;

  SILSSAUpdater *ssaUpdater = nullptr;

  SwiftPassInvocation *nestedSwiftPassInvocation = nullptr;

  static constexpr int BlockSetCapacity = SILBasicBlock::numCustomBits;
  char blockSetStorage[sizeof(BasicBlockSet) * BlockSetCapacity];
  bool aliveBlockSets[BlockSetCapacity];
  int numBlockSetsAllocated = 0;

  static constexpr int NodeSetCapacity = SILNode::numCustomBits;
  char nodeSetStorage[sizeof(NodeSet) * NodeSetCapacity];
  bool aliveNodeSets[NodeSetCapacity];
  int numNodeSetsAllocated = 0;

  static constexpr int OperandSetCapacity = Operand::numCustomBits;
  char operandSetStorage[sizeof(OperandSet) * OperandSetCapacity];
  bool aliveOperandSets[OperandSetCapacity];
  int numOperandSetsAllocated = 0;

  int numClonersAllocated = 0;

  bool needFixStackNesting = false;

  void endPass();

public:
  SwiftPassInvocation(SILPassManager *passManager, SILFunction *function,
                         SILCombiner *silCombiner) :
    passManager(passManager), function(function), silCombiner(silCombiner) {}

  SwiftPassInvocation(SILPassManager *passManager, SILFunction *function) :
    passManager(passManager), function(function) {}

  SwiftPassInvocation(SILPassManager *passManager) :
    passManager(passManager) {}

  ~SwiftPassInvocation();

  SILPassManager *getPassManager() const { return passManager; }
  
  SILFunction *getFunction() const { return function; }

  irgen::IRGenModule *getIRGenModule();

  FixedSizeSlab *allocSlab(FixedSizeSlab *afterSlab);

  FixedSizeSlab *freeSlab(FixedSizeSlab *slab);

  BasicBlockSet *allocBlockSet();

  void freeBlockSet(BasicBlockSet *set);

  NodeSet *allocNodeSet();

  void freeNodeSet(NodeSet *set);

  OperandSet *allocOperandSet();

  void freeOperandSet(OperandSet *set);

  /// The top-level API to erase an instruction, called from the Swift pass.
  void eraseInstruction(SILInstruction *inst);

  /// Called by the pass when changes are made to the SIL.
  void notifyChanges(SILAnalysis::InvalidationKind invalidationKind);

  /// Called by the pass manager before the pass starts running.
  void startModulePassRun();

  /// Called by the pass manager before the pass starts running.
  void startFunctionPassRun(SILFunction *function);

  /// Called by the SILCombiner before the instruction pass starts running.
  void startInstructionPassRun(SILInstruction *inst);

  /// Called by the pass manager when the pass has finished.
  void finishedModulePassRun();

  /// Called by the pass manager when the pass has finished.
  void finishedFunctionPassRun();

  /// Called by the SILCombiner when the instruction pass has finished.
  void finishedInstructionPassRun();
  
  void beginTransformFunction(SILFunction *function);

  void endTransformFunction();

  void beginVerifyFunction(SILFunction *function);
  void endVerifyFunction();

  void notifyNewCloner() { numClonersAllocated++; }
  void notifyClonerDestroyed() { numClonersAllocated--; }

  void setNeedFixStackNesting(bool newValue) { needFixStackNesting = newValue; }
  bool getNeedFixStackNesting() const { return needFixStackNesting; }

  void initializeSSAUpdater(SILFunction *fn, SILType type,
                            ValueOwnershipKind ownership) {
    if (!ssaUpdater)
      ssaUpdater = new SILSSAUpdater;
    ssaUpdater->initialize(fn, type, ownership);
  }

  SILSSAUpdater *getSSAUpdater() const {
    assert(ssaUpdater && "SSAUpdater not initialized");
    return ssaUpdater;
  }

  SwiftPassInvocation *initializeNestedSwiftPassInvocation(SILFunction *newFunction) {
    assert(!nestedSwiftPassInvocation && "Nested Swift pass invocation already initialized");
    nestedSwiftPassInvocation = new SwiftPassInvocation(passManager, newFunction);
    nestedSwiftPassInvocation->isTransformingFunction = true;
    return nestedSwiftPassInvocation;
  }

  void deinitializeNestedSwiftPassInvocation() {
    assert(nestedSwiftPassInvocation && "Nested Swift pass invocation not initialized");
    nestedSwiftPassInvocation->endTransformFunction();
    delete nestedSwiftPassInvocation;
    nestedSwiftPassInvocation = nullptr;
  }
};

class DebugPrintEnabler {
#ifndef NDEBUG
  bool OldDebugFlag;
#endif
public:
  DebugPrintEnabler(unsigned PassNumber);
  ~DebugPrintEnabler();
};

/// The SIL pass manager.
class SILPassManager {
  friend class ExecuteSILPipelineRequest;

  /// The module that the pass manager will transform.
  SILModule *Mod;

  /// An optional IRGenModule associated with this PassManager.
  irgen::IRGenModule *IRMod;
  irgen::IRGenerator *irgen;

  std::vector<SILTransform *> allTransformations;

  /// The list of transformations to run.
  llvm::SmallVector<SILTransform *, 16> Transformations;

  /// A list of registered analysis.
  llvm::SmallVector<SILAnalysis *, 16> Analyses;

  /// The number of passes run so far.
  unsigned NumPassesRun = 0;

  unsigned maxNumPassesToRun = UINT_MAX;
  unsigned maxNumSubpassesToRun = UINT_MAX;

  /// For invoking Swift passes.
  SwiftPassInvocation swiftPassInvocation;

  // Backlink to the swift PassManager.
  OptionalSwiftObject swiftPassManager = nullptr;

  std::optional<PrettyStackTraceSILModuleTransform> modulePassStackTracer;
  std::optional<PrettyStackTraceSILFunctionTransform> functionPassStackTracer;
  std::optional<DebugPrintEnabler> debugPrintEnabler;
  llvm::sys::TimePoint<> startTime;

  /// If true, passes are also run for functions which have
  /// OptimizationMode::NoOptimization.
  bool isMandatory = false;

  /// The notification handler for this specific SILPassManager.
  ///
  /// This is not owned by the pass manager, it is owned by the SILModule which
  /// is guaranteed to outlive any pass manager associated with it. We keep this
  /// bare pointer to ensure that we can deregister the notification after this
  /// pass manager is destroyed.
  DeserializationNotificationHandler *deserializationNotificationHandler;

public:
  /// C'tor. It creates and registers all analysis passes, which are defined
  /// in Analysis.def.
  SILPassManager(SILModule *M, bool isMandatory, irgen::IRGenModule *IRMod);

  void setSwiftPassManager(OptionalSwiftObject swiftPM) {
    swiftPassManager = swiftPM;
  }

  OptionalSwiftObject getSwiftPassManager() const { return swiftPassManager; }

  const SILOptions &getOptions() const;

  SILTransform *getPass(PassKind Kind);

  /// Searches for an analysis of type T in the list of registered
  /// analysis. If the analysis is not found, the program terminates.
  template<typename T>
  T *getAnalysis() {
    for (SILAnalysis *A : Analyses)
      if (auto *R = llvm::dyn_cast<T>(A))
        return R;

    llvm_unreachable("Unable to find analysis for requested type.");
  }

  template<typename T>
  T *getAnalysis(SILFunction *f) {
    for (SILAnalysis *A : Analyses) {
      if (A->getKind() == T::getAnalysisKind())
        return static_cast<FunctionAnalysisBase<T> *>(A)->get(f);
    }
    llvm_unreachable("Unable to find analysis for requested type.");
  }

  /// \returns the module that the pass manager owns.
  SILModule *getModule() { return Mod; }

  /// \returns the associated IGenModule or null if this is not an IRGen
  /// pass manager.
  irgen::IRGenModule *getIRGenModule();

  SwiftPassInvocation *getSwiftPassInvocation() {
    return &swiftPassInvocation;
  }

  unsigned getMaxNumPassesToRun() const { return maxNumPassesToRun; }
  unsigned getMaxNumSubpassesToRun() const { return maxNumSubpassesToRun; }

  /// Restart the function pass pipeline on the same function
  /// that is currently being processed.
  void restartWithCurrentFunction(SILTransform *T);
  /// Iterate over all analysis and invalidate them.
  void invalidateAllAnalysis() {
    // Invalidate the analysis (unless they are locked)
    for (auto AP : Analyses)
      if (!AP->isLocked())
        AP->invalidate();

    notifyPassHasInvalidated();
  }

  void notifyNewFunction(SILFunction *function);

  /// Add the function \p F to the function pass worklist.
  /// If not null, the function \p DerivedFrom is the function from which \p F
  /// is derived. This is used to avoid an infinite amount of functions pushed
  /// on the worklist (e.g. caused by a bug in a specializing optimization).
  void notifyNewCallee(SILFunction *callee, SILFunction *DerivedFrom);

  /// Iterate over all analysis and notify them of the function.
  ///
  /// This function does not necessarily have to be newly created function. It
  /// is the job of the analysis to make sure no extra work is done if the
  /// particular analysis has been done on the function.
  void notifyAnalysisOfFunction(SILFunction *F) {
    for (auto AP : Analyses) {
      AP->notifyAddedOrModifiedFunction(F);
    }
  }

  /// Broadcast the invalidation of the function to all analysis.
  void invalidateAnalysis(SILFunction *F,
                          SILAnalysis::InvalidationKind K) {
    // Invalidate the analysis (unless they are locked)
    for (auto AP : Analyses)
      if (!AP->isLocked())
        AP->invalidate(F, K);
    
    notifyPassHasInvalidated();
  }

  /// Iterate over all analysis and notify them of a change in witness-
  /// or vtables.
  void invalidateFunctionTables() {
    // Invalidate the analysis (unless they are locked)
    for (auto AP : Analyses)
      if (!AP->isLocked())
        AP->invalidateFunctionTables();

    notifyPassHasInvalidated();
  }

  /// Iterate over all analysis and notify them of a deleted function.
  void notifyWillDeleteFunction(SILFunction *F) {
    // Invalidate the analysis (unless they are locked)
    for (auto AP : Analyses)
      if (!AP->isLocked())
        AP->notifyWillDeleteFunction(F);

    notifyPassHasInvalidated();
  }

  void setDependingOnCalleeBodies();

  /// Reset the state of the pass manager and remove all transformation
  /// owned by the pass manager. Analysis passes will be kept.
  void resetAndRemoveTransformations();

  /// D'tor.
  ~SILPassManager();

  /// Verify all analyses.
  void verifyAnalyses() const;

  /// Precompute all analyses.
  void forcePrecomputeAnalyses(SILFunction *F) {
    for (auto *A : Analyses) {
      A->forcePrecompute(F);
    }
  }

  /// Verify all analyses, limiting the verification to just this one function
  /// if possible.
  ///
  /// Discussion: We leave it up to the analyses to decide how to implement
  /// this. If no override is provided the SILAnalysis should just call the
  /// normal verify method.
  void verifyAnalyses(SILFunction *F) const;

  void executePassPipelinePlan(PassPipelineKind kind);
  void executeCustomPassPipeline(ArrayRef<PassKind> passKinds, bool isMandatory);

  void executePassPipelinePlan(const SILPassPipelinePlan &Plan);

  // Called from the swift pass manager.
  void runBridgedFunctionPass(PassKind passKind, SILFunction *f);
  void runBridgedModulePass(PassKind passKind);
  void preFunctionPassRun(SILFunction *function, StringRef passName, unsigned passIdx);
  int64_t postFunctionPassRun(StringRef passName, unsigned passIdx);
  void preModulePassRun(StringRef passName, unsigned passIdx);
  int64_t postModulePassRun(StringRef passName, unsigned passIdx);

  bool continueWithNextSubpassRun(SILInstruction *forInst, SILFunction *function);

  static bool isPassDisabled(StringRef passName);
  static bool isInstructionPassDisabled(StringRef instName);

  /// Runs the SIL verifier which is implemented in the SwiftCompilerSources.
  void runSwiftFunctionVerification(SILFunction *f);

  /// Runs the SIL verifier which is implemented in the SwiftCompilerSources.
  void runSwiftModuleVerification();

private:
  void parsePassesToRunCount(StringRef countsStr);

  bool doPrintAfter(SILTransform *T, SILFunction *F);

  void execute();

  /// Add a pass of a specific kind.
  void addPass(PassKind Kind);

  /// Run the \p TransIdx'th SIL module transform over all the functions in
  /// the module.
  void runModulePass(SILModuleTransform *SMT, unsigned TransIdx);

  /// Run the \p TransIdx'th pass on the function \p F.
  void runPassOnFunction(SILFunctionTransform *SFT, unsigned TransIdx, SILFunction *F);

  /// Run the passes in Transform from \p FromTransIdx to \p ToTransIdx.
  void runFunctionPasses(unsigned FromTransIdx, unsigned ToTransIdx);

  SILTransform *getCachedPass(PassKind passKind);

  void notifyPassHasInvalidated();

  /// A helper function that returns (based on SIL stage and debug
  /// options) whether we should continue running passes.
  bool continueTransforming();

  /// Return true if all analyses are unlocked.
  bool analysesUnlocked();

  /// Dumps information about the pass with index \p TransIdx to llvm::dbgs().
  void dumpPassInfo(const char *Title, SILTransform *Tr, SILFunction *F,
                    int passIdx = -1);

  /// Dumps information about the pass with index \p TransIdx to llvm::dbgs().
  void dumpPassInfo(const char *Title, unsigned TransIdx,
                    SILFunction *F = nullptr);

};

inline void SwiftPassInvocation::
notifyChanges(SILAnalysis::InvalidationKind invalidationKind) {
    changeNotifications = (SILAnalysis::InvalidationKind)
        (changeNotifications | invalidationKind);
}

} // end namespace swift

#endif
