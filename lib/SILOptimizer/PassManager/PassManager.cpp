//===--- PassManager.cpp - Swift Pass Manager -----------------------------===//
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

#define DEBUG_TYPE "sil-passmanager"

#include "swift/SILOptimizer/PassManager/PassManager.h"
#include "../../IRGen/IRGenModule.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/SILOptimizerRequests.h"
#include "swift/Basic/Assertions.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Demangling/Demangler.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/DynamicCasts.h"
#include "swift/SIL/SILBridging.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Analysis/BasicCalleeAnalysis.h"
#include "swift/SILOptimizer/IPO/ClosureSpecializer.h"
#include "swift/SILOptimizer/OptimizerBridging.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFGOptUtils.h"
#include "swift/SILOptimizer/Utils/ConstantFolding.h"
#include "swift/SILOptimizer/Utils/Devirtualize.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/OptimizerStatsUtils.h"
#include "swift/SILOptimizer/Utils/SILInliner.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "swift/SILOptimizer/Utils/StackNesting.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/GraphWriter.h"
#include "llvm/Support/ManagedStatic.h"
#include "llvm/Support/Casting.h"
#include <fstream>

using namespace swift;

llvm::cl::opt<bool> SILPrintPassName(
    "sil-print-pass-name", llvm::cl::init(false),
    llvm::cl::desc("Print the name of each SIL pass before it runs"));

llvm::cl::opt<bool> SILPrintPassTime(
    "sil-print-pass-time", llvm::cl::init(false),
    llvm::cl::desc("Print the execution time of each SIL pass"));

llvm::cl::opt<bool> SILPrintLast(
    "sil-print-last", llvm::cl::init(false),
    llvm::cl::desc("Print the last optimized function before and after the last pass"));

llvm::cl::opt<std::string> SILNumOptPassesToRun(
    "sil-opt-pass-count", llvm::cl::init(""),
    llvm::cl::desc("Stop optimizing after <N> passes or <N>.<M> passes/sub-passes"));

// Read pass counts for each module from a config file.
// Config file format:
//   <module-name>:<pass-count>(.<sub-pass-count>)?
//
// This is useful for bisecting passes in large projects:
//   1. create a config file from a full build log. E.g. with
//        grep -e '-module-name' build.log  | sed -e 's/.*-module-name \([^ ]*\) .*/\1:10000000/' | sort | uniq > config.txt
//   2. add the `-Xllvm -sil-pass-count-config-file config.txt` option to the project settings
//   3. bisect by modifying the counts in the config file
//   4. clean-rebuild after each bisecting step
llvm::cl::opt<std::string> SILPassCountConfigFile(
    "sil-pass-count-config-file", llvm::cl::init(""),
    llvm::cl::desc("Read optimization counts from file"));

llvm::cl::list<std::string>
    SILPrintFunction("sil-print-function", llvm::cl::CommaSeparated,
                    llvm::cl::desc("Only print out the sil for this function"));

llvm::cl::opt<std::string>
    SILPrintFunctions("sil-print-functions", llvm::cl::init(""),
                     llvm::cl::desc("Only print out the sil for the functions "
                                    "whose name contains this substring"));

llvm::cl::list<std::string>
    SILPrintBefore("sil-print-before", llvm::cl::CommaSeparated,
                   llvm::cl::desc("Print out the sil before passes which "
                                  "contain a string from this list."));

llvm::cl::list<std::string>
    SILPrintAfter("sil-print-after", llvm::cl::CommaSeparated,
                  llvm::cl::desc("Print out the sil after passes which contain "
                                 "a string from this list."));

llvm::cl::list<std::string>
    SILPrintAround("sil-print-around", llvm::cl::CommaSeparated,
                   llvm::cl::desc("Print out the sil before and after passes "
                                  "which contain a string from this list"));

llvm::cl::list<std::string>
    SILDisablePass("sil-disable-pass", llvm::cl::CommaSeparated,
                     llvm::cl::desc("Disable passes "
                                    "which contain a string from this list"));
static llvm::ManagedStatic<std::vector<unsigned>> DebugPassNumbers;

namespace {

struct DebugOnlyPassNumberOpt {
  void operator=(const std::string &Val) const {
    if (Val.empty())
      return;
    SmallVector<StringRef, 8> dbgPassNumbers;
    StringRef(Val).split(dbgPassNumbers, ',', -1, false);
    for (auto dbgPassNumber : dbgPassNumbers) {
      int PassNumber;
      if (dbgPassNumber.getAsInteger(10, PassNumber) || PassNumber < 0)
        llvm_unreachable("The pass number should be an integer number >= 0");
      DebugPassNumbers->push_back(static_cast<unsigned>(PassNumber));
    }
  }
};

} // end anonymous namespace

static DebugOnlyPassNumberOpt DebugOnlyPassNumberOptLoc;

static llvm::cl::opt<DebugOnlyPassNumberOpt, true,
                     llvm::cl::parser<std::string>>
    DebugOnly("debug-only-pass-number",
              llvm::cl::desc("Enable a specific type of debug output (comma "
                             "separated list pass numbers)"),
              llvm::cl::Hidden, llvm::cl::ZeroOrMore,
              llvm::cl::value_desc("pass number"),
              llvm::cl::location(DebugOnlyPassNumberOptLoc),
              llvm::cl::ValueRequired);

static llvm::cl::opt<bool> SILPrintAllSubpasses(
    "sil-print-all-subpasses", llvm::cl::init(false),
    llvm::cl::desc("Print the function before every subpass run of the last pass."
                   "In combination with -sil-opt-pass-count"));

bool isFunctionSelectedForPrinting(SILFunction *F) {
  for (const std::string &printFnName : SILPrintFunction) {
    if (printFnName == F->getName())
      return true;
    if (!printFnName.empty() && printFnName[0] != '$' &&
        !F->getName().empty() && F->getName()[0] == '$' &&
        printFnName == F->getName().drop_front()) {
      return true;
    }
  }

  if (!SILPrintFunctions.empty() && F->getName().contains(SILPrintFunctions))
    return true;

  return false;
}

void printInliningDetails(StringRef passName, SILFunction *caller,
                          SILFunction *callee, bool isCaller,
                          bool alreadyInlined) {
  if (!isFunctionSelectedForPrinting(caller))
    return;
  llvm::dbgs() << "  " << passName
               << (alreadyInlined ? " has inlined " : " will inline ")
               << callee->getName() << " into " << caller->getName() << ".\n";
  auto *printee = isCaller ? caller : callee;
  printee->dump(caller->getModule().getOptions().EmitVerboseSIL);
  llvm::dbgs() << '\n';
}

void printInliningDetailsCallee(StringRef passName, SILFunction *caller,
                                SILFunction *callee) {
  printInliningDetails(passName, caller, callee, /*isCaller=*/false,
                       /*alreadyInlined=*/false);
}

void printInliningDetailsCallerBefore(StringRef passName, SILFunction *caller,
                                      SILFunction *callee) {
  printInliningDetails(passName, caller, callee, /*isCaller=*/true,
                       /*alreadyInlined=*/false);
}

void printInliningDetailsCallerAfter(StringRef passName, SILFunction *caller,
                                     SILFunction *callee) {
  printInliningDetails(passName, caller, callee, /*isCaller=*/true,
                       /*alreadyInlined=*/true);
}

static BridgedPassManager::ExecutePassesFn executePassesFunction = nullptr;
static BridgedPassManager::ExecutePassesFromNameFn executePassesFromNameFunction = nullptr;
static BridgedPassManager::RegisterBridgedModulePassFn registerBridgedModulePassFunction = nullptr;
static BridgedPassManager::RegisterBridgedFunctionPassFn registerBridgedFunctionPassFunction = nullptr;
static BridgedPassManager::NotifyNewFunctionFn notifyNewFunctionFunction = nullptr;
static BridgedPassManager::NotifyNewCalleeFn notifyNewCalleeFunction = nullptr;
static BridgedPassManager::ContinueWithSubpassFn continueWithSubpassFunction = nullptr;
static BridgedPassManager::NotifyFn notifyPassHasInvalidatedFunction = nullptr;
static BridgedPassManager::NotifyFn notifyDepdendencyFunction = nullptr;
static BridgedPassManager::NotifyFn notifyRestartPipelineFunction = nullptr;

void BridgedPassManager::registerBridging(ExecutePassesFn executePassesFn,
                                          ExecutePassesFromNameFn executePassesFromNameFn,
                                          RegisterBridgedModulePassFn registerBridgedModulePassFn,
                                          RegisterBridgedFunctionPassFn registerBridgedFunctionPassFn,
                                          NotifyNewFunctionFn notifyNewFunctionFn,
                                          NotifyNewCalleeFn notifyNewCalleeFn,
                                          ContinueWithSubpassFn continueWithSubpassFn,
                                          NotifyFn notifyPassHasInvalidatedFn,
                                          NotifyFn notifyDepdendencyFn,
                                          NotifyFn notifyRestartPipelineFn) {
  executePassesFunction = executePassesFn;
  executePassesFromNameFunction = executePassesFromNameFn;
  registerBridgedModulePassFunction = registerBridgedModulePassFn;
  registerBridgedFunctionPassFunction = registerBridgedFunctionPassFn;
  notifyNewFunctionFunction = notifyNewFunctionFn;
  notifyNewCalleeFunction = notifyNewCalleeFn;
  continueWithSubpassFunction = continueWithSubpassFn;
  notifyPassHasInvalidatedFunction = notifyPassHasInvalidatedFn;
  notifyDepdendencyFunction = notifyDepdendencyFn;
  notifyRestartPipelineFunction = notifyRestartPipelineFn;
}

static bool functionSelectionEmpty() {
  return SILPrintFunction.empty() && SILPrintFunctions.empty();
}

bool SILPassManager::doPrintAfter(SILTransform *T, SILFunction *F) {
  if (F && !isFunctionSelectedForPrinting(F))
    return false;

  auto MatchFun = [&](const std::string &Str) -> bool {
    return T->getTag().contains(Str) || T->getID().contains(Str);
  };

  if (SILPrintAfter.end() !=
      std::find_if(SILPrintAfter.begin(), SILPrintAfter.end(), MatchFun))
    return true;
  if (!SILPrintAfter.empty())
    return false;

  if (SILPrintAround.end() !=
      std::find_if(SILPrintAround.begin(), SILPrintAround.end(), MatchFun))
    return true;
  if (!SILPrintAround.empty())
    return false;

  return !functionSelectionEmpty();
}

DebugPrintEnabler::DebugPrintEnabler(unsigned PassNumber) {
#ifndef NDEBUG
  OldDebugFlag = llvm::DebugFlag;
  if (llvm::DebugFlag)
    return;
  if (DebugPassNumbers->empty())
    return;
  // Enable debug printing if the pass number matches
  // one of the pass numbers provided as a command line option.
  for (auto DebugPassNumber : *DebugPassNumbers) {
    if (DebugPassNumber == PassNumber) {
      llvm::DebugFlag = true;
      return;
    }
  }
#endif
}

DebugPrintEnabler::~DebugPrintEnabler() {
#ifndef NDEBUG
  llvm::DebugFlag = OldDebugFlag;
#endif
}

//===----------------------------------------------------------------------===//
//                 Serialization Notification Implementation
//===----------------------------------------------------------------------===//

namespace {

class PassManagerDeserializationNotificationHandler final
    : public DeserializationNotificationHandler {
  NullablePtr<SILPassManager> pm;

public:
  PassManagerDeserializationNotificationHandler(SILPassManager *pm) : pm(pm) {}
  ~PassManagerDeserializationNotificationHandler() override = default;

  StringRef getName() const override {
    return "PassManagerDeserializationNotificationHandler";
  }

  /// Observe that we deserialized a function declaration.
  void didDeserialize(ModuleDecl *mod, SILFunction *fn) override {
    pm.get()->notifyNewFunction(fn);
    pm.get()->notifyAnalysisOfFunction(fn);
  }
};

} // end anonymous namespace

evaluator::SideEffect ExecuteSILPipelineRequest::evaluate(
    Evaluator &evaluator, SILPipelineExecutionDescriptor desc) const {
  SILPassManager PM(desc.SM, desc.IsMandatory, desc.IRMod);
  PM.executePassPipelinePlan(desc.planKind);
  return std::make_tuple<>();
}

void swift::executePasses(ArrayRef<PassKind> passKinds,
                          SILModule *mod, irgen::IRGenModule *IRMod,
                          bool isMandatory) {
  SILPassManager pm(mod, isMandatory, IRMod);
  pm.executeCustomPassPipeline(passKinds, isMandatory);
}

void swift::executePassPipelinePlan(SILModule *SM,
                                    PassPipelineKind kind,
                                    bool isMandatory,
                                    irgen::IRGenModule *IRMod) {
  auto &evaluator = SM->getASTContext().evaluator;
  SILPipelineExecutionDescriptor desc{SM, kind, isMandatory, IRMod};
  (void)evaluateOrFatal(evaluator, ExecuteSILPipelineRequest{desc});
}

SILPassManager::SILPassManager(SILModule *M, bool isMandatory,
                               irgen::IRGenModule *IRMod)
    : Mod(M), IRMod(IRMod), irgen(nullptr),
      swiftPassInvocation(this),
      isMandatory(isMandatory), deserializationNotificationHandler(nullptr) {
#define SIL_ANALYSIS(NAME) \
  Analyses.push_back(create##NAME##Analysis(Mod));
#include "swift/SILOptimizer/Analysis/Analysis.def"

  if (!SILNumOptPassesToRun.empty()) {
    parsePassesToRunCount(SILNumOptPassesToRun);
  } else if (!SILPassCountConfigFile.empty()) {
    StringRef moduleName = M->getSwiftModule()->getName().str();
    std::fstream fs(SILPassCountConfigFile);
    if (!fs) {
      llvm::errs() << "cannot open pass count config file\n";
      exit(1);
    }
    std::string line;
    while (std::getline(fs, line)) {
      auto pair = StringRef(line).split(":");
      StringRef modName = pair.first;
      StringRef countsStr = pair.second;
      if (modName == moduleName) {
        parsePassesToRunCount(countsStr);
        break;
      }
    }
    fs.close();
  }

  for (SILAnalysis *A : Analyses) {
    A->initialize(this);
  }

  std::unique_ptr<DeserializationNotificationHandler> handler(
      new PassManagerDeserializationNotificationHandler(this));
  deserializationNotificationHandler = handler.get();
  M->registerDeserializationNotificationHandler(std::move(handler));
}

void SILPassManager::parsePassesToRunCount(StringRef countsStr) {
  bool validFormat = true;
  if (countsStr.consumeInteger(10, maxNumPassesToRun))
    validFormat = false;
  if (countsStr.starts_with(".")) {
    countsStr = countsStr.drop_front(1);
    if (countsStr.consumeInteger(10, maxNumSubpassesToRun))
      validFormat = false;
  }
  if (!validFormat || !countsStr.empty()) {
    llvm::errs() << "error: wrong format of -sil-opt-pass-count option\n";
    exit(1);
  }
}

bool SILPassManager::continueTransforming() {
  if (isMandatory)
    return true;
  return NumPassesRun < maxNumPassesToRun;
}

bool SILPassManager::continueWithNextSubpassRun(SILInstruction *forInst,
                                                SILFunction *function) {
  if (continueWithSubpassFunction) {
    if (forInst) {
      return continueWithSubpassFunction({this}, {function}, {forInst->asSILNode()});
    } else {
      return continueWithSubpassFunction({this}, {function}, {nullptr});
    }
  }
  return true;
}

bool SILPassManager::analysesUnlocked() {
  for (auto *A : Analyses)
    if (A->isLocked())
      return false;

  return true;
}

void SILPassManager::dumpPassInfo(const char *Title, SILTransform *Tr,
                                  SILFunction *F, int passIdx) {
  llvm::dbgs() << "  " << Title << " #" << NumPassesRun << ", pass";
  if (passIdx >= 0)
    llvm::dbgs() << ' ' << passIdx;
  llvm::dbgs() << ": " << Tr->getID() << " (" << Tr->getTag() << ")";
  if (F)
    llvm::dbgs() << ", Function: " << F->getName();
  llvm::dbgs() << '\n';
}

void SILPassManager::dumpPassInfo(const char *Title, unsigned TransIdx,
                                  SILFunction *F) {
  dumpPassInfo(Title, Transformations[TransIdx], F, (int)TransIdx);
}

static bool isDisabled(SILTransform *T) {
  if (SILDisablePass.empty())
    return false;

  if (SILPassManager::isPassDisabled(T->getTag()) ||
      SILPassManager::isPassDisabled(T->getID())) {
    return true;
  }
  return false;
}

bool SILPassManager::isInstructionPassDisabled(StringRef instName) {
  StringRef prefix("simplify-");
  for (const std::string &namePattern : SILDisablePass) {
    StringRef pattern(namePattern);
    if (pattern.starts_with(prefix) && pattern.ends_with(instName) &&
        pattern.size() == prefix.size() + instName.size()) {
      return true;
    }
  }
  return false;
}

void SILPassManager::runPassOnFunction(SILFunctionTransform *SFT, unsigned TransIdx, SILFunction *F) {

  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");

  if (!F->shouldOptimize()) {
    return;
  }

  SFT->injectPassManager(this);
  SFT->injectFunction(F);

  PrettyStackTraceSILFunctionTransform X(F, SFT->getTag(), NumPassesRun);

  if (isDisabled(SFT)) {
    return;
  }

  if (SILPrintPassName)
    dumpPassInfo("Run", TransIdx, F);

  SFT->run();

  Mod->flushDeletedInsts();

  // If this pass invalidated anything, print and verify.
  if (doPrintAfter(SFT, F)) {
    dumpPassInfo("*** SIL function after ", TransIdx);
    F->dump(getOptions().EmitVerboseSIL);
  }

  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");

  if (getOptions().VerifyAll) {
    F->verify(getAnalysis<BasicCalleeAnalysis>()->getCalleeCache());
    verifyAnalyses(F);
    runSwiftFunctionVerification(F);
  }

  ++NumPassesRun;
}

void SILPassManager::
runFunctionPasses(unsigned FromTransIdx, unsigned ToTransIdx) {
  if (ToTransIdx <= FromTransIdx)
    return;

  for (SILFunction &f : *getModule()) {
    if (!continueTransforming())
      return;

    for (unsigned transIdx = FromTransIdx; transIdx < ToTransIdx; ++transIdx) {
      auto *sft = cast<SILFunctionTransform>(Transformations[transIdx]);
      runPassOnFunction(sft, transIdx, &f);
    }
  }
}

void SILPassManager::runModulePass(SILModuleTransform *SMT, unsigned TransIdx) {
  if (isDisabled(SMT))
    return;

  const SILOptions &Options = getOptions();

  SMT->injectPassManager(this);
  SMT->injectModule(Mod);

  PrettyStackTraceSILModuleTransform X(SMT->getID(), NumPassesRun);

  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
  SMT->run();
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
  Mod->flushDeletedInsts();

  // If this pass invalidated anything, print and verify.
  if (doPrintAfter(SMT, nullptr)) {
    dumpPassInfo("*** SIL module after", TransIdx);
    Mod->dump();
  }

  if (Options.VerifyAll) {
    Mod->verify(getAnalysis<BasicCalleeAnalysis>()->getCalleeCache());
    verifyAnalyses();
    runSwiftModuleVerification();
  }
}

void SILPassManager::verifyAnalyses() const {
  if (Mod->getOptions().VerifyNone)
    return;

  for (auto *A : Analyses) {
    A->verify();
  }
}

void SILPassManager::verifyAnalyses(SILFunction *F) const {
  if (Mod->getOptions().VerifyNone)
    return;
    
  for (auto *A : Analyses) {
    A->verify(F);
  }
}

void SILPassManager::executePassPipelinePlan(PassPipelineKind kind) {
  if (executePassesFunction) {
    executePassesFunction({this}, (BridgedPassManager::PassPipelineKind)kind);
  } else {
    SILPassPipelinePlan plan = SILPassPipelinePlan::getPlan(kind, getModule());
    executePassPipelinePlan(plan);
  }
}

void SILPassManager::executeCustomPassPipeline(ArrayRef<PassKind> passKinds, bool isMandatory) {
  llvm::SmallVector<BridgedStringRef, 8> passNames;
  for (PassKind kind : passKinds) {
    passNames.push_back(PassKindTag(kind));
  }
  ASSERT(executePassesFromNameFunction != nullptr && "executing custom passes requires SwiftCompilerSources");
  executePassesFromNameFunction({this}, BridgedArrayRef(ArrayRef(passNames)), isMandatory);
}

void SILPassManager::executePassPipelinePlan(const SILPassPipelinePlan &Plan) {
  for (const SILPassPipeline &Pipeline : Plan.getPipelines()) {
    resetAndRemoveTransformations();
    for (PassKind Kind : Plan.getPipelinePasses(Pipeline)) {
      addPass(Kind);
      assert(!Pipeline.isFunctionPassPipeline
             || isa<SILFunctionTransform>(Transformations.back()));
    }
    execute();
  }
}

SILTransform *SILPassManager::getCachedPass(PassKind passKind) {
  unsigned passIdx = (unsigned)passKind;
  if (allTransformations.empty()) {
    allTransformations.resize((unsigned)PassKind::AllPasses_Last + 1);
  }
  if (!allTransformations[passIdx]) {
    allTransformations[passIdx] = getPass(passKind);
  }
  return allTransformations[passIdx];
}

void SILPassManager::runBridgedFunctionPass(PassKind passKind, SILFunction *f) {
  auto *sft = cast<SILFunctionTransform>(getCachedPass(passKind));
  sft->injectPassManager(this);
  sft->injectFunction(f);
  sft->run();
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
}

void SILPassManager::runBridgedModulePass(PassKind passKind) {
  auto *smt = cast<SILModuleTransform>(getCachedPass(passKind));
  smt->injectPassManager(this);
  smt->injectModule(Mod);
  smt->run();
  assert(analysesUnlocked() && "Expected all analyses to be unlocked!");
}

void SILPassManager::preFunctionPassRun(SILFunction *function, StringRef passName, unsigned passIdx) {
  ASSERT(!functionPassStackTracer.has_value());
  ASSERT(!debugPrintEnabler.has_value());
  functionPassStackTracer.emplace(function, passName, passIdx);
  debugPrintEnabler.emplace(passIdx);
  swiftPassInvocation.startFunctionPassRun(function);
  startTime = std::chrono::system_clock::now();
}

int64_t SILPassManager::postFunctionPassRun(StringRef passName, unsigned passIdx) {
  swiftPassInvocation.finishedFunctionPassRun();
  functionPassStackTracer.reset();
  debugPrintEnabler.reset();
  Mod->flushDeletedInsts();
  std::chrono::nanoseconds duration = std::chrono::system_clock::now() - startTime;
  updateSILModuleStatsAfterTransform(*Mod, passName, *this, passIdx, duration.count());
  return (int64_t)duration.count() / 1000000;
}

void SILPassManager::preModulePassRun(StringRef passName, unsigned passIdx) {
  ASSERT(!modulePassStackTracer.has_value());
  ASSERT(!debugPrintEnabler.has_value());
  modulePassStackTracer.emplace(passName, passIdx);
  debugPrintEnabler.emplace(passIdx);
  swiftPassInvocation.startModulePassRun();
  startTime = std::chrono::system_clock::now();
}

int64_t SILPassManager::postModulePassRun(StringRef passName, unsigned passIdx) {
  swiftPassInvocation.finishedModulePassRun();
  modulePassStackTracer.reset();
  debugPrintEnabler.reset();
  Mod->flushDeletedInsts();
  std::chrono::nanoseconds duration = std::chrono::system_clock::now() - startTime;
  updateSILModuleStatsAfterTransform(*Mod, passName, *this, passIdx, duration.count());
  return (int64_t)duration.count() / 1000000;
}

void SILPassManager::execute() {
  LLVM_DEBUG(llvm::dbgs() << "*** Optimizing the module *** \n");
  // Run the transforms by alternating between function transforms and
  // module transforms. We'll queue up all the function transforms
  // that we see in a row and then run the entire group of transforms
  // on each function in turn. Then we move on to running the next set
  // of consecutive module transforms.
  unsigned Idx = 0, NumTransforms = Transformations.size();

  while (Idx < NumTransforms && continueTransforming()) {
    SILTransform *Tr = Transformations[Idx];
    assert((isa<SILFunctionTransform>(Tr) || isa<SILModuleTransform>(Tr)) &&
           "Unexpected pass kind!");
    (void)Tr;

    unsigned FirstFuncTrans = Idx;
    while (Idx < NumTransforms && isa<SILFunctionTransform>(Transformations[Idx]))
      ++Idx;

    runFunctionPasses(FirstFuncTrans, Idx);

    while (Idx < NumTransforms && isa<SILModuleTransform>(Transformations[Idx])
           && continueTransforming()) {
      auto *smt = cast<SILModuleTransform>(Transformations[Idx]);
      runModulePass(smt, Idx);

      ++Idx;
      ++NumPassesRun;
    }
  }
}

irgen::IRGenModule *SILPassManager::getIRGenModule() {
  // We need an IRGenModule to get the actual sizes from type lowering.
  // Creating an IRGenModule involves some effort, let's cache it for the
  // whole pass.
  if (IRMod == nullptr) {
    SILModule *module = getModule();

    auto *irgenOpts = module->getIRGenOptionsOrNull();
    if (!irgenOpts)
      return nullptr;

    if (irgen == nullptr)
      irgen = new irgen::IRGenerator(*irgenOpts, *module);
    auto targetMachine = irgen->createTargetMachine();
    assert(targetMachine && "failed to create target");
    IRMod = new irgen::IRGenModule(*irgen, std::move(targetMachine));
  }

  return IRMod;
}

/// D'tor.
SILPassManager::~SILPassManager() {

  // Before we do anything further, verify the module and our analyses. These
  // are natural points with which to verify.
  //
  // TODO: We currently do not verify the module here since the verifier asserts
  // in the normal build. This should be enabled and those problems resolved
  // either by changing the verifier or treating those asserts as signs of a
  // bug.
  for (auto *A : Analyses) {
    // We use verify full instead of just verify to ensure that passes that want
    // to run more expensive verification after a pass manager is destroyed
    // properly trigger.
    //
    // NOTE: verifyFull() has a default implementation that just calls
    // verify(). So functionally, there is no difference here.
    A->verifyFull();
  }

  // Remove our deserialization notification handler.
  Mod->removeDeserializationNotificationHandler(
      deserializationNotificationHandler);

  // Free all transformations.
  for (auto *T : Transformations)
    delete T;

  // delete the analysis.
  for (auto *A : Analyses) {
    assert(!A->isLocked() &&
           "Deleting a locked analysis. Did we forget to unlock ?");
    delete A;
  }

  if (irgen) {
    // If irgen is set, we also own the IRGenModule
    if (IRMod) {
      delete IRMod;
      IRMod = nullptr;
    }
    delete irgen;
    irgen = nullptr;
  }
}

void SILPassManager::notifyPassHasInvalidated() {
  if (notifyPassHasInvalidatedFunction)
    notifyPassHasInvalidatedFunction({this});
}

void SILPassManager::setDependingOnCalleeBodies() {
  if (notifyDepdendencyFunction)
    notifyDepdendencyFunction({this});
}

void SILPassManager::notifyNewFunction(SILFunction *function) {
  if (notifyNewFunctionFunction) {
    notifyNewFunctionFunction({this}, {function});
  }
}

void SILPassManager::notifyNewCallee(SILFunction *callee, SILFunction *derivedFrom) {
  if (notifyNewCalleeFunction) {
    notifyNewCalleeFunction({this}, {callee}, {derivedFrom});
  }
}

void SILPassManager::restartWithCurrentFunction(SILTransform *T) {
  assert(isa<SILFunctionTransform>(T) &&
         "Can only restart the pipeline from function passes");
  if (notifyRestartPipelineFunction) {
    notifyRestartPipelineFunction({this});
  }
}

/// Reset the state of the pass manager and remove all transformation
/// owned by the pass manager. Analysis passes will be kept.
void SILPassManager::resetAndRemoveTransformations() {
  for (auto *T : Transformations)
    delete T;

  Transformations.clear();
}

const SILOptions &SILPassManager::getOptions() const {
  return Mod->getOptions();
}

namespace {
enum class IRGenPasses : uint8_t {
#define PASS(ID, TAG, NAME)
#define IRGEN_PASS(ID, TAG, NAME) ID,
#include "swift/SILOptimizer/PassManager/Passes.def"
};
} // end anonymous namespace

void SILPassManager::addPass(PassKind kind) {
  Transformations.push_back(getPass(kind));
}

SILTransform *SILPassManager::getPass(PassKind Kind) {
  assert(unsigned(PassKind::AllPasses_Last) >= unsigned(Kind) &&
         "Invalid pass kind");
  switch (Kind) {
#define PASS(ID, TAG, NAME)                                                    \
  case PassKind::ID: {                                                         \
    SILTransform *T = swift::create##ID();                                     \
    T->setPassKind(PassKind::ID);                                              \
    return T;                                                                  \
  }
#define IRGEN_PASS(ID, TAG, NAME)                                              \
  case PassKind::ID: {                                                         \
    auto &ctx = Mod->getASTContext();                                          \
    auto irPasses = ctx.getIRGenSILTransforms();                               \
    SILTransform *T = irPasses[static_cast<unsigned>(IRGenPasses::ID)]();      \
    assert(T && "Missing IRGen pass?");                                        \
    T->setPassKind(PassKind::ID);                                              \
    return T;                                                                  \
  }
#include "swift/SILOptimizer/PassManager/Passes.def"
  default:
    llvm_unreachable("invalid pass kind");
  }
}

//===----------------------------------------------------------------------===//
//                           SwiftPassInvocation
//===----------------------------------------------------------------------===//

FixedSizeSlab *SwiftPassInvocation::allocSlab(FixedSizeSlab *afterSlab) {
  FixedSizeSlab *slab = passManager->getModule()->allocSlab();
  if (afterSlab) {
    allocatedSlabs.insert(std::next(afterSlab->getIterator()), *slab);
  } else {
    allocatedSlabs.push_back(*slab);
  }
  return slab;
}

FixedSizeSlab *SwiftPassInvocation::freeSlab(FixedSizeSlab *slab) {
  FixedSizeSlab *prev = nullptr;
  assert(!allocatedSlabs.empty());
  if (&allocatedSlabs.front() != slab)
    prev = &*std::prev(slab->getIterator());

  allocatedSlabs.remove(*slab);
  passManager->getModule()->freeSlab(slab);
  return prev;
}

BasicBlockSet *SwiftPassInvocation::allocBlockSet() {
  ASSERT(numBlockSetsAllocated < BlockSetCapacity &&
         "too many BasicBlockSets allocated");

  auto *storage = (BasicBlockSet *)blockSetStorage + numBlockSetsAllocated;
  BasicBlockSet *set = new (storage) BasicBlockSet(function);
  aliveBlockSets[numBlockSetsAllocated] = true;
  ++numBlockSetsAllocated;
  return set;
}

void SwiftPassInvocation::freeBlockSet(BasicBlockSet *set) {
  int idx = set - (BasicBlockSet *)blockSetStorage;
  assert(idx >= 0 && idx < numBlockSetsAllocated);
  assert(aliveBlockSets[idx] && "double free of BasicBlockSet");
  aliveBlockSets[idx] = false;

  while (numBlockSetsAllocated > 0 && !aliveBlockSets[numBlockSetsAllocated - 1]) {
    auto *set = (BasicBlockSet *)blockSetStorage + numBlockSetsAllocated - 1;
    set->~BasicBlockSet();
    --numBlockSetsAllocated;
  }
}

NodeSet *SwiftPassInvocation::allocNodeSet() {
  ASSERT(numNodeSetsAllocated < NodeSetCapacity &&
         "too many NodeSets allocated");

  auto *storage = (NodeSet *)nodeSetStorage + numNodeSetsAllocated;
  NodeSet *set = new (storage) NodeSet(function);
  aliveNodeSets[numNodeSetsAllocated] = true;
  ++numNodeSetsAllocated;
  return set;
}

void SwiftPassInvocation::freeNodeSet(NodeSet *set) {
  int idx = set - (NodeSet *)nodeSetStorage;
  assert(idx >= 0 && idx < numNodeSetsAllocated);
  assert(aliveNodeSets[idx] && "double free of NodeSet");
  aliveNodeSets[idx] = false;

  while (numNodeSetsAllocated > 0 && !aliveNodeSets[numNodeSetsAllocated - 1]) {
    auto *set = (NodeSet *)nodeSetStorage + numNodeSetsAllocated - 1;
    set->~NodeSet();
    --numNodeSetsAllocated;
  }
}

OperandSet *SwiftPassInvocation::allocOperandSet() {
  ASSERT(numOperandSetsAllocated < OperandSetCapacity &&
         "too many OperandSets allocated");

  auto *storage = (OperandSet *)operandSetStorage + numOperandSetsAllocated;
  OperandSet *set = new (storage) OperandSet(function);
  aliveOperandSets[numOperandSetsAllocated] = true;
  ++numOperandSetsAllocated;
  return set;
}

void SwiftPassInvocation::freeOperandSet(OperandSet *set) {
  int idx = set - (OperandSet *)operandSetStorage;
  assert(idx >= 0 && idx < numOperandSetsAllocated);
  assert(aliveOperandSets[idx] && "double free of OperandSet");
  aliveOperandSets[idx] = false;

  while (numOperandSetsAllocated > 0 && !aliveOperandSets[numOperandSetsAllocated - 1]) {
    auto *set = (OperandSet *)operandSetStorage + numOperandSetsAllocated - 1;
    set->~OperandSet();
    --numOperandSetsAllocated;
  }
}

void SwiftPassInvocation::startModulePassRun() {
  assert(!isTransformingFunction && "a function pass is already running");
  assert(!this->function && "a pass is already running");
}

void SwiftPassInvocation::startFunctionPassRun(SILFunction *function) {
  beginTransformFunction(function);
}

void SwiftPassInvocation::startInstructionPassRun(SILInstruction *inst) {
  assert(inst->getFunction() == function &&
         "running instruction pass on wrong function");
}

void SwiftPassInvocation::finishedModulePassRun() {
  endPass();
  assert(!isTransformingFunction && "still transforming function");
  assert(!function && "not running a pass");
  assert(changeNotifications == SILAnalysis::InvalidationKind::Nothing
         && "unhandled change notifications at end of module pass");
}

void SwiftPassInvocation::finishedFunctionPassRun() {
  endPass();
  endTransformFunction();
}

void SwiftPassInvocation::finishedInstructionPassRun() {
  endPass();
}

irgen::IRGenModule *SwiftPassInvocation::getIRGenModule() {
  return passManager->getIRGenModule();
}

void SwiftPassInvocation::endPass() {
  assert(allocatedSlabs.empty() && "StackList is leaking slabs");
  assert(numBlockSetsAllocated == 0 && "Not all BasicBlockSets deallocated");
  assert(numNodeSetsAllocated == 0 && "Not all NodeSets deallocated");
  assert(numOperandSetsAllocated == 0 && "Not all OperandSets deallocated");
  assert(numClonersAllocated == 0 && "Not all cloners deallocated");
  assert(!needFixStackNesting && "Stack nesting not fixed");
  if (ssaUpdater) {
    delete ssaUpdater;
    ssaUpdater = nullptr;
  }
}

void SwiftPassInvocation::beginTransformFunction(SILFunction *function) {
  assert(!isTransformingFunction && "a pass is already running");
  isTransformingFunction = true;
  assert(!this->function && "not running a function pass");
  this->function = function;
  assert(changeNotifications == SILAnalysis::InvalidationKind::Nothing
         && "change notifications not cleared");
}

void SwiftPassInvocation::endTransformFunction() {
  assert(isTransformingFunction && "not running a function pass");
  isTransformingFunction = false;
  assert(function && "not running a function pass");
  if (changeNotifications != SILAnalysis::InvalidationKind::Nothing) {
    passManager->invalidateAnalysis(function, changeNotifications);
    changeNotifications = SILAnalysis::InvalidationKind::Nothing;
  }
  function = nullptr;
  assert(numBlockSetsAllocated == 0 && "Not all BasicBlockSets deallocated");
  assert(numNodeSetsAllocated == 0 && "Not all NodeSets deallocated");
  assert(numOperandSetsAllocated == 0 && "Not all OperandSets deallocated");
}

void SwiftPassInvocation::beginVerifyFunction(SILFunction *function) {
  if (isTransformingFunction) {
    assert(this->function == function);
  } else {
    assert(!this->function);
    this->function = function;
  }
}

void SwiftPassInvocation::endVerifyFunction() {
  assert(function);
  if (!isTransformingFunction) {
    assert(changeNotifications == SILAnalysis::InvalidationKind::Nothing &&
           "verifyication must not change the SIL of a function");
    assert(numBlockSetsAllocated == 0 && "Not all BasicBlockSets deallocated");
    assert(numNodeSetsAllocated == 0 && "Not all NodeSets deallocated");
    assert(numOperandSetsAllocated == 0 && "Not all OperandSets deallocated");
    function = nullptr;
  }
}

SwiftPassInvocation::~SwiftPassInvocation() {}

//===----------------------------------------------------------------------===//
//                           SIL Bridging
//===----------------------------------------------------------------------===//

bool BridgedFunction::isTrapNoReturn() const {
  return swift::isTrapNoReturnFunction(getFunction());
}

bool BridgedFunction::isAutodiffVJP() const {
  return swift::isDifferentiableFuncComponent(
      getFunction(), swift::AutoDiffFunctionComponent::VJP);
}

SwiftInt BridgedFunction::specializationLevel() const {
  return swift::getSpecializationLevel(getFunction());
}

//===----------------------------------------------------------------------===//
//                           OptimizerBridging
//===----------------------------------------------------------------------===//

static swift::PassKind getFunctionPassKind(BridgedPass kind) {
  switch (kind) {
#define PASS(ID, TAG, NAME) case BridgedPass::ID: return swift::PassKind::ID;
#define MODULE_PASS(ID, TAG, NAME)
#define IRGEN_MODULE_PASS(ID, TAG, NAME)
#include "swift/SILOptimizer/PassManager/Passes.def"
    default:
      llvm_unreachable("invalid BridgedPass kind");
  }
}

static swift::PassKind getModulePassKind(BridgedModulePass kind) {
  switch (kind) {
#define PASS(ID, TAG, NAME)
#define MODULE_PASS(ID, TAG, NAME) case BridgedModulePass::ID: return swift::PassKind::ID;
#define IRGEN_MODULE_PASS MODULE_PASS
#include "swift/SILOptimizer/PassManager/Passes.def"
    default:
      llvm_unreachable("invalid BridgedPass kind");
  }
}

void BridgedPassManager::registerBridgedPasses() {
  for (int i = 0; i < (int)BridgedModulePass::lastKind; i++) {
    auto bridgedKind = (BridgedModulePass)i;
    PassKind kind = getModulePassKind(bridgedKind);
    StringRef passName = PassKindTag(kind);
    registerBridgedModulePassFunction(BridgedStringRef(passName), bridgedKind);
  }
  for (int i = 0; i < (int)BridgedPass::lastKind; i++) {
    auto bridgedKind = (BridgedPass)i;
    PassKind kind = getFunctionPassKind(bridgedKind);
    StringRef passName = PassKindTag(kind);
    registerBridgedFunctionPassFunction(BridgedStringRef(passName), bridgedKind);
  }
}

void BridgedPassManager::setSwiftPassManager(OptionalSwiftObject passManager) const {
  pm->setSwiftPassManager(passManager);
  if (!passManager) {
    fflush(stdout);
  }
}

void BridgedPassManager::runBridgedFunctionPass(BridgedPass passKind, BridgedFunction f) const {
  pm->runBridgedFunctionPass(getFunctionPassKind(passKind), f.getFunction());
}

void BridgedPassManager::runBridgedModulePass(BridgedModulePass passKind) const {
  pm->runBridgedModulePass(getModulePassKind(passKind));
}

bool BridgedPassManager::shouldPrintPassNames() const {
  return SILPrintPassName;
}

bool BridgedPassManager::shouldPrintPassTimes() const {
  return SILPrintPassTime;
}

bool BridgedPassManager::shouldPrintLast() const {
  return SILPrintLast;
}

bool BridgedPassManager::shouldPrintAllSubpasses() const {
  return SILPrintAllSubpasses;
}

bool BridgedPassManager::anyPassOptionSet() const {
  return !SILDisablePass.empty() ||
         !SILPrintFunction.empty() ||
         !SILPrintBefore.empty() ||
         !SILPrintAfter.empty();
}

bool BridgedPassManager::shouldVerifyAfterAllChanges() const {
  return pm->getOptions().VerifyAll;
}

bool BridgedPassManager::isPassDisabled(BridgedStringRef passName) const {
  return SILPassManager::isPassDisabled(passName.unbridged());
}

static PassKind getKindFromPassName(StringRef Name) {
  PassKind k = llvm::StringSwitch<PassKind>(Name)
#define PASS(ID, TAG, NAME) .Case(#ID, PassKind::ID)
#include "swift/SILOptimizer/PassManager/Passes.def"
    .Default(PassKind::invalidPassKind);

  return k;
}

static bool isContainedIn(StringRef passName, const llvm::cl::list<std::string> &option) {
  for (const std::string &s : option) {
    if (passName.contains(s))
      return true;
    if (s.size() > 0 && isupper(s[0])) {
      PassKind k = getKindFromPassName(s);
      if (k != PassKind::invalidPassKind) {
        StringRef tag = PassKindTag(k);
        if (passName.contains(tag))
          return true;
      }
    }
  }
  return false;
}

bool BridgedPassManager::shouldPrintBefore(BridgedStringRef passName) const {
  StringRef name = passName.unbridged();
  return isContainedIn(name, SILPrintBefore) || isContainedIn(name, SILPrintAround);
}

bool BridgedPassManager::shouldPrintAfter(BridgedStringRef passName) const {
  StringRef name = passName.unbridged();
  return isContainedIn(name, SILPrintAfter) || isContainedIn(name, SILPrintAround);
}

bool SILPassManager::isPassDisabled(StringRef passName) {
  return isContainedIn(passName, SILDisablePass);
}

bool BridgedPassManager::shouldPrintAnyFunction() const {
  return !functionSelectionEmpty();
}

bool BridgedPassManager::shouldPrintFunction(BridgedFunction function) const {
  return isFunctionSelectedForPrinting(function.getFunction());
}

BridgedStringRef BridgedPassManager::getPassName(BridgedPass passKind) {
  return PassKindTag(getFunctionPassKind(passKind));
}

BridgedStringRef BridgedPassManager::getPassName(BridgedModulePass passKind) {
  return PassKindTag(getModulePassKind(passKind));
}

llvm::cl::list<std::string>
    SimplifyInstructionTest("simplify-instruction", llvm::cl::CommaSeparated,
                     llvm::cl::desc("Simplify instruction of specified kind(s)"));

#ifdef PURE_BRIDGING_MODE
// In PURE_BRIDGING_MODE, briding functions are not inlined and therefore inluded in the cpp file.
#include "swift/SILOptimizer/OptimizerBridgingImpl.h"
#endif

void BridgedChangeNotificationHandler::notifyChanges(Kind changeKind) const {
  switch (changeKind) {
  case Kind::instructionsChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::Instructions);
    break;
  case Kind::callsChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::CallsAndInstructions);
    break;
  case Kind::branchesChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::BranchesAndInstructions);
    break;
  case Kind::effectsChanged:
    invocation->notifyChanges(SILAnalysis::InvalidationKind::Effects);
    break;
  }
}

BridgedOwnedString BridgedPassContext::getModuleDescription() const {
  std::string str;
  llvm::raw_string_ostream os(str);
  invocation->getPassManager()->getModule()->print(os);
  str.pop_back(); // Remove trailing newline.
  return str;
}

bool BridgedPassContext::tryOptimizeApplyOfPartialApply(BridgedInstruction closure) const {
  auto *pa = closure.getAs<PartialApplyInst>();
  SILBuilder builder(pa);
  return ::tryOptimizeApplyOfPartialApply(pa, builder.getBuilderContext(), InstModCallbacks());
}

bool BridgedPassContext::tryDeleteDeadClosure(BridgedInstruction closure, bool needKeepArgsAlive) const {
  return ::tryDeleteDeadClosure(closure.getAs<SingleValueInstruction>(), InstModCallbacks(), needKeepArgsAlive);
}

BridgedPassContext::DevirtResult BridgedPassContext::tryDevirtualizeApply(BridgedInstruction apply,
                                                                          bool isMandatory) const {
  SILPassManager *pm = invocation->getPassManager();
  auto cha = pm->getAnalysis<ClassHierarchyAnalysis>();
  auto result = ::tryDevirtualizeApply(pm, ApplySite(apply.unbridged()), cha,
                                       nullptr, isMandatory);
  if (result.first) {
    OptionalBridgedInstruction newApply(result.first.getInstruction()->asSILNode());
    return {newApply, result.second};
  }
  return {{nullptr}, false};
}

bool BridgedPassContext::tryOptimizeKeypath(BridgedInstruction apply) const {
  SILBuilder builder(apply.unbridged());
  return ::tryOptimizeKeypath(apply.getAs<ApplyInst>(), builder);
}

OptionalBridgedValue BridgedPassContext::constantFoldBuiltin(BridgedInstruction builtin) const {
  auto bi = builtin.getAs<BuiltinInst>();
  std::optional<bool> resultsInError;
  return {::constantFoldBuiltin(bi, resultsInError)};
}

void BridgedPassContext::inlineFunction(BridgedInstruction apply, bool mandatoryInline) const {
  SILOptFunctionBuilder funcBuilder(invocation->getPassManager());
  InstructionDeleter deleter;
  SILInliner::inlineFullApply(FullApplySite(apply.unbridged()),
                              mandatoryInline
                                  ? SILInliner::InlineKind::MandatoryInline
                                  : SILInliner::InlineKind::PerformanceInline,
                              funcBuilder, deleter);
}

static const irgen::TypeInfo &getTypeInfoOfBuiltin(swift::SILType type, irgen::IRGenModule &IGM) {
  SILType lowered = IGM.getLoweredType(swift::Lowering::AbstractionPattern::getOpaque(), type.getASTType());
  return IGM.getTypeInfo(lowered);
}

static SwiftInt integerValueFromConstant(llvm::Constant *c, SwiftInt add = 0) {
  auto *intConst = dyn_cast_or_null<llvm::ConstantInt>(c);
  if (!intConst)
    return -1;
  APInt value = intConst->getValue();
  return value.getLimitedValue() + add;
}

SwiftInt BridgedPassContext::getStaticSize(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.unbridged(), *IGM);
  llvm::Constant *c = ti.getStaticSize(*IGM);
  return integerValueFromConstant(c);
}

SwiftInt BridgedPassContext::getStaticAlignment(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.unbridged(), *IGM);
  llvm::Constant *c = ti.getStaticAlignmentMask(*IGM);
  return integerValueFromConstant(c, 1);
}

SwiftInt BridgedPassContext::getStaticStride(BridgedType type) const {
  irgen::IRGenModule *IGM = invocation->getIRGenModule();
  if (!IGM)
    return -1;
  auto &ti = getTypeInfoOfBuiltin(type.unbridged(), *IGM);
  llvm::Constant *c = ti.getStaticStride(*IGM);
  return integerValueFromConstant(c);
}

bool BridgedPassContext::canMakeStaticObjectReadOnly(BridgedType type) const {
  if (irgen::IRGenModule *IGM = invocation->getIRGenModule()) {
    return IGM->canMakeStaticObjectReadOnly(type.unbridged());
  }
  return false;
}

swift::SILVTable * BridgedPassContext::specializeVTableForType(BridgedType type, BridgedFunction function) const {
  return ::specializeVTableForType(type.unbridged(),
                                   function.getFunction()->getModule(),
                                   invocation->getPassManager());
}

bool BridgedPassContext::specializeClassMethodInst(BridgedInstruction cm) const {
  return ::specializeClassMethodInst(cm.getAs<ClassMethodInst>());
}

bool BridgedPassContext::specializeAppliesInFunction(BridgedFunction function, bool isMandatory) const {
  return ::specializeAppliesInFunction(*function.getFunction(), invocation->getPassManager(), isMandatory);
}

namespace  {
class GlobalVariableMangler : public Mangle::ASTMangler {
public:
  std::string mangleOutlinedVariable(SILFunction *F, int &uniqueIdx) {
    std::string GlobName;
    do {
      beginManglingWithoutPrefix();
      appendOperator(F->getName());
      appendOperator("Tv", Index(uniqueIdx++));
      GlobName = finalize();
    } while (F->getModule().lookUpGlobalVariable(GlobName));

    return GlobName;
  }
};
} // namespace

BridgedOwnedString BridgedPassContext::mangleOutlinedVariable(BridgedFunction function) const {
  int idx = 0;
  SILFunction *f = function.getFunction();
  SILModule &mod = f->getModule();
  while (true) {
    GlobalVariableMangler mangler;
    std::string name = mangler.mangleOutlinedVariable(f, idx);
    if (!mod.lookUpGlobalVariable(name))
      return name;
    idx++;
  }
}

BridgedOwnedString BridgedPassContext::mangleAsyncRemoved(BridgedFunction function) const {
  SILFunction *F = function.getFunction();

  // FIXME: hard assumption on what pass is requesting this.
  auto P = Demangle::SpecializationPass::AsyncDemotion;

  Mangle::FunctionSignatureSpecializationMangler Mangler(
      P, F->getSerializedKind(), F);
  Mangler.setRemovedEffect(EffectKind::Async);
  return Mangler.mangle();
}

BridgedOwnedString BridgedPassContext::mangleWithDeadArgs(const SwiftInt * _Nullable deadArgs,
                                                          SwiftInt numDeadArgs,
                                                          BridgedFunction function) const {
  SILFunction *f = function.getFunction();
  Mangle::FunctionSignatureSpecializationMangler Mangler(
      Demangle::SpecializationPass::FunctionSignatureOpts,
      f->getSerializedKind(), f);
  for (SwiftInt idx = 0; idx < numDeadArgs; idx++) {
    Mangler.setArgumentDead((unsigned)idx);
  }
  return Mangler.mangle();
}

BridgedOwnedString BridgedPassContext::mangleWithClosureArgs(
  BridgedValueArray bridgedClosureArgs,
  BridgedArrayRef bridgedClosureArgIndices,
  BridgedFunction applySiteCallee
) const {
  auto pass = Demangle::SpecializationPass::ClosureSpecializer;
  auto serializedKind = applySiteCallee.getFunction()->getSerializedKind();
  Mangle::FunctionSignatureSpecializationMangler mangler(
      pass, serializedKind, applySiteCallee.getFunction());

  llvm::SmallVector<swift::SILValue, 16> closureArgsStorage;
  auto closureArgs = bridgedClosureArgs.getValues(closureArgsStorage);
  auto closureArgIndices = bridgedClosureArgIndices.unbridged<SwiftInt>();

  assert(closureArgs.size() == closureArgIndices.size() &&
         "Number of closures arguments and number of closure indices do not match!");

  for (size_t i = 0; i < closureArgs.size(); i++) {
    auto closureArg = closureArgs[i];
    auto closureArgIndex = closureArgIndices[i];

    if (auto *PAI = dyn_cast<PartialApplyInst>(closureArg)) {
      mangler.setArgumentClosureProp(closureArgIndex,
                                     const_cast<PartialApplyInst *>(PAI));
    } else {
      auto *TTTFI = cast<ThinToThickFunctionInst>(closureArg);
      mangler.setArgumentClosureProp(closureArgIndex, 
                                     const_cast<ThinToThickFunctionInst *>(TTTFI));
    }
  }

  return mangler.mangle();
}

BridgedGlobalVar BridgedPassContext::createGlobalVariable(BridgedStringRef name, BridgedType type, bool isPrivate) const {
  return {SILGlobalVariable::create(
      *invocation->getPassManager()->getModule(),
      isPrivate ? SILLinkage::Private : SILLinkage::Public, IsNotSerialized,
      name.unbridged(), type.unbridged())};
}

void BridgedPassContext::fixStackNesting(BridgedFunction function) const {
  switch (StackNesting::fixNesting(function.getFunction())) {
    case StackNesting::Changes::None:
      break;
    case StackNesting::Changes::Instructions:
      invocation->notifyChanges(SILAnalysis::InvalidationKind::Instructions);
      break;
    case StackNesting::Changes::CFG:
      invocation->notifyChanges(SILAnalysis::InvalidationKind::BranchesAndInstructions);
      break;
  }
  invocation->setNeedFixStackNesting(false);
}

OptionalBridgedFunction BridgedPassContext::lookupStdlibFunction(BridgedStringRef name) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  SmallVector<ValueDecl *, 1> results;
  mod->getASTContext().lookupInSwiftModule(name.unbridged(), results);
  if (results.size() != 1)
    return {nullptr};

  auto *decl = dyn_cast<FuncDecl>(results.front());
  if (!decl)
    return {nullptr};

  SILDeclRef declRef(decl, SILDeclRef::Kind::Func);
  SILOptFunctionBuilder funcBuilder(invocation->getPassManager());
  return {funcBuilder.getOrCreateFunction(SILLocation(decl), declRef, NotForDefinition)};
}

OptionalBridgedFunction BridgedPassContext::lookUpNominalDeinitFunction(BridgedNominalTypeDecl nominal)  const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  return {mod->lookUpMoveOnlyDeinitFunction(nominal.unbridged())};
}

bool BridgedPassContext::enableSimplificationFor(BridgedInstruction inst) const {
  // Fast-path check.
  if (SimplifyInstructionTest.empty() && SILDisablePass.empty())
    return true;

  StringRef instName = getSILInstructionName(inst.unbridged()->getKind());

  if (SILPassManager::isInstructionPassDisabled(instName))
    return false;

  if (SimplifyInstructionTest.empty())
    return true;

  for (const std::string &testName : SimplifyInstructionTest) {
    if (testName == instName)
      return true;
  }
  return false;
}

BridgedFunction BridgedPassContext::
createEmptyFunction(BridgedStringRef name,
                    const BridgedParameterInfo * _Nullable bridgedParams,
                    SwiftInt paramCount,
                    bool hasSelfParam,
                    BridgedFunction fromFunc) const {
  swift::SILModule *mod = invocation->getPassManager()->getModule();
  SILFunction *fromFn = fromFunc.getFunction();

  llvm::SmallVector<SILParameterInfo> params;
  for (unsigned idx = 0; idx < paramCount; ++idx) {
    params.push_back(bridgedParams[idx].unbridged());
  }

  CanSILFunctionType fTy = fromFn->getLoweredFunctionType();
  assert(fromFn->getGenericSignature().isNull() && "generic functions are not supported");

  auto extInfo = fTy->getExtInfo();
  if (fTy->hasSelfParam() && !hasSelfParam)
    extInfo = extInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);

  CanSILFunctionType newTy = SILFunctionType::get(
      /*GenericSignature=*/nullptr, extInfo, fTy->getCoroutineKind(),
      fTy->getCalleeConvention(), params, fTy->getYields(),
      fTy->getResults(), fTy->getOptionalErrorResult(),
      SubstitutionMap(), SubstitutionMap(),
      mod->getASTContext());

  SILOptFunctionBuilder functionBuilder(invocation->getPassManager());

  SILFunction *newF = functionBuilder.createFunction(
      fromFn->getLinkage(), name.unbridged(), newTy, nullptr,
      fromFn->getLocation(), fromFn->isBare(), fromFn->isTransparent(),
      fromFn->getSerializedKind(), IsNotDynamic, IsNotDistributed,
      IsNotRuntimeAccessible, fromFn->getEntryCount(), fromFn->isThunk(),
      fromFn->getClassSubclassScope(), fromFn->getInlineStrategy(),
      fromFn->getEffectsKind(), nullptr, fromFn->getDebugScope());

  return {newF};
}

void BridgedPassContext::moveFunctionBody(BridgedFunction sourceFunc, BridgedFunction destFunc) const {
  SILFunction *sourceFn = sourceFunc.getFunction();
  SILFunction *destFn = destFunc.getFunction();
  destFn->moveAllBlocksFromOtherFunction(sourceFn);
  invocation->getPassManager()->invalidateAnalysis(sourceFn, SILAnalysis::InvalidationKind::Everything);
  invocation->getPassManager()->invalidateAnalysis(destFn, SILAnalysis::InvalidationKind::Everything);
}

void BridgedPassContext::verifyModule() const {
  getModule()->verify(invocation->getPassManager()->getAnalysis<BasicCalleeAnalysis>()->getCalleeCache());
}

void BridgedPassContext::verifyFunction(BridgedFunction function) const {
  function.getFunction()->verify(invocation->getPassManager()->getAnalysis<BasicCalleeAnalysis>()->getCalleeCache());
}


BridgedFunction BridgedPassContext::
ClosureSpecializer_createEmptyFunctionWithSpecializedSignature(BridgedStringRef specializedName,
                                            const BridgedParameterInfo * _Nullable specializedBridgedParams,
                                            SwiftInt paramCount,
                                            BridgedFunction bridgedApplySiteCallee,
                                            bool isSerialized)  const {
  auto *applySiteCallee = bridgedApplySiteCallee.getFunction();
  auto applySiteCalleeType = applySiteCallee->getLoweredFunctionType();

  llvm::SmallVector<SILParameterInfo> specializedParams;
  for (unsigned idx = 0; idx < paramCount; ++idx) {
    specializedParams.push_back(specializedBridgedParams[idx].unbridged());
  }

  // The specialized function is always a thin function. This is important
  // because we may add additional parameters after the Self parameter of
  // witness methods. In this case the new function is not a method anymore.
  auto extInfo = applySiteCalleeType->getExtInfo();
  extInfo = extInfo.withRepresentation(SILFunctionTypeRepresentation::Thin);

  auto ClonedTy = SILFunctionType::get(
      applySiteCalleeType->getInvocationGenericSignature(), extInfo,
      applySiteCalleeType->getCoroutineKind(),
      applySiteCalleeType->getCalleeConvention(), specializedParams,
      applySiteCalleeType->getYields(), applySiteCalleeType->getResults(),
      applySiteCalleeType->getOptionalErrorResult(),
      applySiteCalleeType->getPatternSubstitutions(),
      applySiteCalleeType->getInvocationSubstitutions(),
      applySiteCallee->getModule().getASTContext());

  SILOptFunctionBuilder functionBuilder(invocation->getPassManager());

  // We make this function bare so we don't have to worry about decls in the
  // SILArgument.
  auto *specializedApplySiteCallee = functionBuilder.createFunction(
      // It's important to use a shared linkage for the specialized function
      // and not the original linkage.
      // Otherwise the new function could have an external linkage (in case the
      // original function was de-serialized) and would not be code-gen'd.
      // It's also important to disconnect this specialized function from any
      // classes (the classSubclassScope), because that may incorrectly
      // influence the linkage.
      getSpecializedLinkage(applySiteCallee, applySiteCallee->getLinkage()), specializedName.unbridged(),
      ClonedTy, applySiteCallee->getGenericEnvironment(),
      applySiteCallee->getLocation(), IsBare, applySiteCallee->isTransparent(),
      isSerialized ? IsSerialized : IsNotSerialized, IsNotDynamic, IsNotDistributed,
      IsNotRuntimeAccessible, applySiteCallee->getEntryCount(),
      applySiteCallee->isThunk(),
      /*classSubclassScope=*/SubclassScope::NotApplicable,
      applySiteCallee->getInlineStrategy(), applySiteCallee->getEffectsKind(),
      applySiteCallee, applySiteCallee->getDebugScope());
  
  if (!applySiteCallee->hasOwnership()) {
    specializedApplySiteCallee->setOwnershipEliminated();
  }
  
  for (auto &Attr : applySiteCallee->getSemanticsAttrs())
    specializedApplySiteCallee->addSemanticsAttr(Attr);
  
  return {specializedApplySiteCallee};
}

bool FullApplySite_canInline(BridgedInstruction apply) {
  return swift::SILInliner::canInlineApplySite(
      swift::FullApplySite(apply.unbridged()));
}

BridgedDynamicCastResult classifyDynamicCastBridged(BridgedType sourceTy, BridgedType destTy,
                                                    BridgedFunction function,
                                                    bool sourceTypeIsExact) {
  static_assert((int)DynamicCastFeasibility::WillSucceed == (int)BridgedDynamicCastResult::willSucceed);
  static_assert((int)DynamicCastFeasibility::MaySucceed  == (int)BridgedDynamicCastResult::maySucceed);
  static_assert((int)DynamicCastFeasibility::WillFail    == (int)BridgedDynamicCastResult::willFail);

  return static_cast<BridgedDynamicCastResult>(
    classifyDynamicCast(function.getFunction()->getModule().getSwiftModule(),
                        sourceTy.unbridged().getASTType(),
                        destTy.unbridged().getASTType(),
                        sourceTypeIsExact));
}

BridgedDynamicCastResult classifyDynamicCastBridged(BridgedInstruction inst) {
  SILDynamicCastInst castInst(inst.unbridged());
  return static_cast<BridgedDynamicCastResult>(castInst.classifyFeasibility(/*allowWholeModule=*/ false));
}

// TODO: can't be inlined to work around https://github.com/apple/swift/issues/64502
BridgedCalleeAnalysis::CalleeList BridgedCalleeAnalysis::getCallees(BridgedValue callee) const {
  return ca->getCalleeListOfValue(callee.getSILValue());
}

// TODO: can't be inlined to work around https://github.com/apple/swift/issues/64502
BridgedCalleeAnalysis::CalleeList BridgedCalleeAnalysis::getDestructors(BridgedType type, bool isExactType) const {
  return ca->getDestructors(type.unbridged(), isExactType);
}

// Need to put ClonerWithFixedLocation into namespace swift to forward reference
// it in OptimizerBridging.h.
namespace swift {

class ClonerWithFixedLocation : public SILCloner<ClonerWithFixedLocation> {
  friend class SILInstructionVisitor<ClonerWithFixedLocation>;
  friend class SILCloner<ClonerWithFixedLocation>;

  SILDebugLocation insertLoc;

public:
  ClonerWithFixedLocation(SILGlobalVariable *gVar)
  : SILCloner<ClonerWithFixedLocation>(gVar),
  insertLoc(ArtificialUnreachableLocation(), nullptr) {}

  ClonerWithFixedLocation(SILInstruction *insertionPoint)
  : SILCloner<ClonerWithFixedLocation>(*insertionPoint->getFunction()),
  insertLoc(insertionPoint->getDebugLocation()) {
    Builder.setInsertionPoint(insertionPoint);
  }

  SILValue getClonedValue(SILValue v) {
    return getMappedValue(v);
  }

  void cloneInst(SILInstruction *inst) {
    visit(inst);
  }

protected:

  SILLocation remapLocation(SILLocation loc) {
    return insertLoc.getLocation();
  }

  const SILDebugScope *remapScope(const SILDebugScope *DS) {
    return insertLoc.getScope();
  }
};

} // namespace swift

BridgedCloner::BridgedCloner(BridgedGlobalVar var, BridgedPassContext context)
  : cloner(new ClonerWithFixedLocation(var.getGlobal())) {
  context.invocation->notifyNewCloner();
}

BridgedCloner::BridgedCloner(BridgedInstruction inst,
                             BridgedPassContext context)
    : cloner(new ClonerWithFixedLocation(inst.unbridged())) {
  context.invocation->notifyNewCloner();
}

void BridgedCloner::destroy(BridgedPassContext context) {
  delete cloner;
  cloner = nullptr;
  context.invocation->notifyClonerDestroyed();
}

BridgedValue BridgedCloner::getClonedValue(BridgedValue v) {
  return {cloner->getClonedValue(v.getSILValue())};
}

bool BridgedCloner::isValueCloned(BridgedValue v) const {
  return cloner->isValueCloned(v.getSILValue());
}

void BridgedCloner::clone(BridgedInstruction inst) {
  cloner->cloneInst(inst.unbridged());
}

static BridgedUtilities::VerifyFunctionFn verifyFunctionFunction;

void BridgedUtilities::registerVerifier(VerifyFunctionFn verifyFunctionFn) {
  verifyFunctionFunction = verifyFunctionFn;
}

void SILPassManager::runSwiftFunctionVerification(SILFunction *f) {
  if (!verifyFunctionFunction)
    return;

  if (f->getModule().getOptions().VerifyNone)
    return;

  getSwiftPassInvocation()->beginVerifyFunction(f);
  verifyFunctionFunction({getSwiftPassInvocation()}, {f});
  getSwiftPassInvocation()->endVerifyFunction();
}

void SILPassManager::runSwiftModuleVerification() {
  for (SILFunction &f : *Mod) {
    runSwiftFunctionVerification(&f);
  }
}

namespace swift {
  class ClosureSpecializationCloner: public SILClonerWithScopes<ClosureSpecializationCloner> {
    friend class SILInstructionVisitor<ClosureSpecializationCloner>;
    friend class SILCloner<ClosureSpecializationCloner>;
  public: 
    using SuperTy = SILClonerWithScopes<ClosureSpecializationCloner>;
    ClosureSpecializationCloner(SILFunction &emptySpecializedFunction): SuperTy(emptySpecializedFunction) {}
  };
} // namespace swift

BridgedSpecializationCloner::BridgedSpecializationCloner(BridgedFunction emptySpecializedFunction): 
  closureSpecCloner(new ClosureSpecializationCloner(*emptySpecializedFunction.getFunction())) {}

BridgedFunction BridgedSpecializationCloner::getCloned() const {
  return { &closureSpecCloner->getBuilder().getFunction() };
}

BridgedBasicBlock BridgedSpecializationCloner::getClonedBasicBlock(BridgedBasicBlock originalBasicBlock) const {
  return { closureSpecCloner->getOpBasicBlock(originalBasicBlock.unbridged()) };
}

void BridgedSpecializationCloner::cloneFunctionBody(BridgedFunction originalFunction, BridgedBasicBlock clonedEntryBlock, BridgedValueArray clonedEntryBlockArgs) const {
  llvm::SmallVector<swift::SILValue, 16> clonedEntryBlockArgsStorage;
  auto clonedEntryBlockArgsArrayRef = clonedEntryBlockArgs.getValues(clonedEntryBlockArgsStorage);
  closureSpecCloner->cloneFunctionBody(originalFunction.getFunction(), clonedEntryBlock.unbridged(), clonedEntryBlockArgsArrayRef);
}

void BridgedBuilder::destroyCapturedArgs(BridgedInstruction partialApply) const {
  if (auto *pai = llvm::dyn_cast<PartialApplyInst>(partialApply.unbridged()); pai->isOnStack()) {
    auto b = unbridged();
    return swift::insertDestroyOfCapturedArguments(pai, b); 
  } else {
    assert(false && "`destroyCapturedArgs` must only be called on a `partial_apply` on stack!");   
  }
}

void verifierError(BridgedStringRef message,
                   OptionalBridgedInstruction atInstruction,
                   OptionalBridgedArgument atArgument) {
  Twine msg(message.unbridged());
  verificationFailure(msg, atInstruction.unbridged(), atArgument.unbridged(), /*extraContext=*/nullptr);
}
