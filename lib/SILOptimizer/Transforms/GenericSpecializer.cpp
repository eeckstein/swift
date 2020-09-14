//===--- GenericSpecializer.cpp - Specialization of generic functions -----===//
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
//
// Specialize calls to generic functions by substituting static type
// information.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-generic-specializer"

#include "swift/SIL/OptimizationRemark.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

// For testing during bring up.
static llvm::cl::opt<bool> EnableGenericSpecializerWithOwnership(
    "sil-generic-specializer-enable-ownership", llvm::cl::init(false));

namespace {

class GenericSpecializer : public SILFunctionTransform {

  bool specializeAppliesInFunction(SILFunction &F);

  /// The entry point to the transformation.
  void run() override {
    SILFunction &F = *getFunction();

    // TODO: We should be able to handle ownership.
    if (F.hasOwnership() && !EnableGenericSpecializerWithOwnership)
      return;

    LLVM_DEBUG(llvm::dbgs() << "***** GenericSpecializer on function:"
                            << F.getName() << " *****\n");

    if (specializeAppliesInFunction(F))
      invalidateAnalysis(SILAnalysis::InvalidationKind::Everything);
  }

};

} // end anonymous namespace

bool GenericSpecializer::specializeAppliesInFunction(SILFunction &F) {
  SILOptFunctionBuilder FunctionBuilder(*this);
  DeadInstructionSet DeadApplies;
  llvm::SmallSetVector<SILInstruction *, 8> Applies;
  OptRemark::Emitter ORE(DEBUG_TYPE, F);

  bool Changed = false;
  for (auto &BB : F) {
    // Collect the applies for this block in reverse order so that we
    // can pop them off the end of our vector and process them in
    // forward order.
    for (auto &I : llvm::reverse(BB)) {

      // Skip non-apply instructions, apply instructions with no
      // substitutions, apply instructions where we do not statically
      // know the called function, and apply instructions where we do
      // not have the body of the called function.
      ApplySite Apply = ApplySite::isa(&I);
      if (!Apply || !Apply.hasSubstitutions())
        continue;

      auto *Callee = Apply.getReferencedFunctionOrNull();
      if (!Callee)
        continue;
      if (!Callee->isDefinition()) {
        ORE.emit([&]() {
          using namespace OptRemark;
          return RemarkMissed("NoDef", I)
                 << "Unable to specialize generic function "
                 << NV("Callee", Callee) << " since definition is not visible";
        });
        continue;
      }

      Applies.insert(Apply.getInstruction());
    }

    // Attempt to specialize each apply we collected, deleting any
    // that we do specialize (along with other instructions we clone
    // in the process of doing so). We pop from the end of the list to
    // avoid tricky iterator invalidation issues.
    while (!Applies.empty()) {
      auto *I = Applies.pop_back_val();
      auto Apply = ApplySite::isa(I);
      assert(Apply && "Expected an apply!");
      SILFunction *Callee = Apply.getReferencedFunctionOrNull();
      assert(Callee && "Expected to have a known callee");

      if (!Apply.canOptimize() || !Callee->shouldOptimize())
        continue;

      // We have a call that can potentially be specialized, so
      // attempt to do so.
      llvm::SmallVector<SILFunction *, 2> NewFunctions;
      trySpecializeApplyOfGeneric(FunctionBuilder, Apply, DeadApplies,
                                  NewFunctions, ORE);

      // Remove all the now-dead applies. We must do this immediately
      // rather than defer it in order to avoid problems with cloning
      // dead instructions when doing recursive specialization.
      while (!DeadApplies.empty()) {
        auto *AI = DeadApplies.pop_back_val();

        // Remove any applies we are deleting so that we don't attempt
        // to specialize them.
        Applies.remove(AI);

        recursivelyDeleteTriviallyDeadInstructions(AI, true);
        Changed = true;
      }

      // If calling the specialization utility resulted in new functions
      // (as opposed to returning a previous specialization), we need to notify
      // the pass manager so that the new functions get optimized.
      for (SILFunction *NewF : reverse(NewFunctions)) {
        addFunctionToPassManagerWorklist(NewF, Callee);
      }
    }
  }

  return Changed;
}

namespace {

class VTableSpecializer : public SILModuleTransform {

  bool specializeVTables(SILModule &module);
  bool specializeVTableFor(AllocRefInst *allocRef, SILModule &module);
  SILFunction *specializeVTableMethod(SILFunction *origMethod,
                                      SubstitutionMap subs, SILModule &module);

  /// The entry point to the transformation.
  void run() override {
    SILModule &module = *getModule();

    LLVM_DEBUG(llvm::dbgs() << "***** VTableSpecializer\n");

    if (specializeVTables(module))
      invalidateFunctionTables();
  }

};

} // end anonymous namespace

bool VTableSpecializer::specializeVTables(SILModule &module) {
  bool changed = false;
  for (SILFunction &func : module) {
    if (func.getLoweredFunctionType()->isPolymorphic())
      continue;

    for (SILBasicBlock &block : func) {
      for (SILInstruction &inst : block) {
        if (auto *allocRef = dyn_cast<AllocRefInst>(&inst)) {
          changed |= specializeVTableFor(allocRef, module);
        }
      }
    }
  }
  
  for (SILVTable *vtable : module.getVTables()) {
    if (vtable->getClass()->isGenericContext())
      continue;
      
    for (SILVTableEntry &entry : vtable->getMutableEntries()) {
      SILFunction *method = entry.getImplementation();
      if (!method->getLoweredFunctionType()->isPolymorphic())
        continue;
        
      if (entry.getKind() != SILVTableEntry::Kind::Inherited) {
        vtable->dump();
        entry.getMethod().dump();
      }
      assert(entry.getKind() == SILVTableEntry::Kind::Inherited);
      Decl *classOfMethod = entry.getMethod().getDecl()->getDeclContext()->getAsDecl();
      SILType classTy = vtable->getClassType();
      while (classTy.getClassOrBoundGenericClass() != classOfMethod) {
        classTy = classTy.getSuperclass();
      }
      auto *classDecl = cast<ClassDecl>(classOfMethod);
      SubstitutionMap subs = classTy.getASTType()->getContextSubstitutionMap(
        classDecl->getParentModule(), classDecl);
        
      SILFunction *specializedMethod = specializeVTableMethod(method, subs, module);
      entry.setImplementation(specializedMethod);
      vtable->updateVTableCache(entry);
    }
    
  }
  
  return changed;
}

bool VTableSpecializer::specializeVTableFor(AllocRefInst *allocRef, SILModule &module) {
  SILType classTy = allocRef->getType();
  CanType astType = classTy.getASTType();
  BoundGenericClassType *genClassTy = dyn_cast<BoundGenericClassType>(astType);
  if (!genClassTy)
    return false;
    
  if (module.lookUpSpecializedVTable(classTy))
    return false;
    
  ClassDecl *classDecl = genClassTy->getDecl();
  SILVTable *origVtable = module.lookUpVTable(classDecl);
  if (!origVtable) {
    llvm::errs() << "No vtable available for " << genClassTy->getDecl()->getName() << '\n';
    abort();
  }

  SubstitutionMap subs = astType->getContextSubstitutionMap(classDecl->getParentModule(), classDecl);

  llvm::SmallVector<SILVTableEntry, 8> newEntries;
  
  for (const SILVTableEntry &entry : origVtable->getEntries()) {
    SILFunction *origMethod = entry.getImplementation();
    SILFunction *specializedMethod = specializeVTableMethod(origMethod, subs, module);
    newEntries.push_back(SILVTableEntry(entry.getMethod(), specializedMethod,
         entry.getKind(), entry.isNonOverridden()));
  }

  SILVTable::create(module, classDecl, classTy, IsNotSerialized, newEntries);
  return true;
}

SILFunction *VTableSpecializer::specializeVTableMethod(SILFunction *origMethod,
      SubstitutionMap subs, SILModule &module) {
  
  if (!origMethod->getLoweredFunctionType()->isPolymorphic())
    return origMethod;
  
  ReabstractionInfo ReInfo(module.getSwiftModule(), module.isWholeModule(), ApplySite(),
                           origMethod, subs,
                           IsNotSerialized,
                           /*ConvertIndirectToDirect=*/true, nullptr);

  if (!ReInfo.canBeSpecialized()) {
    llvm::errs() << "Cannot specialize vtable method " << origMethod->getName() << '\n';
    abort();
  }

  SILOptFunctionBuilder FunctionBuilder(*this);

  GenericFuncSpecializer FuncSpecializer(FunctionBuilder,
                                         origMethod, subs,
                                         ReInfo);
  SILFunction *SpecializedF = FuncSpecializer.lookupSpecialization();
  if (!SpecializedF)
    SpecializedF = FuncSpecializer.tryCreateSpecialization();
  if (!SpecializedF || SpecializedF->getLoweredFunctionType()->hasError()) {
    llvm::errs() << "Cannot specialize vtable method " << origMethod->getName() << '\n'
                 << "Generic class methods are not supported in tiny mode\n";
    exit(1);
  }

  // Link after prespecializing to pull in everything referenced from another
  // module in case some referenced functions have non-public linkage.
  module.linkFunction(SpecializedF, SILModule::LinkingMode::LinkAll);

  SpecializedF->setLinkage(SILLinkage::Public);
  SpecializedF->setSerialized(IsNotSerialized);

  return SpecializedF;
}

SILTransform *swift::createGenericSpecializer() {
  return new GenericSpecializer();
}

SILTransform *swift::createVTableSpecializer() {
  return new VTableSpecializer();
}
