//===--- SILGenCleanup.cpp ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// Perform peephole-style "cleanup" to aid SIL diagnostic passes.
///
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "silgen-cleanup"

#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockBits.h"
#include "swift/SIL/BasicBlockDatastructures.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OSSACompleteLifetime.h"
#include "swift/SIL/PrettyStackTrace.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Analysis/DeadEndBlocksAnalysis.h"
#include "swift/SILOptimizer/Analysis/PostOrderAnalysis.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/CanonicalizeInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "llvm/ADT/PostOrderIterator.h"
#include "swift/SILOptimizer/Utils/OwnershipOptUtils.h"

using namespace swift;

// Define a CanonicalizeInstruction subclass for use in SILGenCleanup.
struct SILGenCanonicalize final : CanonicalizeInstruction {
  bool changed = false;
  llvm::SmallPtrSet<SILInstruction *, 16> deadOperands;

  SILGenCanonicalize(DeadEndBlocks &deadEndBlocks)
      : CanonicalizeInstruction(DEBUG_TYPE, deadEndBlocks) {}

  void notifyNewInstruction(SILInstruction *) override { changed = true; }

  // Just delete the given 'inst' and record its operands. The callback isn't
  // allowed to mutate any other instructions.
  void killInstruction(SILInstruction *inst) override {
    deadOperands.erase(inst);
    for (auto &operand : inst->getAllOperands()) {
      if (auto *operInst = operand.get()->getDefiningInstruction())
        deadOperands.insert(operInst);
    }
    inst->eraseFromParent();
    changed = true;
  }

  void notifyHasNewUsers(SILValue) override { changed = true; }

  /// Delete trivially dead instructions in non-deterministic order.
  ///
  /// We either have that nextII is endII or if nextII is not endII then endII
  /// is nextII->getParent()->end().
  SILBasicBlock::iterator deleteDeadOperands(SILBasicBlock::iterator nextII,
                                             SILBasicBlock::iterator endII) {
    auto callbacks = InstModCallbacks().onDelete([&](SILInstruction *deadInst) {
      LLVM_DEBUG(llvm::dbgs() << "Trivially dead: " << *deadInst);

      // If nextII is the instruction we are going to delete, move nextII past
      // it.
      if (deadInst->getIterator() == nextII)
        ++nextII;

      // Then remove the instruction from the set and delete it.
      deadOperands.erase(deadInst);
      deadInst->eraseFromParent();
    });

    while (!deadOperands.empty()) {
      SILInstruction *deadOperInst = *deadOperands.begin();

      // Make sure at least the first instruction is removed from the set.
      deadOperands.erase(deadOperInst);

      // Then delete this instruction/everything else that we can.
      eliminateDeadInstruction(deadOperInst, callbacks);
    }
    return nextII;
  }
};

//===----------------------------------------------------------------------===//
// SILGenCleanup: Top-Level Module Transform
//===----------------------------------------------------------------------===//

namespace {

// SILGenCleanup must run on all functions that will be seen by any analysis
// used by diagnostics before transforming the function that requires the
// analysis. e.g. Closures need to be cleaned up before the closure's parent can
// be diagnosed.
//
// TODO: This pass can be converted to a function transform if the mandatory
// pipeline runs in bottom-up closure order.
struct SILGenCleanup : SILModuleTransform {
  void run() override;

  bool fixupBorrowAccessors(SILFunction *function);
};

// Iterate over `iterator` until finding a block in `include` and not in
// `exclude`.
SILBasicBlock *
findFirstBlock(SILFunction *function, SILFunction::iterator &iterator,
               llvm::function_ref<bool(SILBasicBlock *)> include,
               llvm::function_ref<bool(SILBasicBlock *)> exclude) {
  while (iterator != function->end()) {
    auto *block = &*iterator;
    iterator = std::next(iterator);
    if (!include(block))
      continue;
    if (exclude(block))
      continue;
    return block;
  }
  return nullptr;
}

// Walk backward from `from` following first predecessors until finding the
// first already-reached block.
SILBasicBlock *findFirstLoop(SILFunction *function, SILBasicBlock *from) {
  BasicBlockSet path(function);
  auto *current = from;
  while (auto *block = current) {
    current = nullptr;
    if (!path.insert(block)) {
      return block;
    }
    if (block->pred_empty()) {
      return nullptr;
    }
    current = *block->getPredecessorBlocks().begin();
  }
  llvm_unreachable("finished function-exiting loop!?");
}

/*  SILGen may produce a borrow accessor result from within a local borrow
 * scope. Such as:
 *
 *    ```
 *      %ld = load_borrow %self
 *      %fwd = unchecked_ownership %ld
 *      %ex = struct_extract %fwd, #Struct.storedProperty
 *      end_borrow %ld
 *      return %ex
 *    ```
 *      This is illegal OSSA, since the return uses a value outside it's borrow
 * scope.
 *
 *    Transform this into valid OSSA:
 *
 *    ```
 *      %ld = load_borrow %self
 *      %ex = struct_extract %ld, #Struct.storedProperty
 *      return_borrow %ex from_scopes %ld
 *    ```
 */
bool SILGenCleanup::fixupBorrowAccessors(SILFunction *function) {
  if (!function->getConventions().hasGuaranteedResult()) {
    return false;
  }
  auto returnBB = function->findReturnBB();
  if (returnBB == function->end()) {
    return false;
  }

  auto *returnInst = cast<ReturnInst>(returnBB->getTerminator());
  if (returnInst->getOperand()->getOwnershipKind() !=
      OwnershipKind::Guaranteed) {
    return false;
  }

  SmallVector<SILValue, 8> enclosingValues;
  findGuaranteedReferenceRoots(returnInst->getOperand(),
                               /*lookThroughNestedBorrows=*/false,
                               enclosingValues);
  SmallVector<SILInstruction *> scopeEnds;
  SmallVector<SILValue> operands;
  SmallVector<SILInstruction *> toDelete;

  // For all the local borrow scopes that enclose the return value, delete
  // their end_borrow instructions and use them as an encolsing value in
  // return_borrow instruction.
  for (auto enclosingValue : enclosingValues) {
    BorrowedValue borrow(enclosingValue);
    if (!borrow.isLocalScope()) {
      continue;
    }
    borrow.getLocalScopeEndingInstructions(scopeEnds);
    for (auto *scopeEnd : scopeEnds) {
      if (auto *endBorrow = dyn_cast<EndBorrowInst>(scopeEnd)) {
        operands.push_back(endBorrow->getOperand());
        endBorrow->eraseFromParent();
      }
    }
    for (auto *uncheckedOwnership :
         borrow->getUsersOfType<UncheckedOwnershipInst>()) {
      uncheckedOwnership->replaceAllUsesWith(*borrow);
      toDelete.push_back(uncheckedOwnership);
    }
  }

  for (auto *inst : toDelete) {
    inst->eraseFromParent();
  }

  if (operands.empty()) {
    return false;
  }

  SILBuilderWithScope(returnInst)
      .createReturnBorrow(returnInst->getLoc(), returnInst->getOperand(),
                          operands);
  returnInst->eraseFromParent();
  return true;
}

void SILGenCleanup::run() {
  auto &module = *getModule();
  for (auto &function : module) {
    if (!function.isDefinition())
      continue;

    getPassManager()->getSwiftPassInvocation()->initializeNestedSwiftPassInvocation(&function);

    PrettyStackTraceSILFunction stackTrace("silgen cleanup", &function);

    LLVM_DEBUG(llvm::dbgs()
               << "\nRunning SILGenCleanup on " << function.getName() << "\n");

    bool changed = fixupBorrowAccessors(&function);
    completeAllLifetimes(getPassManager(), &function);
    function.verifyOwnership(/*deadEndBlocks=*/nullptr);

    DeadEndBlocks deadEndBlocks(&function);
    SILGenCanonicalize sgCanonicalize(deadEndBlocks);

    // Iterate over all blocks even if they aren't reachable. No phi-less
    // dataflow cycles should have been created yet, and these transformations
    // are simple enough they shouldn't be affected by cycles.
    for (auto &bb : function) {
      for (auto ii = bb.begin(), ie = bb.end(); ii != ie;) {
        ii = sgCanonicalize.canonicalize(&*ii);
        ii = sgCanonicalize.deleteDeadOperands(ii, ie);
      }
    }
    changed |= sgCanonicalize.changed;
    if (changed) {
      auto invalidKind = SILAnalysis::InvalidationKind::Instructions;
      invalidateAnalysis(&function, invalidKind);
    }
 
    getPassManager()->getSwiftPassInvocation()->deinitializeNestedSwiftPassInvocation();
  }
}

} // end anonymous namespace

SILTransform *swift::createSILGenCleanup() { return new SILGenCleanup(); }
