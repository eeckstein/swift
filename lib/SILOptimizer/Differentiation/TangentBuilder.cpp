//===--- TangentBuilder.cpp - Tangent SIL builder ------------*- C++ -*----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines a helper class for emitting tangent code for automatic
// differentiation.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Differentiation/TangentBuilder.h"
#include "swift/SILOptimizer/Differentiation/ADContext.h"

namespace swift {
namespace autodiff {

void TangentBuilder::emitZeroIntoBuffer(SILLocation loc, SILValue buffer,
                                        IsInitialization_t isInit) {
  if (!isInit)
    emitDestroyAddr(loc, buffer);
  if (auto tupleType = buffer->getType().getAs<TupleType>()) {
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *eltAddr = createTupleElementAddr(loc, buffer, i);
      emitZeroIntoBuffer(loc, eltAddr, IsInitialization);
    }
    return;
  }
  auto *swiftMod = getModule().getSwiftModule();
  // Look up conformance to `AdditiveArithmetic`.
  auto *additiveArithmeticProto = adContext.getAdditiveArithmeticProtocol();
  auto astType = buffer->getType().getASTType();
  auto confRef = swiftMod->lookupConformance(astType, additiveArithmeticProto);
  assert(!confRef.isInvalid() && "Missing conformance to `AdditiveArithmetic`");
  SILDeclRef accessorDeclRef(adContext.getAdditiveArithmeticZeroGetter(),
                             SILDeclRef::Kind::Func);
  auto silFnType = getModule().Types.getConstantType(
      getTypeExpansionContext(), accessorDeclRef);
  // %wm = witness_method ...
  auto *getter = createWitnessMethod(
      loc, astType, confRef, accessorDeclRef, silFnType);
  // %metatype = metatype $T
  auto metatypeType = CanMetatypeType::get(astType,
                                           MetatypeRepresentation::Thick);
  auto metatype = createMetatype(
      loc, SILType::getPrimitiveObjectType(metatypeType));
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      additiveArithmeticProto, astType, confRef);
  createApply(loc, getter, subMap, {buffer, metatype});
  emitDestroyValueOperation(loc, getter);
}

SILValue TangentBuilder::emitZero(SILLocation loc, CanType type) {
  auto silType = getModule().Types.getLoweredLoadableType(
      type, TypeExpansionContext::minimal(), getModule());
  auto *alloc = createAllocStack(loc, silType);
  emitZeroIntoBuffer(loc, alloc, IsInitialization);
  auto zeroValue = emitLoadValueOperation(
      loc, alloc, LoadOwnershipQualifier::Take);
  createDeallocStack(loc, alloc);
  return zeroValue;
}

void TangentBuilder::emitInPlaceAdd(
    SILLocation loc, SILValue destinationBuffer, SILValue operand) {
  assert(destinationBuffer->getType().isAddress());
  auto type = destinationBuffer->getType();
  if (auto tupleType = type.getAs<TupleType>()) {
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *eltDestAddr = createTupleElementAddr(loc, destinationBuffer, i);
      switch (operand->getType().getCategory()) {
      case SILValueCategory::Address: {
        auto *eltOperand = createTupleElementAddr(loc, operand, i);
        emitInPlaceAdd(loc, eltDestAddr, eltOperand);
        break;
      }
      case SILValueCategory::Object: {
        auto borrowedOp = emitBeginBorrowOperation(loc, operand);
        auto eltOperand = emitTupleExtract(loc, borrowedOp, i);
        emitInPlaceAdd(loc, eltDestAddr, eltOperand);
        emitEndBorrowOperation(loc, borrowedOp);
        break;
      }
      }
    }
    return;
  }
  // Call the combiner function and return.
  auto *swiftMod = getModule().getSwiftModule();
  auto astType = type.getASTType();
  auto confRef = swiftMod->lookupConformance(
      astType, adContext.getAdditiveArithmeticProtocol());
  assert(!confRef.isInvalid() &&
         "Missing conformance to `AdditiveArithmetic`");
  SILDeclRef declRef(adContext.getPlusEqualDecl(), SILDeclRef::Kind::Func);
  auto silFnTy = getModule().Types.getConstantType(
      getTypeExpansionContext(), declRef);
  // %0 = witness_method @+=
  auto witnessMethod =
      createWitnessMethod(loc, astType, confRef, declRef, silFnTy);
  auto subMap = SubstitutionMap::getProtocolSubstitutions(
      adContext.getAdditiveArithmeticProtocol(), astType, confRef);
  // %1 = metatype $T.Type
  auto metatypeType =
      CanMetatypeType::get(astType, MetatypeRepresentation::Thick);
  auto metatypeSILType = SILType::getPrimitiveObjectType(metatypeType);
  auto metatype = createMetatype(loc, metatypeSILType);
  // %2 = apply $0(%lhs, %rhs, %1)
  createApply(loc, witnessMethod, subMap,
              {destinationBuffer, operand, metatype});
  emitDestroyValueOperation(loc, witnessMethod);
}

void TangentBuilder::emitAddIntoBuffer(SILLocation loc,
                                       SILValue destinationBuffer,
                                       SILValue lhsAddress,
                                       SILValue rhsAddress) {
  assert(lhsAddress->getType().getASTType() ==
             rhsAddress->getType().getASTType() &&
         "Adjoint values must have same type!");
  assert(lhsAddress->getType().isAddress() &&
         rhsAddress->getType().isAddress() &&
         "Adjoint values must both have address types!");
  auto type = lhsAddress->getType();
  if (auto tupleType = type.getAs<TupleType>()) {
    for (unsigned i : range(tupleType->getNumElements())) {
      auto *destAddr = createTupleElementAddr(loc, destinationBuffer, i);
      auto *eltAddrLHS = createTupleElementAddr(loc, lhsAddress, i);
      auto *eltAddrRHS = createTupleElementAddr(loc, rhsAddress, i);
      emitAddIntoBuffer(loc, destAddr, eltAddrLHS, eltAddrRHS);
    }
    return;
  }
  auto astType = type.getASTType();
  auto *proto = adContext.getAdditiveArithmeticProtocol();
  auto *combinerFuncDecl = adContext.getPlusDecl();
  // Call the combiner function and return.
  auto *swiftMod = getModule().getSwiftModule();
  auto confRef = swiftMod->lookupConformance(astType, proto);
  assert(!confRef.isInvalid() &&
         "Missing conformance to `AdditiveArithmetic`");
  SILDeclRef declRef(combinerFuncDecl, SILDeclRef::Kind::Func);
  auto silFnTy = getModule().Types.getConstantType(
      getTypeExpansionContext(), declRef);
  // %0 = witness_method @+
  auto witnessMethod =
      createWitnessMethod(loc, astType, confRef, declRef, silFnTy);
  auto subMap =
      SubstitutionMap::getProtocolSubstitutions(proto, astType, confRef);
  // %1 = metatype $T.Type
  auto metatypeType =
      CanMetatypeType::get(astType, MetatypeRepresentation::Thick);
  auto metatypeSILType = SILType::getPrimitiveObjectType(metatypeType);
  auto metatype = createMetatype(loc, metatypeSILType);
  // %2 = apply %0(%result, %new, %old, %1)
  createApply(loc, witnessMethod, subMap,
              {destinationBuffer, rhsAddress, lhsAddress, metatype});
  emitDestroyValueOperation(loc, witnessMethod);
}

SILValue TangentBuilder::emitAdd(SILLocation loc, SILValue lhs, SILValue rhs) {
  LLVM_DEBUG(getADDebugStream() << "Emitting adjoint accumulation for lhs: "
                                << lhs << " and rhs: " << rhs);
  assert(lhs->getType() == rhs->getType() && "Adjoints must have equal types!");
  assert(lhs->getType().isObject() && rhs->getType().isObject() &&
         "Adjoint types must be both object types!");
  auto type = lhs->getType();
  auto lhsCopy = emitCopyValueOperation(loc, lhs);
  auto rhsCopy = emitCopyValueOperation(loc, rhs);
  // Allocate buffers for inputs and output.
  auto *resultBuf = createAllocStack(loc, type);
  auto *lhsBuf = createAllocStack(loc, type);
  auto *rhsBuf = createAllocStack(loc, type);
  // Initialize input buffers.
  emitStoreValueOperation(loc, lhsCopy, lhsBuf);
  emitStoreValueOperation(loc, rhsCopy, rhsBuf);
  emitAddIntoBuffer(loc, resultBuf, lhsBuf, rhsBuf);
  emitDestroyAddr(loc, lhsBuf);
  emitDestroyAddr(loc, rhsBuf);
  // Deallocate input buffers.
  createDeallocStack(loc, rhsBuf);
  createDeallocStack(loc, lhsBuf);
  auto val = emitLoadValueOperation(loc, resultBuf,
                                    LoadOwnershipQualifier::Take);
  // Deallocate result buffer.
  createDeallocStack(loc, resultBuf);
  return val;
}

} // end namespace autodiff
} // end namespace swift
