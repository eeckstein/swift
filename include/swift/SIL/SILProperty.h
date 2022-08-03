//===--- SILProperty.h - Defines the SILProperty class ----------*- C++ -*-===//
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
// This file defines the SILProperty class, which is used to capture the
// metadata about a property definition necessary for it to be resiliently
// included in KeyPaths across modules.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILPROPERTY_H
#define SWIFT_SIL_SILPROPERTY_H

#include "swift/AST/GenericSignature.h"
#include "swift/SIL/SILAllocated.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/ADT/ilist.h"

namespace swift {

class SILPrintContext;
  
/// A descriptor for a public property or subscript that can be resiliently
/// referenced from key paths in external modules.
class SILProperty : public llvm::ilist_node<SILProperty>,
                    public SILAllocated<SILProperty>,
                    public SILFunctionReference::Owner
{
private:
  /// True if serialized.
  bool Serialized;
  
  /// The declaration the descriptor represents.
  AbstractStorageDecl *Decl;
  
  /// The key path component that represents its implementation.
  Optional<KeyPathPatternComponent> Component;

  SILProperty(bool Serialized,
              AbstractStorageDecl *Decl,
              Optional<KeyPathPatternComponent> Component)
    : Owner(FunctionOwnerKind::Property), Serialized(Serialized), Decl(Decl), Component(Component)
  {
    if (Component.hasValue())
      Component.getValue().setOwner(this);
  }

public:
  ~SILProperty() {
    if (Component.hasValue())
      Component.getValue().dropAllReferences();
  }

  static SILProperty *create(SILModule &M,
                             bool Serialized,
                             AbstractStorageDecl *Decl,
                             Optional<KeyPathPatternComponent> Component);
  
  bool isSerialized() const { return Serialized; }
  
  AbstractStorageDecl *getDecl() const { return Decl; }
  
  bool isTrivial() const {
    return !Component.hasValue();
  }
  
  const Optional<KeyPathPatternComponent> &getComponent() const {
    return Component;
  }
  
  void print(SILPrintContext &Ctx) const;
  void dump() const;
  
  void verify(const SILModule &M) const;
};

template <> SILProperty *SILFunctionReference::Owner::getAs<SILProperty>() {
  return functionOwnerKind == FunctionOwnerKind::Property ? static_cast<SILProperty *>(this) : nullptr;
}

} // end namespace swift

namespace llvm {

//===----------------------------------------------------------------------===//
// ilist_traits for SILProperty
//===----------------------------------------------------------------------===//

template <>
struct ilist_traits<::swift::SILProperty>
    : public ilist_node_traits<::swift::SILProperty> {
  using SILProperty = ::swift::SILProperty;

public:
  static void deleteNode(SILProperty *VT) { VT->~SILProperty(); }

private:
  void createNode(const SILProperty &);
};

} // namespace llvm

#endif
