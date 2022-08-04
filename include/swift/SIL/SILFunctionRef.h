//===--- SILFunctionRef.h - a reference to a SILFunction --------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILFUNCTIONREF_H
#define SWIFT_SIL_SILFUNCTIONREF_H

#include "swift/Basic/Compiler.h"

namespace swift {

class SILFunction;

/// A reference to a SILFunction.
///
/// Allows to get all uses of a function.
/// `SILFunctionRef` for `SILFunction` is like `Operand` for `SILValue`.
class SILFunctionRef {
public:
  /// Possible users of a SILFunction.
  enum UserKind {
    FunctionRefInst, Function, WitnessTable, DefaultWitnessTable,
    KeyPathPattern, VTable, Property
  };

  /// The abstract base class of a function user.
  class User {
    UserKind functionUserKind;
  protected:
    User(int kind) : functionUserKind((UserKind)kind) {}
    User(const User &other) = delete;
  public:
  
    /// Dynamically casts to a specific user.
    template <class C> C *getAs() {
      if (functionUserKind == C::FunctionUserKind)
        return static_cast<C *>(this);
      return nullptr;
    }
  };

  // To be used in the inheritance clause of a user class.
  template <int Kind> class UserWithKind : public User {
  public:
    UserWithKind() : User(Kind) {}
    enum { FunctionUserKind = Kind };
  };

private:
  /// The referenced function.
  SILFunction *function = nullptr;

  SILFunctionRef *next = nullptr;
  SILFunctionRef **prevPtr = nullptr;

  // TODO: ideally the user would always be set. But there are many places where
  // e.g. we create a witness table entry and only afterwards create the witness
  // table (which is the user) itself. So we support creating a SILFunctionRef
  // with a null user and set the user afterwards.
  // The SILFunctionRef is only inserted in the use list once an user is set.
  User *user = nullptr;

public:

  // For iterating over a SILFunction's use list.
  class Iterator {
    SILFunctionRef *current = nullptr;
  public:
    Iterator() = default;
    explicit Iterator(SILFunctionRef *r) : current(r) {}
    User *operator->() const { return current->getUser(); }
    User *operator*() const { return current->getUser(); }
    
    Iterator &operator++() {
      assert(current && "can't increment end-iterator");
      current = current->next;
      return *this;
    }
    
    friend bool operator==(Iterator lhs, Iterator rhs) {
      return lhs.current == rhs.current;
    }
    friend bool operator!=(Iterator lhs, Iterator rhs) {
      return !(lhs == rhs);
    }
  };

  SILFunctionRef() {}
  SILFunctionRef(SILFunction *f) : function(f) {}
  SILFunctionRef(SILFunction *f, User *u) : function(f), user(u) {
    insertIntoCurrent();
  }
  ~SILFunctionRef() { removeFromCurrent(); }

  SILFunctionRef(const SILFunctionRef &other)
      : SILFunctionRef(other.function) {
    assert(!other.user && "function ref shouldn't be copied once it has an user");
  }

  SILFunctionRef &operator=(SILFunction *f) {
    removeFromCurrent();
    function = f;
    if (user)
      insertIntoCurrent();
    return *this;
  }

  SILFunctionRef &operator=(const SILFunctionRef &other) {
    *this = other.function;
    return *this;
  }

  SILFunction *operator->() const { return function; }
  operator SILFunction *() const { return function; }

  User *getUser() const { return user; }

  void setUser(User *u) {
    assert(!user && "cannot change user");
    user = u;
    insertIntoCurrent();
  }

private:

  void removeFromCurrent() {
    if (prevPtr) {
      assert(user && "function ref can only be linked with an user");
      *prevPtr = next;
      if (next)
        next->prevPtr = prevPtr;
      prevPtr = nullptr;
      function = nullptr;
    }
  }

  // Implemented in SILFunction.cpp.
  void insertIntoCurrent();
};

} // end swift namespace

#endif
