//===--- PrettyStackTrace.h - PrettyStackTrace for Transforms ---*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_PASSMANAGER_PRETTYSTACKTRACE_H
#define SWIFT_SILOPTIMIZER_PASSMANAGER_PRETTYSTACKTRACE_H

#include "swift/SIL/PrettyStackTrace.h"
#include "llvm/Support/PrettyStackTrace.h"

namespace swift {

class SILFunctionTransform;
class SILModuleTransform;

class PrettyStackTraceSILFunctionTransform
    : public PrettyStackTraceSILFunction {
  StringRef passName;
  unsigned PassNumber;

public:
  PrettyStackTraceSILFunctionTransform(SILFunction *function,
                                       StringRef passName,
                                       unsigned PassNumber);

  virtual void print(llvm::raw_ostream &OS) const override;
};

class PrettyStackTraceSILModuleTransform : public llvm::PrettyStackTraceEntry {
  StringRef passName;
  unsigned PassNumber;

public:
  PrettyStackTraceSILModuleTransform(StringRef passName,
                                     unsigned PassNumber)
      : passName(passName), PassNumber(PassNumber) {}
  virtual void print(llvm::raw_ostream &OS) const override;
};

} // end namespace swift

#endif
