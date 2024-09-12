//===--- PrettyStackTrace.cpp ---------------------------------------------===//
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

#include "swift/SILOptimizer/PassManager/PrettyStackTrace.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

PrettyStackTraceSILFunctionTransform::PrettyStackTraceSILFunctionTransform(
  SILFunction *function, StringRef passName, unsigned PassNumber):
  PrettyStackTraceSILFunction("Running SIL Function Transform",
                              function),
  passName(passName), PassNumber(PassNumber) {}

void PrettyStackTraceSILFunctionTransform::print(llvm::raw_ostream &out) const {
  fflush(stdout);
  out << "While running pass #" << PassNumber
      << " SILFunctionTransform \"" << passName
      << "\" on SILFunction ";
  printFunctionInfo(out);
}

void PrettyStackTraceSILModuleTransform::print(llvm::raw_ostream &out) const {
  fflush(stdout);
  out << "While running pass #" << PassNumber
      << " SILModuleTransform \"" << passName << "\".\n";
}
