#===-----------------------------------------------------------------------===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===-----------------------------------------------------------------------===//

atomicTypes = [
  # Swift                  Size    Alignment  Builtin
  ("AtomicInt8Storage",     "8",         "1", "Builtin.Int8"),
  ("AtomicInt16Storage",   "16",         "2", "Builtin.Int16"),
  ("AtomicInt32Storage",   "32",         "4", "Builtin.Int32"),
  ("AtomicInt64Storage",   "64",         "8", "Builtin.Int64"),
  ("AtomicInt128Storage", "128",        "16", "Builtin.Int128"),
]

intTypes = [
  # Swift   Storage Type 
  ("Int8",  "AtomicInt8Storage",   "Int8"),
  ("Int16", "AtomicInt16Storage",  "Int16"),
  ("Int32", "AtomicInt32Storage",  "Int32"),
  ("Int64", "AtomicInt64Storage",  "Int64"),

  # We handle the word type's storage in source.
  ("Int",   "",                    "Word"),

  ("UInt8",  "AtomicInt8Storage",  "Int8"),
  ("UInt16", "AtomicInt16Storage", "Int16"),
  ("UInt32", "AtomicInt32Storage", "Int32"),
  ("UInt64", "AtomicInt64Storage", "Int64"),

  # We handle the word type's storage in source.
  ("UInt",   "",                   "Word"),
]

loadOrderings = [
  # Swift                    API name                  doc name                   LLVM name
  ("relaxed",                "Relaxed",                "relaxed",                 "monotonic"),
  ("acquiring",              "Acquiring",              "acquiring",               "acquire"),
  ("sequentiallyConsistent", "SequentiallyConsistent", "sequentially consistent", "seqcst")
]

storeOrderings = [
  # Swift enum case,         API name,                 doc name,                  LLVM name
  ("relaxed",                "Relaxed",                "relaxed",                 "monotonic"),
  ("releasing",              "Releasing",              "releasing",               "release"),
  ("sequentiallyConsistent", "SequentiallyConsistent", "sequentially consistent", "seqcst")
]

updateOrderings = [
  # Swift enum case,         API name,                 doc name,                  LLVM name,   failure name
  ("relaxed",                "Relaxed",                "relaxed",                 "monotonic", "monotonic"),
  ("acquiring",              "Acquiring",              "acquiring",               "acquire",   "acquire"),
  ("releasing",              "Releasing",              "releasing",               "release",   "monotonic"),
  ("acquiringAndReleasing",  "AcquiringAndReleasing",  "acquiring-and-releasing", "acqrel",    "acquire"),
  ("sequentiallyConsistent", "SequentiallyConsistent", "sequentially consistent", "seqcst",    "seqcst"),
]

integerOperations = [
  # Swift name,         llvm name,  operator, label,  doc name
  ("WrappingIncrement", "add",      "&+",     "by",   "wrapping add"),
  ("WrappingDecrement", "sub",      "&-",     "by",   "wrapping subtract"),
  ("BitwiseAnd",        "and",      "&",      "with", "bitwise AND"),
  ("BitwiseOr",         "or",       "|",      "with", "bitwise OR"),
  ("BitwiseXor",        "xor",      "^",      "with", "bitwise XOR"),

  # These two are handled specially in source.
  ("Min",               "min",      "",       "with", "minimum"),
  ("Max",               "max",      "",       "with", "maximum")
]

boolOperations = [
  # Swift name,  llvm name, operator, label,  doc
  ("LogicalAnd", "and",     "&&",     "with", "logical AND"),
  ("LogicalOr",  "or",      "||",     "with", "logical OR"),
  ("LogicalXor", "xor",     "!=",     "with", "logical XOR")
]

# LLVM doesn't support arbitrary ordering combinations yet, so for the
# two-ordering cmpxchg variants we need to upgrade the success
# ordering when necessary so that it is at least as "strong" as the
# failure case. This function implements that mapping.
#
# See llvm/Support/AtomicOrdering.h
def actualOrders(success, failure):
  def max(success, failure):
    if failure == "acquire":
      if success == "monotonic":
       return "acquire"
      if success == "release":
       return "acqrel"
    if failure == "seqcst":
      return "seqcst"
    return success
  actualSuccess = max(success, failure)
  return actualSuccess + "_" + failure

def llvmToCaseName(ordering):
  if ordering == "monotonic":
    return "relaxed"
  if ordering == "acquire":
    return "acquiring"
  if ordering == "release":
    return "releasing"
  if ordering == "acqrel":
    return "acquiringAndReleasing"
  if ordering == "seqcst":
    return "sequentiallyConsistent"

def atomicOperationName(intType, operation):
  if operation == "Min":
    return "umin" if intType.startswith("U") else "min"
  if operation == "Max":
    return "umax" if intType.startswith("U") else "max"
  return operation

def lowerFirst(str):
  return str[:1].lower() + str[1:] if str else ""
