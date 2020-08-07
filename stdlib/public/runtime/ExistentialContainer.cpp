//===--- ExistentialContainer.cpp -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/ExistentialContainer.h"
#include "swift/Runtime/HeapObject.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                 OpaqueExistentialContainer Implementation
//===----------------------------------------------------------------------===//

template <>
bool OpaqueExistentialContainer::isValueInline() const {
#ifdef TINY_SWIFT
  return true;
#else
  return Type->getValueWitnesses()->isValueInline();
#endif
}

template <>
const OpaqueValue *OpaqueExistentialContainer::projectValue() const {
#ifdef TINY_SWIFT
  return reinterpret_cast<const OpaqueValue *>(&Buffer);
#else
  auto *vwt = Type->getValueWitnesses();

  if (vwt->isValueInline())
    return reinterpret_cast<const OpaqueValue *>(&Buffer);

  // Compute the byte offset of the object in the box.
  unsigned alignMask = vwt->getAlignmentMask();
  unsigned byteOffset = (sizeof(HeapObject) + alignMask) & ~alignMask;
  auto *bytePtr = reinterpret_cast<const char *>(
      *reinterpret_cast<HeapObject *const *const>(&Buffer));
  return reinterpret_cast<const OpaqueValue *>(bytePtr + byteOffset);
#endif
}

template <>
void OpaqueExistentialContainer::deinit() {
#ifndef TINY_SWIFT
  auto *vwt = Type->getValueWitnesses();
  if (vwt->isValueInline()) {
    return;
  }

  unsigned alignMask = vwt->getAlignmentMask();
  unsigned size = vwt->size;
  swift_deallocObject(*reinterpret_cast<HeapObject **>(&Buffer), size,
                      alignMask);
#endif
}

#ifndef NDEBUG

// *NOTE* This routine performs unused memory reads on purpose to try to catch
// use-after-frees in conjunction with ASAN or Guard Malloc.
template <> SWIFT_USED void OpaqueExistentialContainer::verify() const {
  // We do not actually care about value. We just want to see if the
  // memory is valid or not. So convert to a uint8_t and try to
  // memcpy into firstByte. We use volatile to just ensure that this
  // does not get dead code eliminated.
  uint8_t firstByte;
  memcpy(&firstByte, projectValue(), 1);
  volatile uint8_t firstVolatileByte = firstByte;
  (void)firstVolatileByte;
}

/// Dump information about this specific container and its contents.
template <> SWIFT_USED void OpaqueExistentialContainer::dump() const {
  // Quickly verify to make sure we are well formed.
  verify();

  printf("TargetOpaqueExistentialContainer.\n");
#ifdef TINY_SWIFT
  printf("Value pattern: 0x%x.\n", Pattern);
#else
  printf("Metadata Pointer: %p.\n", Type);
#endif
  printf("Value Pointer: %p.\n", projectValue());
  printf("Is Value Stored Inline: %s.\n", isValueInline() ? "true" : "false");
}

#endif

/***************************************************************************/
/*** Tiny-Swift runtime functions ******************************************/
/***************************************************************************/

#ifdef TINY_SWIFT

SWIFT_RUNTIME_EXPORT
void _swift_copyExistential(OpaqueExistentialContainer *dest,
                            OpaqueExistentialContainer *src,
                            unsigned numTables) {
  intptr_t pattern = src->Pattern;
  for (unsigned idx = 0; idx < 3; idx++) {
    switch (pattern & 0x3) {
      case 0:
        break;
      case 1:
        swift_retain(*(HeapObject **)&src->Buffer.PrivateData[idx]);
        break;
      default:
        swift_runtime_unreachable("unsupported value pattern");
    }
    pattern >>= 3;
  }
  memcpy(dest, src, sizeof(OpaqueExistentialContainer) + numTables * sizeof(void *));
}

SWIFT_RUNTIME_EXPORT
void _swift_destroyExistential(OpaqueExistentialContainer *e) {
  intptr_t pattern = e->Pattern;
  for (unsigned idx = 0; idx < 3; idx++) {
    switch (pattern & 0x3) {
      case 0:
        break;
      case 1:
        swift_release(*(HeapObject **)&e->Buffer.PrivateData[idx]);
        break;
      default:
        swift_runtime_unreachable("unsupported value pattern");
    }
    pattern >>= 3;
  }
}

#endif // TINY_SWIFT
