//===--- SILVTable.cpp - Defines the SILVTable class ----------------------===//
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
// This file defines the SILVTable class, which is used to map dynamically
// dispatchable class methods and properties to their concrete implementations
// for a dynamic type. This information (FIXME) will be used by IRGen to lay
// out class vtables, and can be used by devirtualization passes to lower
// class_method instructions to static function_refs.
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILVTable.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

SILVTable *SILVTable::create(SILModule &M, ClassDecl *Class,
                             IsSerialized_t Serialized,
                             ArrayRef<Entry> Entries) {
  auto size = totalSizeToAlloc<Entry>(Entries.size());
  auto buf = M.allocate(size, alignof(SILVTable));
  SILVTable *vt = ::new (buf) SILVTable(Class, Serialized, Entries);
  M.vtables.push_back(vt);
  M.VTableMap[Class] = vt;
  // Update the Module's cache with new vtable + vtable entries:
  unsigned indexOfEntry = 0;
  for (const Entry &entry : Entries) {
    M.VTableEntryCache.insert({{vt, entry.getMethod()}, indexOfEntry++});
  }
  return vt;
}

const SILVTable::Entry *SILVTable::getEntry(SILModule &M, SILDeclRef method) const {
  SILDeclRef m = method;
  do {
    auto entryIter = M.VTableEntryCache.find({this, m});
    if (entryIter != M.VTableEntryCache.end()) {
      const Entry *entry = &getEntries()[entryIter->second];
      assert(entry->getMethod() == m);
      return entry;
    }
  } while ((m = m.getOverridden()));
  return nullptr;
}

void SILVTable::removeFromVTableCache(Entry &entry) {
  SILModule &M = entry.getImplementation()->getModule();
  M.VTableEntryCache.erase({this, entry.getMethod()});
}

void SILVTable::updateVTableCache(SILModule &module) {
  unsigned indexOfEntry = 0;
  for (const Entry &entry : getEntries()) {
    module.VTableEntryCache[{this, entry.getMethod()}] = indexOfEntry++;
  }
}

SILVTable::SILVTable(ClassDecl *c, IsSerialized_t serialized,
                     ArrayRef<Entry> entries)
  : Owner(FunctionOwnerKind::VTable),
    Class(c), Serialized(serialized), NumEntries(entries.size()) {
  std::uninitialized_copy(entries.begin(), entries.end(),
                          getTrailingObjects<Entry>());
  for (auto &entry : getMutableEntries()) {
    entry.setOwner(this);
  }
}
