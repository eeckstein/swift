//===--- LoadAndStoreSplitting.swift --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SIL

extension StoreInst {
  @discardableResult
  func trySplit(_ context: FunctionPassContext) -> [StoreInst]? {
    let builder = Builder(after: self, context)
    let type = source.type
    let elementStores: [StoreInst]
    if type.isStruct {
      guard !(type.nominal as! StructDecl).hasUnreferenceableStorage,
            let fields = type.getNominalFields(in: parentFunction)
      else {
        return nil
      }
      if parentFunction.hasOwnership && source.ownership != .none {
        let destructure = builder.createDestructureStruct(struct: source)
        elementStores = destructure.results.enumerated().map { (idx, v) in
          let destFieldAddr = builder.createStructElementAddr(structAddress: destination, fieldIndex: idx)
          return builder.createStore(source: v, destination: destFieldAddr, ownership: splitOwnership(for: v))
        }
      } else {
        elementStores = fields.indices.map { idx in
          let v = builder.createStructExtract(struct: source, fieldIndex: idx)
          let fieldAddr = builder.createStructElementAddr(structAddress: destination, fieldIndex: idx)
          return builder.createStore(source: v, destination: fieldAddr, ownership: splitOwnership(for: v))
        }
      }
    } else if type.isTuple {
      if parentFunction.hasOwnership && source.ownership != .none {
        let destructure = builder.createDestructureTuple(tuple: source)
        elementStores = destructure.results.enumerated().map { (idx, v) in
          let elementAddr = builder.createTupleElementAddr(tupleAddress: destination, elementIndex: idx)
          return builder.createStore(source: v, destination: elementAddr, ownership: splitOwnership(for: v))
        }
      } else {
        elementStores = type.tupleElements.indices.map { idx in
          let v = builder.createTupleExtract(tuple: source, elementIndex: idx)
          let destFieldAddr = builder.createTupleElementAddr(tupleAddress: destination, elementIndex: idx)
          return builder.createStore(source: v, destination: destFieldAddr, ownership: splitOwnership(for: v))
        }
      }
    } else {
      return nil
    }
    context.erase(instruction: self)
    return elementStores
  }

  func canSplit(alongPath projectionPath: SmallProjectionPath) -> Bool {
    return source.type.canSplit(alongPath: projectionPath, in: parentFunction)
  }

  func split(alongPath projectionPath: SmallProjectionPath,
             _ context: FunctionPassContext,
             _ visitSplitStoreInst: (StoreInst, Bool) -> () = { store, isProjectedStore in }
    ) {
    if projectionPath.isEmpty {
      visitSplitStoreInst(self, true)
      return
    }

    guard let splitStores = trySplit(context) else {
      fatalError("unsupported type to split")
    }

    let (_, projectionIndex, pathRemainder) = projectionPath.pop()

    for (elementIdx, splitStore) in splitStores.enumerated() {
      if elementIdx == projectionIndex {
        splitStore.split(alongPath: pathRemainder, context, visitSplitStoreInst)
      } else {
        visitSplitStoreInst(splitStore, false)
      }
    }
  }

  private func splitOwnership(for fieldValue: Value) -> StoreOwnership {
    switch self.storeOwnership {
    case .trivial, .unqualified:
      return self.storeOwnership
    case .assign, .initialize:
      return fieldValue.type.isTrivial(in: parentFunction) ? .trivial : self.storeOwnership
    }
  }
}

extension StoreAndBorrowInst {
  @discardableResult
  func trySplit(_ context: FunctionPassContext) -> [StoringInstruction]? {
    guard allScopeEndsAreEndBorrows else {
      return nil
    }
    let builder = Builder(after: self, context)
    let type = source.type
    if type.isStruct {
      guard !(type.nominal as! StructDecl).hasUnreferenceableStorage,
            type.getNominalFields(in: parentFunction) != nil
      else {
        return nil
      }
      let destructure = builder.createDestructureStruct(struct: source)
      let elementStores = destructure.results.enumerated().map { (idx, v) in
        let destFieldAddr = builder.createStructElementAddr(structAddress: destination, fieldIndex: idx)
        return createSplitStore(of: v, to: destFieldAddr, context, builder)
      }
      let newStruct = builder.createStruct(type: self.type, elements: elementStores.map {
        switch $0 {
        case let s as StoreInst:            return s.source
        case let sab as StoreAndBorrowInst: return sab
        default: fatalError("unknown element store")
        }
      })
      context.erase(instructions: self.uses.users(ofType: EndBorrowInst.self))
      self.replace(with: newStruct, context)
      updateBorrowedFrom(for: Array(newStruct.uses.users(ofType: BorrowedFromInst.self).map{ $0.borrowedPhi }), context)
      return elementStores
    } else if type.isTuple {
      let destructure = builder.createDestructureTuple(tuple: source)
      let elementStores = destructure.results.enumerated().map { (idx, v) in
        let elementAddr = builder.createTupleElementAddr(tupleAddress: destination, elementIndex: idx)
        return createSplitStore(of: v, to: elementAddr, context, builder)
      }
      let newTuple = builder.createTuple(type: self.type, elements: elementStores.map {
        switch $0 {
        case let s as StoreInst:            return s.source
        case let sab as StoreAndBorrowInst: return sab
        default: fatalError("unknown element store")
        }
      })
      context.erase(instructions: self.uses.users(ofType: EndBorrowInst.self))
      updateBorrowedFrom(for: Array(newTuple.uses.users(ofType: BorrowedFromInst.self).map{ $0.borrowedPhi }), context)
      self.replace(with: newTuple, context)
      return elementStores
    }
    return nil
  }

  func canSplit(alongPath projectionPath: SmallProjectionPath) -> Bool {
    return allScopeEndsAreEndBorrows && source.type.canSplit(alongPath: projectionPath, in: parentFunction)
  }

  func split(alongPath projectionPath: SmallProjectionPath,
             _ context: FunctionPassContext,
             _ visitSplitStoreInst: (StoringInstruction, Bool) -> () = { store, isProjectedStore in }
    ) {
    if projectionPath.isEmpty {
      visitSplitStoreInst(self, true)
      return
    }

    guard let splitStores = trySplit(context) else {
      fatalError("unsupported type to split")
    }

    let (_, projectionIndex, pathRemainder) = projectionPath.pop()

    for (elementIdx, splitStore) in splitStores.enumerated() {
      if elementIdx == projectionIndex {
        switch splitStore {
        case let store as StoreInst:
          store.split(alongPath: pathRemainder, context, visitSplitStoreInst)
        case let storeAndBorrow as StoreAndBorrowInst:
          storeAndBorrow.split(alongPath: pathRemainder, context, visitSplitStoreInst)
        default:
          fatalError("wrong split store")
        }
      } else {
        visitSplitStoreInst(splitStore, false)
      }
    }
  }

  private func createSplitStore(of value: Value, to address: Value,
                                _ context: FunctionPassContext,
                                _ builder: Builder
  ) -> StoringInstruction {
    if value.type.isTrivial(in: parentFunction) {
      return builder.createStore(source: value, destination: address, ownership: .trivial)
    }
    let splitStore = builder.createStoreAndBorrow(source: value, destination: address)
    for endBorrow in self.uses.endingLifetime.users {
      Builder(before: endBorrow, context).createEndBorrow(of: splitStore)
    }
    return splitStore
  }
}

extension LoadInst {
  @discardableResult
  func trySplit(_ context: FunctionPassContext) -> [LoadInst]? {
    if type.isStruct {
      guard !(type.nominal as! StructDecl).hasUnreferenceableStorage,
            let fields = type.getNominalFields(in: parentFunction)
      else {
        return nil
      }
      let builder = Builder(before: self, context)
      let elements = fields.indices.map {
        let fieldAddr = builder.createStructElementAddr(structAddress: address, fieldIndex: $0)
        return builder.createLoad(fromAddress: fieldAddr, ownership: self.splitOwnership(for: fieldAddr))
      }
      let newStruct = builder.createStruct(type: self.type, elements: elements)
      self.replace(with: newStruct, context)
      return elements
    }
    if type.isTuple {
      let builder = Builder(before: self, context)
      let elements = type.tupleElements.indices.map {
        let fieldAddr = builder.createTupleElementAddr(tupleAddress: address, elementIndex: $0)
        return builder.createLoad(fromAddress: fieldAddr, ownership: self.splitOwnership(for: fieldAddr))
      }
      let newTuple = builder.createTuple(type: self.type, elements: elements)
      self.replace(with: newTuple, context)
      return elements
    }
    return nil
  }

  private func splitOwnership(for fieldValue: Value) -> LoadOwnership {
    switch self.loadOwnership {
    case .trivial, .unqualified:
      return self.loadOwnership
    case .copy, .take:
      return fieldValue.type.isTrivial(in: parentFunction) ? .trivial : self.loadOwnership
    }
  }

  func canSplit(alongPath projectionPath: SmallProjectionPath) -> Bool {
    return type.canSplit(alongPath: projectionPath, in: parentFunction)
  }

  func split(alongPath projectionPath: SmallProjectionPath,
             _ context: FunctionPassContext,
             _ visitSplitLoadInst: (LoadInst, Bool) -> () = { load, isProjectedLoad in }
    ) {
    if projectionPath.isEmpty {
      visitSplitLoadInst(self, true)
      return
    }

    guard let splitLoads = trySplit(context) else {
      fatalError("unsupported type to split")
    }

    let (_, projectionIndex, pathRemainder) = projectionPath.pop()

    for (elementIdx, splitLoad) in splitLoads.enumerated() {
      if elementIdx == projectionIndex {
        splitLoad.split(alongPath: pathRemainder, context, visitSplitLoadInst)
      } else {
        visitSplitLoadInst(splitLoad, false)
      }
    }
  }
}

extension LoadBorrowInst {
  @discardableResult
  func trySplit(_ context: FunctionPassContext) -> [LoadInstruction]? {
    guard allScopeEndsAreEndBorrows else {
      return nil
    }
    if type.isStruct {
      guard !(type.nominal as! StructDecl).hasUnreferenceableStorage,
            let fields = type.getNominalFields(in: parentFunction)
      else {
        return nil
      }
      let builder = Builder(before: self, context)
      let elements = fields.indices.map {
        let fieldAddr = builder.createStructElementAddr(structAddress: address, fieldIndex: $0)
        return createSplitLoad(from: fieldAddr, context, builder)
      }
      let newStruct = builder.createStruct(type: self.type, elements: elements)
      context.erase(instructions: self.uses.users(ofType: EndBorrowInst.self))
      self.replace(with: newStruct, context)
      updateBorrowedFrom(for: Array(newStruct.uses.users(ofType: BorrowedFromInst.self).map{ $0.borrowedPhi }), context)
      return elements
    }
    if type.isTuple {
      let builder = Builder(before: self, context)
      let elements = type.tupleElements.indices.map {
        let fieldAddr = builder.createTupleElementAddr(tupleAddress: address, elementIndex: $0)
        return createSplitLoad(from: fieldAddr, context, builder)
      }
      let newTuple = builder.createTuple(type: self.type, elements: elements)
      context.erase(instructions: self.uses.users(ofType: EndBorrowInst.self))
      updateBorrowedFrom(for: Array(newTuple.uses.users(ofType: BorrowedFromInst.self).map{ $0.borrowedPhi }), context)
      self.replace(with: newTuple, context)
      return elements
    }
    return nil
  }

  func canSplit(alongPath projectionPath: SmallProjectionPath) -> Bool {
    return allScopeEndsAreEndBorrows && type.canSplit(alongPath: projectionPath, in: parentFunction)
  }

  func split(alongPath projectionPath: SmallProjectionPath,
             _ context: FunctionPassContext,
             _ visitSplitLoadInst: (LoadInstruction, Bool) -> () = { load, isProjectedLoad in }
  ) {
    if projectionPath.isEmpty {
      visitSplitLoadInst(self, true)
      return
    }

    guard let splitLoads = trySplit(context) else {
      fatalError("unsupported type to split")
    }

    let (_, projectionIndex, pathRemainder) = projectionPath.pop()

    for (elementIdx, splitLoad) in splitLoads.enumerated() {
      if elementIdx == projectionIndex {
        switch splitLoad {
        case let load as LoadInst:
          load.split(alongPath: pathRemainder, context, visitSplitLoadInst)
        case let loadBorrow as LoadBorrowInst:
          loadBorrow.split(alongPath: pathRemainder, context, visitSplitLoadInst)
        default:
          fatalError("wrong split store")
        }
      } else {
        visitSplitLoadInst(splitLoad, false)
      }
    }
  }

  private func createSplitLoad(from address: Value,
                                _ context: FunctionPassContext,
                                _ builder: Builder
  ) -> LoadInstruction {
    if address.type.isTrivial(in: parentFunction) {
      return builder.createLoad(fromAddress: address, ownership: .trivial)
    }
    let splitLoad = builder.createLoadBorrow(fromAddress: address)
    for endBorrow in self.uses.endingLifetime.users {
      Builder(before: endBorrow, context).createEndBorrow(of: splitLoad)
    }
    return splitLoad
  }
}

extension CopyAddrInst {
  @discardableResult
  func trySplit(_ context: FunctionPassContext) -> Bool {
    let builder = Builder(before: self, context)
    if source.type.isStruct {
      if (source.type.nominal as! StructDecl).hasUnreferenceableStorage {
        return false
      }
      guard let fields = source.type.getNominalFields(in: parentFunction) else {
        return false
      }
      for idx in 0..<fields.count {
        let srcFieldAddr = builder.createStructElementAddr(structAddress: source, fieldIndex: idx)
        let destFieldAddr = builder.createStructElementAddr(structAddress: destination, fieldIndex: idx)
        builder.createCopyAddr(from: srcFieldAddr, to: destFieldAddr,
                               takeSource: isTake(for: srcFieldAddr), initializeDest: isInitializationOfDestination)
      }
      context.erase(instruction: self)
      return true
    } else if source.type.isTuple {
      let builder = Builder(before: self, context)
      for idx in 0..<source.type.tupleElements.count {
        let srcFieldAddr = builder.createTupleElementAddr(tupleAddress: source, elementIndex: idx)
        let destFieldAddr = builder.createTupleElementAddr(tupleAddress: destination, elementIndex: idx)
        builder.createCopyAddr(from: srcFieldAddr, to: destFieldAddr,
                               takeSource: isTake(for: srcFieldAddr), initializeDest: isInitializationOfDestination)
      }
      context.erase(instruction: self)
      return true
    }
    return false
  }

  private func isTake(for fieldValue: Value) -> Bool {
    return isTakeOfSource && !fieldValue.type.objectType.isTrivial(in: parentFunction)
  }
}

extension Type {
  func canSplit(alongPath projectionPath: SmallProjectionPath, in function: Function) -> Bool {
    if projectionPath.isEmpty {
      return true
    }
    let (fieldKind, index, pathRemainder) = projectionPath.pop()

    switch fieldKind {
    case .structField where self.isStruct:
      guard !(nominal as! StructDecl).hasUnreferenceableStorage,
            let fields = getNominalFields(in: function)
      else {
        return false
      }
      return fields[index].canSplit(alongPath: pathRemainder, in: function)
    case .tupleField where self.isTuple:
      return tupleElements[index].canSplit(alongPath: pathRemainder, in: function)
    default:
      return false
    }
  }
}

private extension Value {
  var allScopeEndsAreEndBorrows: Bool {
    uses.endingLifetime.ignore(usersOfType: EndBorrowInst.self).isEmpty
  }
}
