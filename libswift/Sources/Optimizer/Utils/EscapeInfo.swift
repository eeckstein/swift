//===--- EscapeInfo.swift - the alias analysis -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension Escapes {
  public static func |(lhs: Escapes, rhs: Escapes) -> Escapes {
    switch (lhs, rhs) {
      case (.noEscape, let rhs): return rhs
      case (let lhs, .noEscape): return lhs
      case (.toReturn, .toReturn): return .toReturn
      case (.toArgument(let lhsArg), .toArgument(let rhsArg)) where lhsArg == rhsArg:
        return .toArgument(lhsArg)
      default:
        return .toGlobal
    }
  }
  public static func |=(lhs: inout Escapes, rhs: Escapes) {
    lhs = lhs | rhs
  }
}

struct EscapeInfo {

  private struct Path : CustomStringConvertible, Hashable {
    private let bytes: UInt64

    enum FieldKind : Int {
      case root = 0
      case anything = 1
      case anyValueField = 2
      case structField = 3
      case tupleField = 4
      case enumCase = 5
      case classField = 6
      case tailElements = 7
      
      var isValueField: Bool {
        switch self {
          case .anyValueField, .structField, .tupleField, .enumCase:
            return true
          case .root, .anything, .classField, .tailElements:
            return false
        }
      }
    }

    init() { self.bytes = 0 }
    
    init(pattern: Effect.Pattern) {
      self.init()
      switch pattern {
        case .firstLevel: self = push(kind: .anyValueField)
        case .transitive: self = push(kind: .anything)
      }
    }

    static var matchingAll: Path {
      Path().push(kind: .anything)
    }

    init(bytes: UInt64) { self.bytes = bytes }

    var isEmpty: Bool { bytes == 0 }

    var description: String {
      var descr = ""
      var p = self
      while !p.isEmpty {
        let (kind, idx, numBits) = p.top
        let s: String
        switch kind {
          case .root:          fatalError()
          case .anything:      s = "**"
          case .anyValueField: s = "*"
          case .structField:   s = "s#\(idx)"
          case .tupleField:    s = "t#\(idx)"
          case .enumCase:      s = "e#\(idx)"
          case .classField:    s = "c#\(idx)"
          case .tailElements:  s = "tail"
        }
        descr = s + (descr.isEmpty ? "" : ".\(descr)")
        p = p.pop(numBits: max(numBits, 8))
      }
      return "[\(descr)]"
    }
    
    var top: (kind: FieldKind, index: Int, numBits: Int) {
      var idx = 0
      var b = bytes
      var numBits = 0
      while (b & 1) == 1 {
        idx = (idx << 7) | Int((b >> 1) &  0x7f)
        b >>= 8
        numBits = numBits &+ 8
      }
      let k = FieldKind(rawValue: Int((b >> 1) & 0x7))!
      idx = (idx << 4) | Int((b >> 4) &  0xf)
      if k == .anything {
        assert((b >> 8) == 0, "'anything' must be the top level path component")
      } else {
        numBits = numBits &+ 8
      }
      return (k, idx, numBits)
    }

    func pop(numBits: Int) -> Path {
      return Path(bytes: bytes &>> numBits)
    }

    func pop() -> (kind: FieldKind, index: Int, path: Path) {
      let (k, idx, numBits) = top
      return (k, idx, pop(numBits: numBits))
    }

    func pop(kind: FieldKind) -> (index: Int, path: Path)? {
      let (k, idx, newPath) = pop()
      if k != kind { return nil }
      return (idx, newPath)
    }

    func push(kind: FieldKind, index: Int = 0) -> Path {
      var idx = index
      var b = bytes
      if (b >> 56) != 0 { return Path.matchingAll }
      b = (b << 8) | UInt64(((idx & 0xf) << 4) | (kind.rawValue << 1))
      idx >>= 4
      while idx != 0 {
        if (b >> 56) != 0 { return Path.matchingAll }
        b = (b << 8) | UInt64(((idx & 0x7f) << 1) | 1)
        idx >>= 7
      }
      return Path(bytes: b)
    }

    func popIfMatches(kind: FieldKind, index: Int) -> Path? {
      let (k, idx, numBits) = top
      switch k {
        case .anything:
          return self
        case .anyValueField:
          if kind.isValueField {
            return self
          }
          return pop(numBits: numBits).popIfMatches(kind: kind, index: index)
        case kind:
          if idx != index { return nil }
          return pop(numBits: numBits)
        default:
          return nil
      }
    }

    func popIfMatches(anyOfKind: FieldKind) -> Path? {
      let (k, _, numBits) = top
      switch k {
        case .anything:
          return self
        case .anyValueField:
          if anyOfKind.isValueField {
            return self
          }
          return pop(numBits: numBits).popIfMatches(anyOfKind: anyOfKind)
        case anyOfKind:
          return pop(numBits: numBits)
        default:
          return nil
      }
    }

    var matchesAnyValueField: Bool {
      top.kind == .anyValueField
    }

    func popAllValueFields() -> Path {
      var p = self
      while true {
        let (k, _, numBits) = p.top
        if !k.isValueField {
          return p
        }
        p = p.pop(numBits: numBits)
      }
    }
    
    func matches(pattern: Effect.Pattern) -> Bool {
      switch pattern {
        case .firstLevel:
          return popAllValueFields().isEmpty
        case .transitive:
          return true
      }
    }
    
    func merge(with rhs: Path) -> Path {
      if self == rhs { return self }
      
      let (lhsKind, lhsIdx, lhsBits) = top
      let (rhsKind, rhsIdx, rhsBits) = rhs.top
      if lhsKind == rhsKind && lhsIdx == rhsIdx {
        assert(lhsBits == rhsBits)
        let subPath = pop(numBits: lhsBits).merge(with: rhs.pop(numBits: rhsBits))
        if subPath.top.kind == .anyValueField && lhsKind.isValueField {
          return subPath
        }
        return subPath.push(kind: lhsKind, index: lhsIdx)
      }
      if lhsKind.isValueField && rhsKind.isValueField {
        let subPath = popAllValueFields().merge(with: rhs.popAllValueFields())
        assert(!subPath.top.kind.isValueField)
        return subPath.push(kind: .anyValueField)
      }
      return Path.matchingAll
    }

    static func unitTest() {
      func basicPushPop() {
        let p1 = Path().push(kind: .structField, index: 3).push(kind: .classField, index: 12345678)
        let t = p1.top
        assert(t.kind == .classField && t.index == 12345678)
        let (k2, i2, p2) = p1.pop()
        assert(k2 == .classField && i2 == 12345678)
        let (k3, i3, p3) = p2.pop()
        assert(k3 == .structField && i3 == 3)
        assert(p3.isEmpty)
        let (k4, i4, _) = p2.push(kind: .enumCase, index: 876).pop()
        assert(k4 == .enumCase && i4 == 876)
      }
      
      func merging() {
        let p1 = Path().push(kind: .classField).push(kind: .structField).push(kind: .structField).push(kind: .tailElements)
        let p2 = Path().push(kind: .classField).push(kind: .structField).push(kind: .enumCase).push(kind: .structField).push(kind: .tailElements)
        let p3 = p1.merge(with: p2)
        assert(p3 == Path().push(kind: .classField).push(kind: .anyValueField).push(kind: .tailElements))
      }
      
      basicPushPop()
      merging()
    }
  }

  private struct CacheEntry {
    private(set) var path = Path()
    private(set) var followStores = false
    private var valid = false

    mutating func needWalk(path: Path, followStores: Bool) -> CacheEntry? {
      if !valid {
        valid = true
        self.path = path
        self.followStores = followStores
        return self
      }
      if self.path != path || (!self.followStores && followStores) {
        let newFS = self.followStores || followStores
        let newPath = self.path.merge(with: path)
        if newPath != self.path || newFS != self.followStores {
          self.followStores = newFS
          self.path = newPath
          return self
        }
      }
      return nil
    }
  }

  private var walkedDownCache = Dictionary<HashableValue, CacheEntry>()
  private var walkedUpCache = Dictionary<HashableValue, CacheEntry>()
  
  private let calleeAnalysis: CalleeAnalysis
  
  init(calleeAnalysis: CalleeAnalysis) {
    self.calleeAnalysis = calleeAnalysis
    
    // TODO: move this to a separate test file
    Path.unitTest()
  }

  public mutating func escapes(_ allocRef: AllocRefInst) -> Escapes {
    start()
    let result = walkDownAndCache(allocRef,
                              path: Path(),
                              followStores: false)
    cleanup()
    return result
  }

  public mutating func escapes(argument: FunctionArgument,
                               pattern: Effect.Pattern) -> Escapes {
    start()
    let result = walkDownAndCache(argument,
                              path: Path(pattern: pattern),
                              followStores: false)
    cleanup()
    return result
  }

  private func start() {
    assert(walkedDownCache.isEmpty && walkedUpCache.isEmpty)
  }

  private mutating func cleanup() {
    walkedDownCache.removeAll(keepingCapacity: true)
    walkedUpCache.removeAll(keepingCapacity: true)
  }

  private mutating func walkDownAndCache(_ value: Value,
                                         path: Path,
                                         followStores: Bool) -> Escapes {
    if let entry = walkedDownCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkDown(value, path: entry.path, followStores: entry.followStores)
    }
    return .noEscape
  }

  private mutating func walkUpAndCache(_ value: Value,
                                       path: Path,
                                       followStores: Bool) -> Escapes {
    if let entry = walkedUpCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkUp(value, path: entry.path, followStores: entry.followStores)
    }
    return .noEscape
  }

  private mutating func walkDown(_ value: Value,
                                 path: Path,
                                 followStores: Bool) -> Escapes {
    var esc = Escapes.noEscape
    for use in value.uses {
      let user = use.instruction
      switch user {
        case let rta as RefTailAddrInst:
          if let newPath = path.popIfMatches(anyOfKind: .tailElements) {
            esc |= walkDown(rta, path: newPath, followStores: followStores)
          }
        case let rea as RefElementAddrInst:
          if let newPath = path.popIfMatches(kind: .classField, index: rea.fieldIndex) {
            esc |= walkDown(rea, path: newPath, followStores: followStores)
          }
        case let str as StructInst:
          esc |= walkDown(str, path: path.push(kind: .structField, index: use.index),
                             followStores: followStores)
        case let se as StructExtractInst:
          if let newPath = path.popIfMatches(kind: .structField, index: se.fieldIndex) {
            esc |= walkDown(se, path: newPath, followStores: followStores)
          }
        case let ds as DestructureStructInst:
          esc |= walkDownInstructionResults(results: ds.results,
                fieldKind: .structField, path: path, followStores: followStores)
        case let dt as DestructureTupleInst:
          esc |= walkDownInstructionResults(results: dt.results,
                fieldKind: .tupleField, path: path, followStores: followStores)
        case let sea as StructElementAddrInst:
          if let newPath = path.popIfMatches(kind: .structField, index: sea.fieldIndex) {
            esc |= walkDown(sea, path: newPath, followStores: followStores)
          }
        case let t as TupleInst:
          esc |= walkDown(t, path: path.push(kind: .tupleField, index: use.index),
                             followStores: followStores)
        case let te as TupleExtractInst:
          if let newPath = path.popIfMatches(kind: .tupleField, index: te.fieldIndex) {
            esc |= walkDown(te, path: newPath, followStores: followStores)
          }
        case let tea as TupleElementAddrInst:
          if let newPath = path.popIfMatches(kind: .tupleField, index: tea.fieldIndex) {
            esc |= walkDown(tea, path: newPath, followStores: followStores)
          }
        case let e as EnumInst:
          esc |= walkDown(e, path: path.push(kind: .enumCase, index: e.caseIndex),
                             followStores: followStores)
        case let ued as UncheckedEnumDataInst:
          if let newPath = path.popIfMatches(kind: .enumCase, index: ued.caseIndex) {
            esc |= walkDown(ued, path: newPath, followStores: followStores)
          }
        case let se as SwitchEnumInst:
          if let (caseIdx, newPath) = path.pop(kind: .enumCase) {
            if let succBlock = se.getUniqueSuccessor(forCaseIndex: caseIdx) {
              if let payload = succBlock.arguments.first {
                esc |= walkDown(payload, path: newPath,
                                   followStores: followStores)
              }
            }
          } else if path.matchesAnyValueField {
            for succBlock in se.block.successors {
              if let payload = succBlock.arguments.first {
                esc |= walkDown(payload, path: path,
                                   followStores: followStores)
              }
            }
          } else {
            return Escapes.toGlobal
          }
        case let store as StoreInst:
          if use == store.sourceOperand {
            esc |= walkUp(store.destination, path: path,
                          followStores: followStores)
          } else if followStores {
            assert(use == store.destinationOperand)
            esc |= walkUp(store.source, path: path, followStores: followStores)
          }
        case let copyAddr as CopyAddrInst:
          if use == copyAddr.sourceOperand {
            esc |= walkUp(copyAddr.destination, path: path,
                          followStores: followStores)
          } else if followStores {
            assert(use == copyAddr.destinationOperand)
            esc |= walkUp(copyAddr.source, path: path,
                          followStores: followStores)
          }
        case is DestroyValueInst, is ReleaseValueInst, is StrongReleaseInst,
             is DestroyAddrInst:
          // Destroying cannot escape the value/reference itself, but its
          // contents (in the destructor).
          let p = path.popAllValueFields()
          if let _ = p.popIfMatches(anyOfKind: .classField) {
            return Escapes.toGlobal
          }
        case let br as BranchInst:
          esc |= walkDownAndCache(br.getArgument(for: use), path: path,
                                  followStores: followStores)
        case is ReturnInst:
          esc |= Escapes.toReturn
        case let ap as ApplyInst:
          esc |= handleArgumentEffects(argOp: use, apply: ap, path: path,
                                       followStores: followStores)
        case let tap as TryApplyInst:
          esc |= handleArgumentEffects(argOp: use, apply: tap, path: path,
                                       followStores: followStores)
        case is LoadInst, is InitExistentialRefInst, is OpenExistentialRefInst,
             is BeginAccessInst, is BeginBorrowInst, is CopyValueInst,
             is UpcastInst, is UncheckedRefCastInst,
             is PointerToAddressInst, is IndexAddrInst:
          esc |= walkDown(user as! SingleValueInstruction, path: path,
                          followStores: followStores)
        case is DeallocStackInst, is StrongRetainInst, is RetainValueInst,
             is DebugValueInst, is ValueMetatypeInst,
             is InitExistentialMetatypeInst, is OpenExistentialMetatypeInst,
             is ExistentialMetatypeInst, is DeallocRefInst, is SetDeallocatingInst,
             is FixLifetimeInst, is ClassifyBridgeObjectInst,
             is EndBorrowInst, is EndAccessInst,
             is StrongRetainInst, is RetainValueInst:
          break
        default:
          return Escapes.toGlobal
      }
      if case .toGlobal = esc {
        return .toGlobal
      }
    }
    return esc
  }
  
  private mutating func walkDownInstructionResults(
                        results: Instruction.Results, fieldKind: Path.FieldKind,
                        path: Path, followStores: Bool) -> Escapes {
    if let (index, newPath) = path.pop(kind: fieldKind) {
      return walkDown(results[index], path: newPath, followStores: followStores)
    } else if path.matchesAnyValueField {
      var esc = Escapes.noEscape
      for elem in results {
        esc |= walkDown(elem, path: path, followStores: followStores)
      }
      return esc
    }
    return Escapes.toGlobal
  }

  private mutating func handleArgumentEffects(argOp: Operand, apply: FullApplySite,
                                     path: Path,
                                     followStores: Bool) -> Escapes {
    guard let argIdx = apply.argumentIndex(of: argOp) else {
      // The callee or a type dependend operand does not let escape anything.
      return .noEscape
    }
    
    if followStores {
      // We don't know if something is stored to an argument.
      return Escapes.toGlobal
    }
  
    let callees = calleeAnalysis.getCallees(callee: apply.callee)
    if !callees.allCalleesKnown {
      return Escapes.toGlobal
    }

    var esc = Escapes.noEscape

    for callee in callees {
      var escapeEffectFound = false
      for effect in callee.effects {
        if case .escaping(let argInfo, let escapes) = effect.kind {
          if argInfo.argIndex == argIdx &&
             path.matches(pattern: argInfo.pattern) {
            escapeEffectFound = true
        
            switch escapes {
              case .noEscape:
                break
              case .toReturn:
                guard let result = apply.singleDirectResult else {
                  return Escapes.toGlobal
                }
                esc |= walkDown(result, path: Path().push(kind: .anyValueField),
                                followStores: false)
              case .toArgument(let destArgIdx):
                esc |= walkUp(apply.arguments[destArgIdx],
                              path: Path().push(kind: .anyValueField),
                              followStores: false)
              case .toGlobal:
                return .toGlobal
            }
            if case .toGlobal = esc {
              return .toGlobal
            }
          } else if case .toArgument(let destArgIdx) = escapes {
            if destArgIdx == argIdx {
              escapeEffectFound = true
            }
          }
        }
      }
      if !escapeEffectFound {
        return .toGlobal
      }
    }
    return esc
  }

  private mutating func walkUp(_ value: Value,
                               path: Path,
                               followStores: Bool) -> Escapes {
    var val = value
    var p = path
    var fSt = followStores
    while true {
      switch val {
        case is AllocRefInst, is AllocStackInst:
          return walkDownAndCache(val, path: p, followStores: fSt)
        case let arg as FunctionArgument:
          let esc = walkDownAndCache(arg, path: p, followStores: fSt)
          let argIdx = arg.index
          return esc | Escapes.toArgument(argIdx)
        case let arg as BlockArgument:
          if arg.isPhiArgument {
            var esc = Escapes.noEscape
            for incoming in arg.incomingPhiValues {
              esc |= walkUpAndCache(incoming, path: p, followStores: fSt)
            }
            return esc
          }
          let block = arg.block
          switch block.singlePredecessor!.terminator {
            case let se as SwitchEnumInst:
              guard let caseIdx = se.getUniqueCase(forSuccessor: block) else {
                return Escapes.toGlobal
              }
              val = se.enumOp
              p = p.push(kind: .enumCase, index: caseIdx)
            default:
              return Escapes.toGlobal
          }
        case let str as StructInst:
          guard let (structField, poppedPath) = p.pop(kind: .structField) else {
            // TODO: handle anyValueField by iterating over all operands
            return Escapes.toGlobal
          }
          val = str.operands[structField].value
          p = poppedPath
        case let se as StructExtractInst:
          val = se.operand
          p = p.push(kind: .structField, index: se.fieldIndex)
        case let t as TupleInst:
          guard let (tupleField, poppedPath) = p.pop(kind: .tupleField) else {
            // TODO: handle anyValueField by iterating over all operands
            return Escapes.toGlobal
          }
          val = t.operands[tupleField].value
          p = poppedPath
        case let te as TupleExtractInst:
          val = te.operand
          p = p.push(kind: .tupleField, index: te.fieldIndex)
        case let load as LoadInst:
          val = load.operand
          // When the value is loaded from somewhere it also matters what's
          // stored to that memory location.
          fSt = true
        case let rta as RefTailAddrInst:
          val = rta.operand
          p = p.push(kind: .tailElements)
        case let rea as RefElementAddrInst:
          val = rea.operand
          p = p.push(kind: .classField, index: rea.fieldIndex)
        case let sea as StructElementAddrInst:
          p = p.push(kind: .structField, index: sea.fieldIndex)
          val = sea.operand
        case let tea as TupleElementAddrInst:
          p = p.push(kind: .tupleField, index: tea.fieldIndex)
          val = tea.operand
        case let e as EnumInst:
          guard let newPath = p.popIfMatches(kind: .enumCase, index: e.caseIndex) else {
            return .noEscape
          }
          p = newPath
          val = e.operand
        case let ued as UncheckedEnumDataInst:
          val = ued.operand
          p = p.push(kind: .enumCase, index: ued.caseIndex)
        case is UpcastInst, is UncheckedRefCastInst, is InitExistentialRefInst,
             is OpenExistentialRefInst, is BeginAccessInst, is BeginBorrowInst:
          val = (val as! Instruction).operands[0].value
        case let mvr as MultipleValueInstructionResult:
          let inst = mvr.instruction
          switch inst {
            case is DestructureStructInst, is DestructureTupleInst:
              val = inst.operands[0].value
              // TODO: push the specific result index.
              // But currently there is no O(0) method to get the result index
              // from a result value.
              p = p.popAllValueFields().push(kind: .anyValueField)
            default:
              return Escapes.toGlobal
          }
        default:
          return Escapes.toGlobal
      }
    }
  }
}
