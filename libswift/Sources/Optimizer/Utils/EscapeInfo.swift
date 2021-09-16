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
      case root          = 0x0
      case structField   = 0x1
      case tupleField    = 0x2
      case enumCase      = 0x3
      case classField    = 0x4
      case tailElements  = 0x5
      case anyValueField = 0x6
      case anyClassField = 0x7
      
      // Starting from here the low 3 bits must be 1.
      // The path entries cannot have an index.
      case anything      = 0xf
  
      var isValueField: Bool {
        switch self {
          case .anyValueField, .structField, .tupleField, .enumCase:
            return true
          case .root, .anything, .anyClassField, .classField, .tailElements:
            return false
        }
      }
      
      var isClassField: Bool {
        switch self {
          case .anyClassField, .classField, .tailElements:
            return true
          case .root, .anything, .anyValueField, .structField, .tupleField, .enumCase:
            return false
        }
      }
    }

    init() { self.bytes = 0 }
    
    init(pattern: Effect.Pattern) {
      self.init()
      switch pattern {
        case .noIndirection:
          self = push(.anyValueField)
        case .oneIndirection:
          self = push(.anyValueField).push(.anyClassField).push(.anyValueField)
        case .anything:
          self = push(.anything)
      }
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
          case .structField:   s = "s\(idx)"
          case .tupleField:    s = "t\(idx)"
          case .enumCase:      s = "e\(idx)"
          case .classField:    s = "c\(idx)"
          case .tailElements:  s = "ct"
          case .anyValueField: s = "v*"
          case .anyClassField: s = "c*"
          case .anything:      s = "**"
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
      var kindVal = (b >> 1) & 0x7
      if kindVal == 0x7 {
        kindVal = (b >> 1) & 0x7f
        assert(idx == 0)
      } else {
        idx = (idx << 4) | Int((b >> 4) &  0xf)
      }
      let k = FieldKind(rawValue: Int(kindVal))!
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

    func push(_ kind: FieldKind, index: Int = 0) -> Path {
      var idx = index
      var b = bytes
      if (b >> 56) != 0 { return Path().push(.anything) }
      b = (b << 8) | UInt64(((idx & 0xf) << 4) | (kind.rawValue << 1))
      idx >>= 4
      while idx != 0 {
        if (b >> 56) != 0 { return Path().push(.anything) }
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
          if kind.isValueField { return self }
          return pop(numBits: numBits).popIfMatches(kind: kind, index: index)
        case .anyClassField:
          if kind.isClassField { return self }
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
          if anyOfKind.isValueField { return self }
          return pop(numBits: numBits).popIfMatches(anyOfKind: anyOfKind)
        case .anyClassField:
          if anyOfKind.isClassField { return self }
          return pop(numBits: numBits).popIfMatches(anyOfKind: anyOfKind)
        case anyOfKind:
          return pop(numBits: numBits)
        default:
          return nil
      }
    }

    var matchesAnyValueField: Bool {
      switch top.kind {
        case .anyValueField, .anything: return true
        default: return false
      }
    }

    func popAllValueFields() -> Path {
      var p = self
      while true {
        let (k, _, numBits) = p.top
        if !k.isValueField { return p }
        p = p.pop(numBits: numBits)
      }
    }
    
    func popAllClassFields() -> Path {
      var p = self
      while true {
        let (k, _, numBits) = p.top
        if !k.isClassField { return p }
        p = p.pop(numBits: numBits)
      }
    }
    
    func matches(pattern: Effect.Pattern) -> Bool {
      switch pattern {
        case .noIndirection:
          return popAllValueFields().isEmpty
        case .oneIndirection:
          let p = popAllValueFields()
          if p.isEmpty { return false }
          return p.popAllClassFields().popAllValueFields().isEmpty
        case .anything:
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
        return subPath.push(lhsKind, index: lhsIdx)
      }
      if lhsKind.isValueField && rhsKind.isValueField {
        let subPath = popAllValueFields().merge(with: rhs.popAllValueFields())
        assert(!subPath.top.kind.isValueField)
        return subPath.push(.anyValueField)
      }
      return Path().push(.anything)
    }

    static func unitTest() {
      func basicPushPop() {
        let p1 = Path().push(.structField, index: 3).push(.classField, index: 12345678)
        let t = p1.top
        assert(t.kind == .classField && t.index == 12345678)
        let (k2, i2, p2) = p1.pop()
        assert(k2 == .classField && i2 == 12345678)
        let (k3, i3, p3) = p2.pop()
        assert(k3 == .structField && i3 == 3)
        assert(p3.isEmpty)
        let (k4, i4, _) = p2.push(.enumCase, index: 876).pop()
        assert(k4 == .enumCase && i4 == 876)
      }
      
      func merging() {
        let p1 = Path().push(.classField).push(.structField).push(.structField).push(.tailElements)
        let p2 = Path().push(.classField).push(.structField).push(.enumCase).push(.structField).push(.tailElements)
        let p3 = p1.merge(with: p2)
        assert(p3 == Path().push(.classField).push(.anyValueField).push(.tailElements))
        
        let p4 = Path().push(.classField, index: 0).push(.classField, index: 1)
        let p5 = Path().push(.classField, index: 0)
        let p6 = p5.merge(with: p4)
        assert(p6 == Path().push(.anything))
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
          esc |= walkDown(str, path: path.push(.structField, index: use.index),
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
          esc |= walkDown(t, path: path.push(.tupleField, index: use.index),
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
          esc |= walkDown(e, path: path.push(.enumCase, index: e.caseIndex),
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
            return .toGlobal
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
            return .toGlobal
          }
        case let br as BranchInst:
          esc |= walkDownAndCache(br.getArgument(for: use), path: path,
                                  followStores: followStores)
        case is ReturnInst:
          if path.popAllValueFields().isEmpty {
            esc |= .toReturn
          } else {
            esc |= .toGlobal
          }
        case let ap as ApplyInst:
          esc |= handleArgumentEffects(argOp: use, apply: ap, path: path,
                                       followStores: followStores)
        case let tap as TryApplyInst:
          esc |= handleArgumentEffects(argOp: use, apply: tap, path: path,
                                       followStores: followStores)
        case is LoadInst, is InitExistentialRefInst, is OpenExistentialRefInst,
             is BeginAccessInst, is BeginBorrowInst, is CopyValueInst,
             is UpcastInst, is UncheckedRefCastInst, is EndCOWMutationInst,
             is PointerToAddressInst, is IndexAddrInst, is BridgeObjectToRefInst:
          esc |= walkDown(user as! SingleValueInstruction, path: path,
                          followStores: followStores)
        case let bcm as BeginCOWMutationInst:
          esc |= walkDown(bcm.bufferResult, path: path,
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
          return .toGlobal
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
    return .toGlobal
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
      return .toGlobal
    }
  
    let callees = calleeAnalysis.getCallees(callee: apply.callee)
    if !callees.allCalleesKnown {
      return .toGlobal
    }

    var esc = Escapes.noEscape

    for callee in callees {
      esc |= handleArgument(argIdx: argIdx, argPath: path, apply: apply,
                            callee: callee)
      if case .toGlobal = esc {
        return .toGlobal
      }
    }
    return esc
  }
  
  private mutating func handleArgument(argIdx: Int, argPath: Path,
                            apply: FullApplySite, callee: Function) -> Escapes {
    for effect in callee.effects {
      guard case .escaping(let argInfo, let escapes) = effect.kind else {
        continue
      }
      if argInfo.argIndex == argIdx && argPath.matches(pattern: argInfo.pattern) {
        switch escapes {
          case .noEscape, .toGlobal:
            return escapes
          case .toReturn:
            if let result = apply.singleDirectResult {
              return walkDown(result, path: Path().push(.anyValueField),
                              followStores: false)
            }
            return .toGlobal
          case .toArgument(let destArgIdx):
            return walkUp(apply.arguments[destArgIdx],
                          path: Path().push(.anyValueField),
                          followStores: false)
        }
      } else if case .toArgument(let destArgIdx) = escapes {
        if destArgIdx == argIdx && argPath.popAllValueFields().isEmpty {
          return .noEscape
        }
      }
    }
    return .toGlobal
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
          if !p.popAllValueFields().isEmpty {
            return .toGlobal
          }
          let esc = walkDownAndCache(arg, path: p, followStores: fSt)
          return esc | Escapes.toArgument(arg.index)
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
                return .toGlobal
              }
              val = se.enumOp
              p = p.push(.enumCase, index: caseIdx)
            default:
              return .toGlobal
          }
        case let str as StructInst:
          guard let (structField, poppedPath) = p.pop(kind: .structField) else {
            // TODO: handle anyValueField by iterating over all operands
            return .toGlobal
          }
          val = str.operands[structField].value
          p = poppedPath
        case let se as StructExtractInst:
          val = se.operand
          p = p.push(.structField, index: se.fieldIndex)
        case let t as TupleInst:
          guard let (tupleField, poppedPath) = p.pop(kind: .tupleField) else {
            // TODO: handle anyValueField by iterating over all operands
            return .toGlobal
          }
          val = t.operands[tupleField].value
          p = poppedPath
        case let te as TupleExtractInst:
          val = te.operand
          p = p.push(.tupleField, index: te.fieldIndex)
        case let load as LoadInst:
          val = load.operand
          // When the value is loaded from somewhere it also matters what's
          // stored to that memory location.
          fSt = true
        case let rta as RefTailAddrInst:
          val = rta.operand
          p = p.push(.tailElements)
        case let rea as RefElementAddrInst:
          val = rea.operand
          p = p.push(.classField, index: rea.fieldIndex)
        case let sea as StructElementAddrInst:
          p = p.push(.structField, index: sea.fieldIndex)
          val = sea.operand
        case let tea as TupleElementAddrInst:
          p = p.push(.tupleField, index: tea.fieldIndex)
          val = tea.operand
        case let e as EnumInst:
          guard let newPath = p.popIfMatches(kind: .enumCase, index: e.caseIndex) else {
            return .noEscape
          }
          p = newPath
          val = e.operand
        case let ued as UncheckedEnumDataInst:
          val = ued.operand
          p = p.push(.enumCase, index: ued.caseIndex)
        case is UpcastInst, is UncheckedRefCastInst, is InitExistentialRefInst,
             is OpenExistentialRefInst, is BeginAccessInst, is BeginBorrowInst,
             is EndCOWMutationInst, is BridgeObjectToRefInst:
          val = (val as! Instruction).operands[0].value
        case let mvr as MultipleValueInstructionResult:
          let inst = mvr.instruction
          switch inst {
            case is DestructureStructInst, is DestructureTupleInst:
              val = inst.operands[0].value
              // TODO: only push the specific result index.
              // But currently there is no O(0) method to get the result index
              // from a result value.
              p = p.popAllValueFields().push(.anyValueField)
            case let bcm as BeginCOWMutationInst:
              val = bcm.operand
            default:
              return .toGlobal
          }
        default:
          return .toGlobal
      }
    }
  }
}
