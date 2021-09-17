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

struct EscapeInfo {

  struct Path : CustomStringConvertible, Hashable {
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

    init(_ kind: FieldKind, index: Int = 0) {
      self = Path().push(kind, index: index)
    }

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
        descr = (descr.isEmpty ? s : "\(descr).\(s)")
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
      if (b >> 56) != 0 { return Path(.anything) }
      b = (b << 8) | UInt64(((idx & 0xf) << 4) | (kind.rawValue << 1))
      idx >>= 4
      while idx != 0 {
        if (b >> 56) != 0 { return Path(.anything) }
        b = (b << 8) | UInt64(((idx & 0x7f) << 1) | 1)
        idx >>= 7
      }
      return Path(bytes: b)
    }

    func popIfMatches(_ kind: FieldKind, index: Int? = nil) -> Path? {
      let (k, idx, numBits) = top
      switch k {
        case .anything:
          return self
        case .anyValueField:
          if kind.isValueField { return self }
          return pop(numBits: numBits).popIfMatches(kind, index: index)
        case .anyClassField:
          if kind.isClassField {
            return pop(numBits: numBits)
          }
          return nil
        case kind:
          if let i = index {
            if i != idx { return nil }
          }
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
        if lhsKind == .anyValueField && subPath.top.kind == .anyValueField {
          return subPath
        }
        return subPath.push(lhsKind, index: lhsIdx)
      }
      if lhsKind.isValueField || rhsKind.isValueField {
        let subPath = popAllValueFields().merge(with: rhs.popAllValueFields())
        assert(!subPath.top.kind.isValueField)
        if subPath.top.kind == .anything {
          return subPath
        }
        return subPath.push(.anyValueField)
      }
      if lhsKind.isClassField && rhsKind.isClassField {
        let subPath = pop(numBits: lhsBits).merge(with: rhs.pop(numBits: rhsBits))
        return subPath.push(.anyClassField)
      }
      return Path(.anything)
    }

    static func unitTest() {
      func basicPushPop() {
        let p1 = Path(.structField, index: 3).push(.classField, index: 12345678)
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
      
      func testMerge(_ lhs: Path, _ rhs: Path, expect: Path) {
        let result = lhs.merge(with: rhs)
        assert(result == expect)
         let result2 = rhs.merge(with: lhs)
        assert(result2 == expect)
      }
     
      func merging() {
        testMerge(
          Path(.classField).push(.structField).push(.structField).push(.tailElements),
          Path(.classField).push(.structField).push(.enumCase).push(.structField).push(.tailElements),
          expect: Path(.classField).push(.anyValueField).push(.structField).push(.tailElements))
        testMerge(
          Path(.classField, index: 0).push(.classField, index: 1),
          Path(.classField, index: 0),
          expect: Path(.anything).push(.anyClassField))
        testMerge(
          Path(.classField, index: 1).push(.classField, index: 0),
          Path(.classField, index: 0),
          expect: Path(.anything).push(.classField, index: 0))
        testMerge(
          Path(.classField),
          Path(.classField).push(.structField),
          expect: Path(.classField).push(.anyValueField))
        testMerge(
          Path(.classField, index: 0),
          Path(.classField, index: 1).push(.structField),
          expect: Path(.anyClassField).push(.anyValueField))
        testMerge(
          Path(.classField).push(.structField).push(.structField),
          Path(.classField).push(.structField),
          expect: Path(.classField).push(.anyValueField).push(.structField))
        testMerge(
          Path(.structField).push(.structField, index: 1),
          Path(.structField).push(.structField, index: 2),
          expect: Path(.anyValueField))
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

  mutating
  func escapes(_ allocRef: AllocRefInst,
               visitUse: (Operand, Path) -> Bool = { _, _ in true },
               visitArg: (FunctionArgument, Path) -> Bool = { _, _ in true }) -> Bool {
    start()
    let result = walkDownAndCache(allocRef,
                              path: Path(), followStores: false,
                              visitUse: visitUse, visitArg: visitArg)
    cleanup()
    return result
  }

   mutating
   func escapes(argument: FunctionArgument,
                pattern: Effect.Pattern,
                visitUse: (Operand, Path) -> Bool = { _, _ in true },
                visitArg: (FunctionArgument, Path) -> Bool = { _, _ in true }) -> Bool {
    start()
    let result = walkDownAndCache(argument,
                              path: Path(pattern: pattern), followStores: false,
                              visitUse: visitUse, visitArg: visitArg)
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

  private mutating
  func walkDownAndCache(_ value: Value,
                        path: Path, followStores: Bool,
                        visitUse: (Operand, Path) -> Bool,
                        visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    if let entry = walkedDownCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkDown(value, path: entry.path, followStores: entry.followStores,
                      visitUse: visitUse, visitArg: visitArg)
    }
    return false
  }

  private mutating
  func walkUpAndCache(_ value: Value,
                      path: Path, followStores: Bool,
                      visitUse: (Operand, Path) -> Bool,
                      visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    if let entry = walkedUpCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkUp(value, path: entry.path, followStores: entry.followStores,
                    visitUse: visitUse, visitArg: visitArg)
    }
    return false
  }

  private mutating func walkDown(_ value: Value,
                                 path: Path, followStores: Bool,
                                 visitUse: (Operand, Path) -> Bool,
                                 visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    for use in value.uses {
      if !visitUse(use, path) { continue }
      let user = use.instruction
      switch user {
        case let rta as RefTailAddrInst:
          if let newPath = path.popIfMatches(.tailElements) {
            if walkDown(rta, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let rea as RefElementAddrInst:
          if let newPath = path.popIfMatches(.classField, index: rea.fieldIndex) {
            if walkDown(rea, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let str as StructInst:
          if walkDown(str, path: path.push(.structField, index: use.index),
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let se as StructExtractInst:
          if let newPath = path.popIfMatches(.structField, index: se.fieldIndex) {
            if walkDown(se, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let ds as DestructureStructInst:
          if walkDownInstructionResults(results: ds.results,
                fieldKind: .structField, path: path, followStores: followStores,
                visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let dt as DestructureTupleInst:
          if walkDownInstructionResults(results: dt.results,
                fieldKind: .tupleField, path: path, followStores: followStores,
                visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let sea as StructElementAddrInst:
          if let newPath = path.popIfMatches(.structField, index: sea.fieldIndex) {
            if walkDown(sea, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let t as TupleInst:
          if walkDown(t, path: path.push(.tupleField, index: use.index),
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let te as TupleExtractInst:
          if let newPath = path.popIfMatches(.tupleField, index: te.fieldIndex) {
            if walkDown(te, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let tea as TupleElementAddrInst:
          if let newPath = path.popIfMatches(.tupleField, index: tea.fieldIndex) {
            if walkDown(tea, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let e as EnumInst:
          if walkDown(e, path: path.push(.enumCase, index: e.caseIndex),
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let ued as UncheckedEnumDataInst:
          if let newPath = path.popIfMatches(.enumCase, index: ued.caseIndex) {
            if walkDown(ued, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let se as SwitchEnumInst:
          if let (caseIdx, newPath) = path.pop(kind: .enumCase) {
            if let succBlock = se.getUniqueSuccessor(forCaseIndex: caseIdx) {
              if let payload = succBlock.arguments.first {
                if walkDown(payload, path: newPath, followStores: followStores,
                            visitUse: visitUse, visitArg: visitArg) {
                  return true
                }
              }
            }
          } else if path.matchesAnyValueField {
            for succBlock in se.block.successors {
              if let payload = succBlock.arguments.first {
                if walkDown(payload, path: path, followStores: followStores,
                            visitUse: visitUse, visitArg: visitArg) {
                  return true
                }
              }
            }
          } else {
            return isEscaping
          }
        case let store as StoreInst:
          if use == store.sourceOperand {
            if walkUp(store.destination, path: path,
                          followStores: followStores,
                          visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          } else if followStores {
            assert(use == store.destinationOperand)
            if walkUp(store.source, path: path, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let copyAddr as CopyAddrInst:
          if use == copyAddr.sourceOperand {
            if walkUp(copyAddr.destination, path: path, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          } else if followStores {
            assert(use == copyAddr.destinationOperand)
            if walkUp(copyAddr.source, path: path, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case is DestroyValueInst, is ReleaseValueInst, is StrongReleaseInst,
             is DestroyAddrInst:
          // Destroying cannot escape the value/reference itself, but its
          // contents (in the destructor).
          let p = path.popAllValueFields()
          if let _ = p.popIfMatches(.classField) {
            return isEscaping
          }
        case let br as BranchInst:
          if walkDownAndCache(br.getArgument(for: use), path: path,
                              followStores: followStores,
                              visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case is ReturnInst:
          return isEscaping
        case let ap as ApplyInst:
          if handleArgumentEffects(argOp: use, apply: ap, path: path,
                                   followStores: followStores,
                                   visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let tap as TryApplyInst:
          if handleArgumentEffects(argOp: use, apply: tap, path: path,
                                   followStores: followStores,
                                   visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case is LoadInst,
             is InitExistentialRefInst, is OpenExistentialRefInst,
             is InitExistentialAddrInst, is OpenExistentialAddrInst,
             is BeginAccessInst, is BeginBorrowInst, is CopyValueInst,
             is UpcastInst, is UncheckedRefCastInst, is EndCOWMutationInst,
             is PointerToAddressInst, is IndexAddrInst, is BridgeObjectToRefInst:
          if walkDown(user as! SingleValueInstruction, path: path,
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let bcm as BeginCOWMutationInst:
          if walkDown(bcm.bufferResult, path: path, followStores: followStores,
                          visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case is DeallocStackInst, is StrongRetainInst, is RetainValueInst,
             is DebugValueInst, is ValueMetatypeInst,
             is InitExistentialMetatypeInst, is OpenExistentialMetatypeInst,
             is ExistentialMetatypeInst, is DeallocRefInst, is SetDeallocatingInst,
             is FixLifetimeInst, is ClassifyBridgeObjectInst,
             is EndBorrowInst, is EndAccessInst,
             is StrongRetainInst, is RetainValueInst:
          break
        default:
          return isEscaping
      }
    }
    return false
  }
  
  private mutating
  func walkDownInstructionResults(results: Instruction.Results,
                                  fieldKind: Path.FieldKind,
                                  path: Path, followStores: Bool,
                                  visitUse: (Operand, Path) -> Bool,
                                  visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    if let (index, newPath) = path.pop(kind: fieldKind) {
      return walkDown(results[index], path: newPath, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg)
    }
    if path.matchesAnyValueField {
      for elem in results {
        if walkDown(elem, path: path, followStores: followStores,
                    visitUse: visitUse, visitArg: visitArg) {
          return true
        }
      }
      return false
    }
    return isEscaping
  }

  private mutating
  func handleArgumentEffects(argOp: Operand, apply: FullApplySite,
                             path: Path, followStores: Bool,
                             visitUse: (Operand, Path) -> Bool,
                             visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    guard let argIdx = apply.argumentIndex(of: argOp) else {
      // The callee or a type dependend operand does not let escape anything.
      return false
    }
    
    if followStores {
      // We don't know if something is stored to an argument.
      return isEscaping
    }
  
    let callees = calleeAnalysis.getCallees(callee: apply.callee)
    if !callees.allCalleesKnown {
      return isEscaping
    }

    for callee in callees {
      if handleArgument(argIdx: argIdx, argPath: path, apply: apply,
                        callee: callee, visitUse: visitUse, visitArg: visitArg) {
        return true
      }
    }
    return false
  }
  
  private mutating
  func handleArgument(argIdx: Int, argPath: Path,
                      apply: FullApplySite, callee: Function,
                      visitUse: (Operand, Path) -> Bool,
                      visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    for effect in callee.effects {
      guard case .escaping(let argInfo, let escapes) = effect.kind else {
        continue
      }
      if argInfo.argIndex == argIdx && argPath.matches(pattern: argInfo.pattern) {
        switch escapes {
          case .noEscape:
            return false
          case .toReturn:
            if let result = apply.singleDirectResult {
              return walkDown(result, path: Path().push(.anyValueField),
                              followStores: false,
                              visitUse: visitUse, visitArg: visitArg)
            }
            return isEscaping
          case .toArgument(let destArgIdx):
            return walkUp(apply.arguments[destArgIdx],
                          path: Path().push(.anyValueField),
                          followStores: false,
                          visitUse: visitUse, visitArg: visitArg)
        }
      } else if case .toArgument(let destArgIdx) = escapes {
        if destArgIdx == argIdx && argPath.popAllValueFields().isEmpty {
          return false
        }
      }
    }
    return isEscaping
  }

  private mutating func walkUp(_ value: Value,
                               path: Path, followStores: Bool,
                               visitUse: (Operand, Path) -> Bool,
                               visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    var val = value
    var p = path
    var fSt = followStores
    while true {
      switch val {
        case is AllocRefInst, is AllocStackInst:
          return walkDownAndCache(val, path: p, followStores: fSt,
                                  visitUse: visitUse, visitArg: visitArg)
        case let arg as FunctionArgument:
          if visitArg(arg, p) {
            return isEscaping
          }
          return walkDownAndCache(arg, path: p, followStores: fSt,
                                  visitUse: visitUse, visitArg: visitArg)
        case let arg as BlockArgument:
          if arg.isPhiArgument {
            for incoming in arg.incomingPhiValues {
              if walkUpAndCache(incoming, path: p, followStores: fSt,
                                 visitUse: visitUse, visitArg: visitArg) {
                return true
              }
            }
            return false
          }
          let block = arg.block
          switch block.singlePredecessor!.terminator {
            case let se as SwitchEnumInst:
              guard let caseIdx = se.getUniqueCase(forSuccessor: block) else {
                return isEscaping
              }
              val = se.enumOp
              p = p.push(.enumCase, index: caseIdx)
            default:
              return isEscaping
          }
        case let str as StructInst:
          guard let (structField, poppedPath) = p.pop(kind: .structField) else {
            // TODO: handle anyValueField by iterating over all operands
            return isEscaping
          }
          val = str.operands[structField].value
          p = poppedPath
        case let se as StructExtractInst:
          val = se.operand
          p = p.push(.structField, index: se.fieldIndex)
        case let t as TupleInst:
          guard let (tupleField, poppedPath) = p.pop(kind: .tupleField) else {
            // TODO: handle anyValueField by iterating over all operands
            return isEscaping
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
          guard let newPath = p.popIfMatches(.enumCase, index: e.caseIndex) else {
            return false
          }
          p = newPath
          val = e.operand
        case let ued as UncheckedEnumDataInst:
          val = ued.operand
          p = p.push(.enumCase, index: ued.caseIndex)
        case is UpcastInst, is UncheckedRefCastInst,
             is InitExistentialRefInst, is OpenExistentialRefInst,
             is InitExistentialAddrInst, is OpenExistentialAddrInst,
             is BeginAccessInst, is BeginBorrowInst,
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
              return isEscaping
          }
        default:
          return isEscaping
      }
    }
  }
  
  // For convenient setting a breakpoing to whatever lets something escape.
  private var isEscaping: Bool {
    true
  }
}
