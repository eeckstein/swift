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

public struct Escapes : OptionSet, CustomStringConvertible {
  public var rawValue: Int

  public init(rawValue: Int = 0) { self.rawValue = rawValue }

  static let invalid = Escapes(rawValue: -1)
  static let toGlobal = Escapes(rawValue: 1 << 0)
  static let toReturn = Escapes(rawValue: 1 << 1)
  static func toArg(_ index: Int) -> Escapes {
    return Escapes(rawValue: 1 << (index + 2))
  }
  static let maxArgs = Int.bitWidth - 2
  
  public var description: String {
    var str = "{"
    var separator = ""
    if contains(Escapes.toGlobal) {
      str += "global"
      separator = ","
    }
    if contains(Escapes.toReturn) {
      str += separator + "return"
      separator = ","
    }
    for i in 0..<Escapes.maxArgs {
      if contains(Escapes.toArg(i)) {
        str += separator + "arg\(i)"
        separator = ","
      }
    }
    str += "}"
    return str
  }
  
  public static func |(lhs: Escapes, rhs: Escapes) -> Escapes {
    return Escapes(rawValue: lhs.rawValue | rhs.rawValue)
  }
  public static func |=(lhs: inout Escapes, rhs: Escapes) {
    lhs.rawValue |= rhs.rawValue
  }
  
  var isValid: Bool { rawValue != -1 }
}

struct EscapeInfo {

  private struct Path : CustomStringConvertible, Hashable {
    private let bytes: UInt64

    enum FieldKind : Int {
      case root = 0
      case anything = 1
      case structField = 2
      case tupleField = 3
      case enumCase = 4
      case classField = 5
      case tailElements = 6
    }

    init() { self.bytes = 0 }

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
          case .root:         fatalError()
          case .anything:     s = "*"
          case .structField:  s = "s#\(idx)"
          case .tupleField:   s = "t#\(idx)"
          case .enumCase:     s = "e#\(idx)"
          case .classField:   s = "c#\(idx)"
          case .tailElements: s = "tail"
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
        case anyOfKind:
          return pop(numBits: numBits)
        default:
          return nil
      }
    }

    func popAllValueFields() -> Path {
      var p = self
      while true {
        let (k, _, numBits) = p.top
        switch k {
          case .structField, .tupleField, .enumCase:
            p = p.pop(numBits: numBits)
          case .root, .anything, .classField, .tailElements:
            return p
        }
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
    
    /*
    static func unitTest() {
      let p = Path()
      let p1 = p.push(kind: .structField, index: 3)
      let p2 = p1.push(kind: .classField, index: 12345678)
      let t = p2.top
      let (k3, i3, p3) = p2.pop()
      let (k4, i4, p4) = p3.pop()
      let po = p2.push(kind: .enumCase, index: 12345678)
      let to = po.pop()
    }
    */
  }

  enum UpDown {
    case up, down
  }

  private struct VisitedKey : Hashable {
    let value: HashableValue
    let path: Path
    let followStores: Bool
    let upDown: UpDown
  }

  private struct VisitCounter {
    var numVisits = 0
    mutating func recordVisit() -> Int {
      numVisits += 1
      return numVisits
    }
  }

  private var visitedValues = Set<VisitedKey>()
  
  private var numPathesPerValue = Dictionary<HashableValue, VisitCounter>()
  
  private let calleeAnalysis: CalleeAnalysis
  
  init(calleeAnalysis: CalleeAnalysis) { self.calleeAnalysis = calleeAnalysis }

  public mutating func escapes(_ allocRef: AllocRefInst) -> Escapes {
    assert(numPathesPerValue.isEmpty)
    let result = walkAndCache(allocRef,
                              path: Path(),
                              followStores: false,
                              direction: .down)
    numPathesPerValue.removeAll(keepingCapacity: true)
    visitedValues.removeAll(keepingCapacity: true)
    return result
  }

  private mutating func walkAndCache(_ value: Value,
                                     path: Path,
                                     followStores: Bool,
                                     direction: UpDown) -> Escapes {
    var p = path
    let key = VisitedKey(value: value.hashable, path: p,
                         followStores: followStores, upDown: direction)
    if !visitedValues.insert(key).inserted {
      return Escapes()
    }
    // print("  --- cache \(direction): \(path) \(value)")
    if numPathesPerValue[value.hashable, default: VisitCounter()].recordVisit() >= 8 {
      p = Path.matchingAll
    }
    switch direction {
      case .up:
        return walkUp(value, path: p, followStores: followStores)
      case .down:
        return walkDown(value, path: p, followStores: followStores)
    }
  }

  private mutating func walkDown(_ value: Value,
                                 path: Path,
                                 followStores: Bool) -> Escapes {
    var result = Escapes()
    for use in value.uses {
      let user = use.instruction
      switch user {
        case let rta as RefTailAddrInst:
          if let newPath = path.popIfMatches(anyOfKind: .tailElements) {
            result |= walkDown(rta, path: newPath, followStores: followStores)
          }
        case let rea as RefElementAddrInst:
          if let newPath = path.popIfMatches(kind: .classField, index: rea.fieldIndex) {
            result |= walkDown(rea, path: newPath, followStores: followStores)
          }
        case let str as StructInst:
          result |= walkDown(str, path: path.push(kind: .structField, index: use.index),
                             followStores: followStores)
        case let se as StructExtractInst:
          if let newPath = path.popIfMatches(kind: .structField, index: se.fieldIndex) {
            result |= walkDown(se, path: newPath, followStores: followStores)
          }
        case let sea as StructElementAddrInst:
          if let newPath = path.popIfMatches(kind: .structField, index: sea.fieldIndex) {
            result |= walkDown(sea, path: newPath, followStores: followStores)
          }
        case let t as TupleInst:
          result |= walkDown(t, path: path.push(kind: .tupleField, index: use.index),
                             followStores: followStores)
        case let te as TupleExtractInst:
          if let newPath = path.popIfMatches(kind: .tupleField, index: te.fieldIndex) {
            result |= walkDown(te, path: newPath, followStores: followStores)
          }
        case let tea as TupleElementAddrInst:
          if let newPath = path.popIfMatches(kind: .tupleField, index: tea.fieldIndex) {
            result |= walkDown(tea, path: newPath, followStores: followStores)
          }
        case let e as EnumInst:
          result |= walkDown(e, path: path.push(kind: .enumCase, index: e.caseIndex),
                             followStores: followStores)
        case let ued as UncheckedEnumDataInst:
          if let newPath = path.popIfMatches(kind: .enumCase, index: ued.caseIndex) {
            result |= walkDown(ued, path: newPath, followStores: followStores)
          }
        case let se as SwitchEnumInst:
          guard let (caseIdx, newPath) = path.pop(kind: .enumCase) else {
            return Escapes.toGlobal
          }
          if let succBlock = se.getUniqueSuccessor(forCaseIndex: caseIdx) {
            if let payload = succBlock.arguments.first {
              result |= walkDown(payload, path: newPath,
                                 followStores: followStores)
            }
          }
        case let store as StoreInst:
          if use == store.sourceOperand {
            result |= walkUp(store.destination, path: path,
                             followStores: followStores)
          } else if followStores {
            assert(use == store.destinationOperand)
            result |= walkUp(store.source, path: path, followStores: followStores)
          }
        case let copyAddr as CopyAddrInst:
          if use == copyAddr.sourceOperand {
            result |= walkUp(copyAddr.destination, path: path,
                             followStores: followStores)
          } else if followStores {
            assert(use == copyAddr.destinationOperand)
            result |= walkUp(copyAddr.source, path: path,
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
          result |= walkAndCache(br.getArgument(for: use), path: path,
                                 followStores: followStores, direction: .down)
        case is ReturnInst:
          result |= Escapes.toReturn
        case let ap as ApplyInst:
          result |= handleArgumentEffects(argOp: use, apply: ap, path: path,
                                          followStores: followStores)
        case is LoadInst, is InitExistentialRefInst, is OpenExistentialRefInst,
             is BeginAccessInst, is BeginBorrowInst, is CopyValueInst,
             // Only handle "upcasts" for now, because down casts are not
             // representable in the projection path. E.g. if a base class is
             // downcast to two distinct derived classes, the same projection path
             // can cover distinct fields in the derived classes.
             is UpcastInst:
          result |= walkDown(user as! SingleValueInstruction, path: path,
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
      if result.contains(Escapes.toGlobal) {
        return Escapes.toGlobal
      }
    }
    return result
  }
  
  private mutating func handleArgumentEffects(argOp: Operand, apply: FullApplySite,
                                     path: Path,
                                     followStores: Bool) -> Escapes {
    guard let argIdx = apply.argumentIndex(of: argOp) else {
      // The callee or a type dependend operand does not let escape anything.
      return Escapes()
    }
    
    if followStores {
      // We don't know if something is stored to an argument.
      return Escapes.toGlobal
    }
  
    let callees = calleeAnalysis.getCallees(callee: apply.callee)
    if !callees.allCalleesKnown {
      return Escapes.toGlobal
    }

    var escaping = Escapes()

    for callee in callees {
      var escapeEffectFound = false
      for effect in callee.effects.forArgument(argIdx) {
        switch effect {
          case .noEscape(let argInfo):
            if path.matches(pattern: argInfo.pattern) {
              escapeEffectFound = true
            }
          case .escapesToReturn(let argInfo):
            if path.matches(pattern: argInfo.pattern) {
              if let result = apply.singleDirectResult {
                escaping |= walkDown(result, path: Path.matchingAll,
                                   followStores: false)
              } else {
                return Escapes.toGlobal
              }
              escapeEffectFound = true
            }
          case .escapesToArgument(let argInfo, let destArgIdx):
            if path.matches(pattern: argInfo.pattern) {
              escaping |= walkUp(apply.arguments[destArgIdx], path: path,
                               followStores: false)
              escapeEffectFound = true
            }
          default:
            break
        }
        if escaping.contains(Escapes.toGlobal) { return Escapes.toGlobal }
      }
      if !escapeEffectFound {
        return Escapes.toGlobal
      }
    }
    return escaping
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
          return walkAndCache(val, path: p, followStores: fSt,
                              direction: .down)
        case let arg as FunctionArgument:
          let result = walkAndCache(arg, path: p, followStores: fSt,
                                    direction: .down)
          let argIdx = arg.index
          if argIdx > Escapes.maxArgs {
            return Escapes.toGlobal
          }
          return result | Escapes.toArg(argIdx)
        case let arg as BlockArgument:
          if arg.isPhiArgument {
            var result = Escapes()
            for incoming in arg.incomingPhiValues {
              result |= walkAndCache(incoming, path: p, followStores: fSt,
                                     direction: .up)
            }
            return result
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
            return Escapes.toGlobal
          }
          val = str.operands[structField].value
          p = poppedPath
        case let se as StructExtractInst:
          val = se.operand
          p = p.push(kind: .structField, index: se.fieldIndex)
        case let t as TupleInst:
          guard let (tupleField, poppedPath) = p.pop(kind: .tupleField) else {
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
            return Escapes()
          }
          p = newPath
          val = e.operand
        case let ued as UncheckedEnumDataInst:
          val = ued.operand
          p = p.push(kind: .enumCase, index: ued.caseIndex)
        case is UpcastInst, is InitExistentialRefInst, is OpenExistentialRefInst,
             is BeginAccessInst, is BeginBorrowInst:
          val = (val as! Instruction).operands[0].value
        default:
          return Escapes.toGlobal
      }
    }
  }
}
