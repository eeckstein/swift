//===--- ProjectionPath.swift - a path of projections ---------------------===//
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

public struct ProjectionPath : CustomStringConvertible, Hashable {
  private let bytes: UInt64

  public enum FieldKind : Int {
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

    public var isValueField: Bool {
      switch self {
        case .anyValueField, .structField, .tupleField, .enumCase:
          return true
        case .root, .anything, .anyClassField, .classField, .tailElements:
          return false
      }
    }
    
    public var isClassField: Bool {
      switch self {
        case .anyClassField, .classField, .tailElements:
          return true
        case .root, .anything, .anyValueField, .structField, .tupleField, .enumCase:
          return false
      }
    }
  }

  public init() { self.bytes = 0 }

  public init(_ kind: FieldKind, index: Int = 0) {
    self = ProjectionPath().push(kind, index: index)
  }

  public init(pattern: Effect.Pattern) {
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

  public init?(parser: inout StringParser) {
    self.init()
    var entries: [(FieldKind, Int)] = []
    repeat {
      if parser.consume("**") {
        entries.append((.anything, 0))
      } else if parser.consume("c*") {
        entries.append((.anyClassField, 0))
      } else if parser.consume("v*") {
        entries.append((.anyValueField, 0))
      } else if parser.consume("ct") {
        entries.append((.tailElements, 0))
      } else if parser.consume("c") {
        guard let idx = parser.consumeInt(withWhiteSpace: false) else { return nil }
        entries.append((.classField, idx))
      } else if parser.consume("e") {
        guard let idx = parser.consumeInt(withWhiteSpace: false) else { return nil }
        entries.append((.enumCase, idx))
      } else if parser.consume("t") {
        guard let idx = parser.consumeInt(withWhiteSpace: false) else { return nil }
        entries.append((.tupleField, idx))
      } else if parser.consume("s") {
        guard let idx = parser.consumeInt(withWhiteSpace: false) else { return nil }
        entries.append((.structField, idx))
      } else {
        return nil
      }
    } while parser.consume(".")
 
    for (kind, idx) in entries.reversed() {
      self = self.push(kind, index: idx)
    }
  }

  private init(bytes: UInt64) { self.bytes = bytes }

  public var isEmpty: Bool { bytes == 0 }

  public var description: String {
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
    return descr
  }
  
  private var top: (kind: FieldKind, index: Int, numBits: Int) {
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

  private func pop(numBits: Int) -> ProjectionPath {
    return Self(bytes: bytes &>> numBits)
  }

  public func pop() -> (kind: FieldKind, index: Int, path: ProjectionPath) {
    let (k, idx, numBits) = top
    return (k, idx, pop(numBits: numBits))
  }

  public func pop(kind: FieldKind) -> (index: Int, path: ProjectionPath)? {
    let (k, idx, newPath) = pop()
    if k != kind { return nil }
    return (idx, newPath)
  }

  public func push(_ kind: FieldKind, index: Int = 0) -> ProjectionPath {
    var idx = index
    var b = bytes
    if (b >> 56) != 0 { return Self(.anything) }
    b = (b << 8) | UInt64(((idx & 0xf) << 4) | (kind.rawValue << 1))
    idx >>= 4
    while idx != 0 {
      if (b >> 56) != 0 { return Self(.anything) }
      b = (b << 8) | UInt64(((idx & 0x7f) << 1) | 1)
      idx >>= 7
    }
    return Self(bytes: b)
  }

  public func popIfMatches(_ kind: FieldKind, index: Int? = nil) -> ProjectionPath? {
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

  public var matchesAnyValueField: Bool {
    switch top.kind {
      case .anyValueField, .anything: return true
      default: return false
    }
  }

  public func popAllValueFields() -> ProjectionPath {
    var p = self
    while true {
      let (k, _, numBits) = p.top
      if !k.isValueField { return p }
      p = p.pop(numBits: numBits)
    }
  }
  
  public func popAllClassFields() -> ProjectionPath {
    var p = self
    while true {
      let (k, _, numBits) = p.top
      if !k.isClassField { return p }
      p = p.pop(numBits: numBits)
    }
  }
  
  public func matches(pattern: Effect.Pattern) -> Bool {
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

  public func merge(with rhs: ProjectionPath) -> ProjectionPath {
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
    return Self(.anything)
  }

  static public func unitTest() {
    func basicPushPop() {
      let p1 = ProjectionPath(.structField, index: 3).push(.classField, index: 12345678)
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
    
    func testMerge(_ lhsStr: String, _ rhsStr: String,
                   expect expectStr: String) {
      var lhsParser = StringParser(lhsStr)
      let lhs = ProjectionPath(parser: &lhsParser)!
      var rhsParser = StringParser(rhsStr)
      let rhs = ProjectionPath(parser: &rhsParser)!
      var expectParser = StringParser(expectStr)
      let expect = ProjectionPath(parser: &expectParser)!

      let result = lhs.merge(with: rhs)
      assert(result == expect)
       let result2 = rhs.merge(with: lhs)
      assert(result2 == expect)
    }
   
    func testMerge(_ lhs: ProjectionPath, _ rhs: ProjectionPath,
                   expect: ProjectionPath) {
      let result = lhs.merge(with: rhs)
      assert(result == expect, "path merging failed")
       let result2 = rhs.merge(with: lhs)
      assert(result2 == expect, "path merging failed")
    }
   
    func merging() {
      testMerge("ct.s0.e0.v*.c0",
                "ct.s0.e0.v*.c0",
        expect: "ct.s0.e0.v*.c0")
  
      testMerge("ct.s0.s0.c0",
                "ct.s0.e0.s0.c0",
        expect: "ct.s0.v*.c0")
  
      testMerge("c1.c0",
                "c0",
        expect: "c*.**")

      testMerge("c2.c1",
                "c2",
        expect: "c2.**")

      testMerge("s3.c0",
                "v*.c0",
        expect: "v*.c0")

      testMerge("c0",
                "s2.c1",
        expect: "v*.c*")

      testMerge("s1.s1.c2",
                "s1.c2",
        expect: "s1.v*.c2")

      testMerge("s1.s0",
                "s2.s0",
        expect: "v*")
    }
    
    basicPushPop()
    merging()
  }
}
