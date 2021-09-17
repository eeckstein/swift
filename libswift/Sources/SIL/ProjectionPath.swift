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
    return "[\(descr)]"
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
    
    func testMerge(_ lhs: ProjectionPath, _ rhs: ProjectionPath,
                   expect: ProjectionPath) {
      let result = lhs.merge(with: rhs)
      assert(result == expect)
       let result2 = rhs.merge(with: lhs)
      assert(result2 == expect)
    }
   
    func merging() {
      typealias P = ProjectionPath
    
      testMerge(
        P(.classField).push(.structField).push(.structField).push(.tailElements),
        P(.classField).push(.structField).push(.enumCase).push(.structField).push(.tailElements),
        expect: P(.classField).push(.anyValueField).push(.structField).push(.tailElements))
      testMerge(
        P(.classField, index: 0).push(.classField, index: 1),
        P(.classField, index: 0),
        expect: P(.anything).push(.anyClassField))
      testMerge(
        P(.classField, index: 1).push(.classField, index: 0),
        P(.classField, index: 0),
        expect: P(.anything).push(.classField, index: 0))
      testMerge(
        P(.classField),
        P(.classField).push(.structField),
        expect: P(.classField).push(.anyValueField))
      testMerge(
        P(.classField, index: 0),
        P(.classField, index: 1).push(.structField),
        expect: P(.anyClassField).push(.anyValueField))
      testMerge(
        P(.classField).push(.structField).push(.structField),
        P(.classField).push(.structField),
        expect: P(.classField).push(.anyValueField).push(.structField))
      testMerge(
        P(.structField).push(.structField, index: 1),
        P(.structField).push(.structField, index: 2),
        expect: P(.anyValueField))
    }
    
    basicPushPop()
    merging()
  }
}
