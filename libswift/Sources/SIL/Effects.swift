//===--- Effects.swift - Defines function side effects --------------------===//
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

public struct Effect : CustomStringConvertible {
  public enum Sign {
    case no
    case may
    case must
  }
  
  // TODO: extend this to support something like "x.y.*.z.**"
  public enum Pattern : CustomStringConvertible {
    case firstLevel
    case transitive
    // TODO:
    // case accessPath

    init?(parser: inout StringParser, for function: Function, argIdx: Int) {
      if parser.consume("**") {
        self = .transitive
      } else if parser.consume("*") {
        self = .firstLevel
      } else {
        return nil
      }
    }
    
    public var description: String {
      switch self {
        case .firstLevel: return "*"
        case .transitive: return "**"
      }
    }
  }

  public enum Kind {
    case synchronize
    case readGlobal
    case writeGlobal
    case read(Pattern)
    case write(Pattern)
    case escape(Pattern)
  }

  public let sign: Sign
  public let kind: Kind
  public let argIdx: Int32?
 
  public init?(parser: inout StringParser, for function: Function) {

    func parseArgIdxAndPattern(parser: inout StringParser,
                               for function: Function) -> (Int32, Pattern)? {
      if !parser.consume("(") { return nil }
      guard let argIdx = parser.consumeInt() else { return nil }
      let pattern: Pattern
      if parser.consume(",") {
        guard let p = Pattern(parser: &parser, for: function, argIdx: argIdx) else {
          return nil
        }
        pattern = p
      } else {
        pattern = .transitive
      }
      if !parser.consume(")") { return nil }
      return (Int32(argIdx), pattern)
    }

    if parser.consume("no") {
      sign = .no
    } else if parser.consume("may") {
      sign = .may
    } else if parser.consume("must") {
      sign = .must
    } else {
      return nil
    }
    if parser.consume("synchronize") {
      kind = .synchronize
      argIdx = nil
    } else if parser.consume("read_global") {
      kind = .readGlobal
      argIdx = nil
    } else if parser.consume("write_global") {
      kind = .writeGlobal
      argIdx = nil
    } else if parser.consume("read") {
      guard let (ai, pattern) = parseArgIdxAndPattern(parser: &parser, for: function) else {
        return nil
      }
      kind = .read(pattern)
      argIdx = ai
    } else if parser.consume("write") {
      guard let (ai, pattern) = parseArgIdxAndPattern(parser: &parser, for: function) else {
        return nil
      }
      kind = .write(pattern)
      argIdx = ai
    } else if parser.consume("escape") {
      guard let (ai, pattern) = parseArgIdxAndPattern(parser: &parser, for: function) else {
        return nil
      }
      kind = .escape(pattern)
      argIdx = ai
    } else {
      return nil
    }
  }
 
  public var description: String {
    let signStr: String
    switch sign {
      case .no: signStr = "no"
      case .may: signStr = "may"
      case .must: signStr = "must"
    }
    
    let kindStr: String
    switch kind {
      case .synchronize: kindStr = "synchronize"
      case .readGlobal:  kindStr = "read_global"
      case .writeGlobal: kindStr = "write_global"
      case .read(let pattern):   kindStr = "read(\(argIdx!), \(pattern))"
      case .write(let pattern):  kindStr = "write(\(argIdx!), \(pattern))"
      case .escape(let pattern): kindStr = "escape(\(argIdx!), \(pattern))"
    }
    return signStr + kindStr
  }
}

public struct FunctionEffects : CustomStringConvertible, RandomAccessCollection {
  private var effects: [Effect] = []
  
  public init() {}
  
  public var startIndex: Int { 0 }
  public var endIndex: Int { effects.endIndex }
  
  public subscript(_ index: Int) -> Effect { effects[index] }
  
  mutating public func parse(parser: inout StringParser,
                             for function: Function) -> Bool {
    if parser.isEmpty() { return true }

    if !parser.consume("[") { return false }

    while true {
      guard let effect = Effect(parser: &parser, for: function) else {
        return false
      }
      effects.append(effect)
      if parser.consume("]") {
        if !parser.isEmpty() { return false }
        return true
      }
      if !parser.consume(",") { return false }
    }
  }
  
  public func forArgument(_ index: Int) -> LazyFilterCollection<FunctionEffects> {
    return self.lazy.filter {
      if let i = $0.argIdx {
        return Int(i) == index
      }
      return false
    }
  }
  
  public var description: String {
    if effects.isEmpty {
      return ""
    }
    return "[" + effects.map { $0.description }.joined(separator: ", ") + "]"
  }
}
