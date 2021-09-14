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

public enum Escapes {
  case noEscape
  case toGlobal
  case toReturn
  case toArgument(Int)
}

public struct Effect : CustomStringConvertible {

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

  public struct ArgInfo : CustomStringConvertible{
    public let argIndex: Int
    public let pattern: Pattern
    
    public init(index: Int, pattern: Pattern) {
      self.argIndex = index
      self.pattern = pattern
    }
    
    public init?(parser: inout StringParser, for function: Function) {
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
      self.argIndex = argIdx
      self.pattern = pattern
    }
    
    public var description: String { "(\(argIndex), \(pattern))" }
  }

  public enum Kind {
    case noReadGlobal
    case noWriteGlobal
    case noRead(ArgInfo)
    case noWrite(ArgInfo)
    case escaping(ArgInfo, Escapes)
  }
  
  public let kind: Kind
  public let isComputed: Bool

  public init(kind: Kind, isComputed: Bool) {
    self.kind = kind
    self.isComputed = isComputed
  }

  public init?(parser: inout StringParser, for function: Function) {

    isComputed = parser.consume("+")

    if parser.consume("noread_global") {
      kind = .noReadGlobal
    } else if parser.consume("nowrite_global") {
      kind = .noWriteGlobal
    } else if parser.consume("noread") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      kind = .noRead(argInfo)
    } else if parser.consume("nowrite") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      kind = .noWrite(argInfo)
    } else if parser.consume("noescape") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      kind = .escaping(argInfo, .noEscape)
    } else if parser.consume("escapes_to_return") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      kind = .escaping(argInfo, .toReturn)
    } else if parser.consume("escapes_to_argument_") {
      guard let destArgIdx = parser.consumeInt(withWhiteSpace: false) else {
        return nil
      }
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      kind = .escaping(argInfo, .toArgument(destArgIdx))
    } else {
      return nil
    }
  }
 
  public var description: String {
    let d: String
    switch kind {
      case .noReadGlobal:                 d = "noread_global"
      case .noWriteGlobal:                d = "nowrite_global"
      case .noRead(let argInfo):          d = "noread\(argInfo)"
      case .noWrite(let argInfo):         d = "nowrite\(argInfo)"
      case .escaping(let argInfo, let escapes):
        switch escapes {
          case .noEscape: d = "noescape\(argInfo)"
          case .toReturn: d = "escapes_to_return\(argInfo)"
          case .toArgument(let argIdx):
            d = "escapes_to_argument_\(argIdx)\(argInfo)"
          case .toGlobal:
            fatalError("cannot define a global-escaping effect")
        }
    }
    return (isComputed ? "+" : "") + d
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

  public mutating func removeComputedEffects() {
    effects = effects.filter { !$0.isComputed }
  }

  public mutating func append(_ effect: Effect) { effects.append(effect) }

  public var description: String {
    if effects.isEmpty {
      return ""
    }
    return "[" + effects.map { $0.description }.joined(separator: ", ") + "]"
  }
}
