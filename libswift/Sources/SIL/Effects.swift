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

public enum Effect : CustomStringConvertible {

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

  case noReadGlobal
  case noWriteGlobal
  case noRead(ArgInfo)
  case noWrite(ArgInfo)
  case noEscape(ArgInfo)
  case escapesToReturn(ArgInfo)
  case escapesToArgument(ArgInfo, Int)

  public init?(parser: inout StringParser, for function: Function) {

    if parser.consume("noread_global") {
      self = .noReadGlobal
    } else if parser.consume("nowrite_global") {
      self = .noWriteGlobal
    } else if parser.consume("noread") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      self = .noRead(argInfo)
    } else if parser.consume("nowrite") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      self = .noWrite(argInfo)
    } else if parser.consume("noescape") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      self = .noEscape(argInfo)
    } else if parser.consume("escapes_to_return") {
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      self = .escapesToReturn(argInfo)
    } else if parser.consume("escapes_to_argument_") {
      guard let destArgIdx = parser.consumeInt(withWhiteSpace: false) else {
        return nil
      }
      guard let argInfo = ArgInfo(parser: &parser, for: function) else {
        return nil
      }
      self = .escapesToArgument(argInfo, destArgIdx)
    } else {
      return nil
    }
  }
 
  public var description: String {
    switch self {
      case .noReadGlobal:                 return "noread_global"
      case .noWriteGlobal:                return "nowrite_global"
      case .noRead(let argInfo):          return "noread\(argInfo)"
      case .noWrite(let argInfo):         return "nowrite\(argInfo)"
      case .noEscape(let argInfo):        return "noescape\(argInfo)"
      case .escapesToReturn(let argInfo): return "escapes_to_return\(argInfo)"
      case .escapesToArgument(let argInfo, let destArgIdx):
        return "escapes_to_argument_\(destArgIdx)\(argInfo)"
    }
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
      switch $0 {
        case .noReadGlobal:                      return false
        case .noWriteGlobal:                     return false
        case .noRead(let argInfo):               return argInfo.argIndex == index
        case .noWrite(let argInfo):              return argInfo.argIndex == index
        case .noEscape(let argInfo):             return argInfo.argIndex == index
        case .escapesToReturn(let argInfo):      return argInfo.argIndex == index
        case .escapesToArgument(let argInfo, _): return argInfo.argIndex == index
      }
    }
  }
  
  public var description: String {
    if effects.isEmpty {
      return ""
    }
    return "[" + effects.map { $0.description }.joined(separator: ", ") + "]"
  }
}
