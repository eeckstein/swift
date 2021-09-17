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

  public enum Pattern : CustomStringConvertible {
    // Matches any value-type projections, but no reference-indirections, e.g
    // struct.field.enumCase.tupleElement
    case noIndirection
    
    // Matches anything which involves exactly one reference-indirection, e.g.
    // class.field
    // struct.field.class.field
    case oneIndirection
    
    // Matches anything
    case anything

    // TODO: support something like "x.y.*.z.**"
    // case accessPath

    init?(parser: inout StringParser, for function: Function, argIdx: Int) {
      if parser.consume("*.*") {
        self = .oneIndirection
      } else if parser.consume("**") {
        self = .anything
      } else if parser.consume("*") {
        self = .noIndirection
      } else {
        return nil
      }
    }
    
    public var description: String {
      switch self {
        case .noIndirection:         return "*"
        case .oneIndirection:        return "*.*"
        case .anything:              return "**"
      }
    }
  }

  public struct ArgInfo : CustomStringConvertible{
    public let argIndex: Int
    public let pattern: Pattern
    
    public init(_ argIdx: Int, pattern: Pattern) {
      self.argIndex = argIdx
      self.pattern = pattern
    }

    public init(_ arg: Argument, pattern: Pattern) {
      self.init(arg.index, pattern: pattern)
    }
    
    public init?(parser: inout StringParser, for function: Function,
                 fromSIL: Bool) {
      guard let argIdx = parser.consumeArgumentIndex(for: function,
                                                     fromSIL: fromSIL) else {
        return nil
      }
      let pattern: Pattern
      if parser.consume(",") {
        guard let p = Pattern(parser: &parser, for: function, argIdx: argIdx) else {
          return nil
        }
        pattern = p
      } else {
        pattern = .anything
      }
      self.argIndex = argIdx
      self.pattern = pattern
    }
    
    public var description: String { "\(argIndex), \(pattern)" }
    
    public func matches(_ rhsIdx: Int, _ rhsPath: ProjectionPath) -> Bool {
      return argIndex == rhsIdx && rhsPath.matches(pattern: pattern)
    }
  }

  public enum Kind {
    case noReadGlobal
    case noWriteGlobal
    case noRead(ArgInfo)
    case noWrite(ArgInfo)
    case notEscaping(ArgInfo)
    case escapesToReturn(ArgInfo, ProjectionPath)
    case escapesToArgument(ArgInfo, Int, ProjectionPath)
  }
  
  public let kind: Kind
  public let isComputed: Bool

  public init(_ kind: Kind, isComputed: Bool = true) {
    self.kind = kind
    self.isComputed = isComputed
  }

  public init?(parser: inout StringParser, for function: Function, fromSIL: Bool) {

    isComputed = parser.consume("+")

    if parser.consume("noread_global") {
      kind = .noReadGlobal
    } else if parser.consume("nowrite_global") {
      kind = .noWriteGlobal

    } else if parser.consume("noread") {
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else {
        return nil
      }
      kind = .noRead(argInfo)

    } else if parser.consume("nowrite") {
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else {
        return nil
      }
      kind = .noWrite(argInfo)

    } else if parser.consume("noescape") {
      if !parser.consume("(") { return nil }
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else {
        return nil
      }
      kind = .notEscaping(argInfo)
      if (!parser.consume(")")) { return nil }

    } else if parser.consume("escapes_to_return") {
      if !parser.consume("(") { return nil }
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else { return nil }
      let path: ProjectionPath
      if parser.consume(",") {
        guard let p = ProjectionPath(parser: &parser) else { return nil }
        path = p
      } else {
        path = ProjectionPath(.anyValueField)
      }
      kind = .escapesToReturn(argInfo, path)
      if !parser.consume(")") { return nil }

    } else if parser.consume("escapes_to_arg") {
      if !parser.consume("(") { return nil }
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else { return nil }
      if !parser.consume(",") { return nil }
      guard let destArgIdx = parser.consumeArgumentIndex(for: function,
                                  fromSIL: fromSIL) else { return nil }
      let path: ProjectionPath
      if parser.consume(",") {
        guard let p = ProjectionPath(parser: &parser) else { return nil }
        path = p
      } else {
        path = ProjectionPath(.anyValueField)
      }
      kind = .escapesToArgument(argInfo, destArgIdx, path)
      if (!parser.consume(")")) { return nil }
    } else {
      return nil
    }
  }
 
  public var description: String {

    func pathArg(_ path: ProjectionPath) -> String {
      path == ProjectionPath(.anyValueField) ? "" : ", \(path)"
    }

    let d: String
    switch kind {
      case .noReadGlobal:                 d = "noread_global"
      case .noWriteGlobal:                d = "nowrite_global"
      case .noRead(let argInfo):          d = "noread(\(argInfo))"
      case .noWrite(let argInfo):         d = "nowrite(\(argInfo))"
      case .notEscaping(let argInfo):     d = "noescape(\(argInfo))"
      case .escapesToReturn(let argInfo, let path):
        d = "escapes_to_return(\(argInfo)\(pathArg(path)))"
      case .escapesToArgument(let argInfo, let destArgIdx, let path):
        d = "escapes_to_arg(\(argInfo), \(destArgIdx)\(pathArg(path)))"
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
                             for function: Function,
                             fromSIL: Bool) -> Bool {
    if parser.isEmpty() { return true }

    while true {
      guard let effect = Effect(parser: &parser, for: function,
                                fromSIL: fromSIL) else {
        return false
      }
      appendEffect(effect, for: function, fromSIL: fromSIL)

      if parser.isEmpty() { return true }
      if !parser.consume(",") { return false }
    }
  }

  mutating private func appendEffect(_ effect: Effect, for function: Function,
                                     fromSIL: Bool) {
    if case .escapesToReturn(let argInfo, let path) = effect.kind {
      if !fromSIL && function.numIndirectResultArguments > 0 {
        if function.numIndirectResultArguments != 1 {
          // Currently not supported
          return
        }
        effects.append(Effect(.escapesToArgument(argInfo, 0, path),
                              isComputed: effect.isComputed))
        effects.append(Effect(.notEscaping(Effect.ArgInfo(0, pattern: .anything)),
                              isComputed: effect.isComputed))
        return
      }
    }
    effects.append(effect)
  }

  public mutating func removeComputedEffects() {
    effects = effects.filter { !$0.isComputed }
  }

  public mutating func append(_ effect: Effect) { effects.append(effect) }

  public mutating
  func append<S: Sequence>(contentsOf source: S) where S.Element == Effect {
    for elem in source {
      append(elem)
    }
  }

  public var description: String {
    if effects.isEmpty {
      return ""
    }
    return "[" + effects.map { $0.description }.joined(separator: ", ") + "]"
  }
}

extension StringParser {
  mutating func consumeArgumentIndex(for function: Function,
                                     fromSIL: Bool) -> Int? {
    if consume("self") && function.hasSelfArgument {
      return function.selfArgumentIndex
    }
    guard let argIdx = consumeInt() else {
      return nil
    }
    return fromSIL ? argIdx : argIdx + function.numIndirectResultArguments
  }
}
