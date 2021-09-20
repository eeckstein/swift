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

  public struct ArgInfo : CustomStringConvertible{
    private let argIdxOrReturn: Int? // nil: refers to the function return value
    public let projection: ProjectionPath
    
    public init(argIndex: Int, projection: ProjectionPath) {
      self.argIdxOrReturn = argIndex
      self.projection = projection
    }

    public init(returnProjection: ProjectionPath) {
      self.argIdxOrReturn = nil
      self.projection = returnProjection
    }

    public init(_ arg: Argument, projection: ProjectionPath) {
      self.init(argIndex: arg.index, projection: projection)
    }

    public init?(parser: inout StringParser, for function: Function,
                 fromSIL: Bool, acceptReturn: Bool = false) {
      if parser.consume("self") && function.hasSelfArgument {
        self.argIdxOrReturn = function.selfArgumentIndex
      } else if parser.consume("argument") || parser.consume("arg") {
        guard let argIdx = parser.consumeInt() else { return nil }
        self.argIdxOrReturn = fromSIL ? argIdx
                                : argIdx + function.numIndirectResultArguments
      } else if acceptReturn && parser.consume("return") {
        if !fromSIL && function.numIndirectResultArguments > 0 {
          if function.numIndirectResultArguments != 1 {
            // Currently not supported
            return nil
          }
          self.argIdxOrReturn = 0
        } else {
          self.argIdxOrReturn = nil
        }
      } else {
        return nil
      }

      if parser.consume(".") {
        guard let p = ProjectionPath(parser: &parser) else { return nil }
        self.projection = p
      } else {
        self.projection = ProjectionPath(.anyValueFields)
      }
    }
    
    public var description: String {
      let argStr: String
      if isForReturn {
        argStr = "return"
      } else {
        argStr = "arg\(argIndex)"
      }
      let pathStr = (projection == ProjectionPath(.anyValueFields) ?
                      "" : ".\(projection)")
      return "\(argStr)\(pathStr)"
    }
    
    public var isForReturn: Bool { return argIdxOrReturn == nil }
    public var isForArg: Bool { return argIdxOrReturn != nil }
    public var argIndex: Int { return argIdxOrReturn! }

    public func matches(argIndex: Int, _ rhsPath: ProjectionPath) -> Bool {
      return argIdxOrReturn == argIndex && rhsPath.matches(pattern: projection)
    }

    public func matchesReturn(_ rhsPath: ProjectionPath) -> Bool {
      return isForReturn && rhsPath.matches(pattern: projection)
    }
  }

  public enum Kind {
    case noReads
    case noWrites
    case notReading(ArgInfo)
    case notWriting(ArgInfo)
    case notEscaping(ArgInfo)
    case escaping(ArgInfo, ArgInfo, Bool)  // fromArg, to, exclusive
  }
  
  public let kind: Kind
  public let isComputed: Bool

  public init(_ kind: Kind, isComputed: Bool = true) {
    self.kind = kind
    self.isComputed = isComputed
  }

  public init?(parser: inout StringParser, for function: Function, fromSIL: Bool) {

    isComputed = parser.consume("+")

    if parser.consume("noReads") {
      kind = .noReads
    } else if parser.consume("noWrites") {
      kind = .noWrites

    } else if parser.consume("notReading") {
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else {
        return nil
      }
      kind = .notReading(argInfo)

    } else if parser.consume("notWriting") {
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else {
        return nil
      }
      kind = .notWriting(argInfo)

    } else if parser.consume("notEscaping") {
      if !parser.consume("(") { return nil }
      guard let argInfo = ArgInfo(parser: &parser, for: function,
                                  fromSIL: fromSIL) else {
        return nil
      }
      kind = .notEscaping(argInfo)
      if (!parser.consume(")")) { return nil }

    } else if parser.consume("escaping") {
      if !parser.consume("(") { return nil }

      guard let from = ArgInfo(parser: &parser, for: function,
                               fromSIL: fromSIL) else { return nil }

      if !parser.consume(",") || !parser.consume("to:") { return nil }

      guard let to = ArgInfo(parser: &parser, for: function,
                       fromSIL: fromSIL, acceptReturn: true) else { return nil }

      let notExclusive = parser.consume(",") && parser.consume("notExclusive")

      kind = .escaping(from, to, !notExclusive)
      if !parser.consume(")") { return nil }

    } else {
      return nil
    }
  }
 
  public var description: String {

    func pathArg(_ path: ProjectionPath) -> String {
      path == ProjectionPath(.anyValueFields) ? "" : ", \(path)"
    }

    let d: String
    switch kind {
      case .noReads:                  d = "noReads"
      case .noWrites:                 d = "noWrites"
      case .notReading(let argInfo):  d = "notReading(\(argInfo))"
      case .notWriting(let argInfo):  d = "noWriting(\(argInfo))"
      case .notEscaping(let argInfo): d = "notEscaping(\(argInfo))"
      case .escaping(let from, let to, let exclusive):
        let exclStr = exclusive ? "" : ", notExclusive"
        d = "escaping(\(from), to: \(to)\(exclStr))"
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
      effects.append(effect)
      if case .escaping(_, let to, _) = effect.kind,
         to.isForArg && to.argIndex == 0 &&
         !fromSIL && function.numIndirectResultArguments > 0 {
        effects.append(Effect(.notEscaping(Effect.ArgInfo(argIndex: 0,
                                projection: ProjectionPath(.anything))),
                                isComputed: effect.isComputed))
      }

      if parser.isEmpty() { return true }
      if !parser.consume(",") { return false }
    }
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
