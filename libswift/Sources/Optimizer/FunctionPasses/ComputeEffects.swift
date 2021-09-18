//===--- ComputeEffects.swift - Compute function effects ------------------===//
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

fileprivate typealias ArgInfo = Effect.ArgInfo

fileprivate extension Optional where Wrapped == ProjectionPath {
  mutating func merge(with rhs: ProjectionPath) {
    if let p = self {
      self = p.merge(with: rhs)
    } else {
      self = rhs
    }
  }
}

let computeEffects = FunctionPass(name: "compute-effects", {
  (function: Function, context: PassContext) in

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)
  var newEffects = StackList<Effect>(context)

  for arg in function.arguments {
    guard !arg.type.isTrivial(in: function) else {
      continue
    }
    if !escapeInfo.escapes(argument: arg, projection: ProjectionPath(.anything)) {
      let argInfo = ArgInfo(arg, projection: ProjectionPath(.anything))
      newEffects.push(Effect(.notEscaping(argInfo)))
      continue
    }
    if addArgEffects(arg, projection: ProjectionPath(.anyValueFields),
                     to: &newEffects, &escapeInfo) {
      continue
    }
    _ = addArgEffects(arg, projection: ProjectionPath(.anyValueFields)
                                                .push(.anyClassField)
                                                .push(.anyValueFields),
                      to: &newEffects, &escapeInfo)
  }

  context.modifyEffects(in: function) { (effects: inout FunctionEffects) in
    effects.removeComputedEffects()
    effects.append(contentsOf: newEffects)
  }
  newEffects.removeAll()
})

private
func addArgEffects(_ arg: FunctionArgument, projection: ProjectionPath,
                   to newEffects: inout StackList<Effect>,
                   _ escapeInfo: inout EscapeInfo) -> Bool {

  var argEscapes = Dictionary<Int, ProjectionPath?>()
  var returnEscape: ProjectionPath?

  if escapeInfo.escapes(argument: arg, projection: projection,
      visitUse: { (op, path) in
        if op.instruction is ReturnInst {
          returnEscape.merge(with: path)
          return false
        }
        return true
      },
      visitArg: { destArg, path in
        argEscapes[destArg.index, default: nil].merge(with: path)
        return false
      }) {

    return true
  }

  let argInfo = ArgInfo(arg, projection: projection)

  if argEscapes.isEmpty && returnEscape == nil {
    newEffects.push(Effect(.notEscaping(argInfo)))
  } else {
    for toArgIdx in argEscapes.keys.sorted() {
      let toArgInfo = ArgInfo(toArgIdx, projection: argEscapes[toArgIdx]!!)
      newEffects.push(Effect(.escaping(argInfo, toArgInfo)))
    }
    if let returnPath = returnEscape {
      let toArgInfo = ArgInfo(returnProjection: returnPath)
      newEffects.push(Effect(.escaping(argInfo, toArgInfo)))
    }
  }

  return false
}
