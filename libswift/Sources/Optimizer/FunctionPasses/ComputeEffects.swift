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
fileprivate typealias Pattern = Effect.Pattern
fileprivate typealias Escapes = Effect.Escapes

let computeEffects = FunctionPass(name: "compute-effects", {
  (function: Function, context: PassContext) in

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)

  var newEffects = StackList<(ArgInfo, Escapes)>(context)

  for arg in function.arguments {
    guard !arg.type.isTrivial(in: function) else {
      continue
    }
    if !escapeInfo.escapes(argument: arg, pattern: .anything) {
      newEffects.push((ArgInfo(arg, pattern: .anything), .noEscape))
      continue
    }
    if addArgEffects(arg, pattern: .noIndirection, to: &newEffects, &escapeInfo) {
      continue
    }
    _ = addArgEffects(arg, pattern: .oneIndirection, to: &newEffects, &escapeInfo)
  }

  context.modifyEffects(in: function) { (effects: inout FunctionEffects) in
    effects.removeComputedEffects()
    for (argInfo, esc) in newEffects {
      effects.append(Effect(kind: .escaping(argInfo, esc), isComputed: true))
    }
  }
  newEffects.removeAll()
})

private
func addArgEffects(_ arg: FunctionArgument, pattern: Pattern,
                   to newEffects: inout StackList<(ArgInfo, Escapes)>,
                   _ escapeInfo: inout EscapeInfo) -> Bool {
                   
  var tempEffects = StackList<(ArgInfo, Escapes)>(useContextFrom: newEffects)
  let argInfo = ArgInfo(arg, pattern: pattern)

  if escapeInfo.escapes(argument: arg, pattern: pattern,
      visitUse: { (op, path) in
        if op.instruction is ReturnInst && path.matches(pattern: .noIndirection) {
          tempEffects.push((argInfo, .toReturn))
          return false
        }
        return true
      },
      visitArg: { destArg, path in
        if path.matches(pattern: .noIndirection) {
          tempEffects.push((argInfo, .toArgument(destArg.index)))
          return false
        }
        return true
      }) {

    tempEffects.removeAll()
    return true
  }

  if tempEffects.isEmpty {
    newEffects.push((argInfo, .noEscape))
  } else {
    newEffects.append(contentsOf: tempEffects)
    tempEffects.removeAll()
  }

  return false
}
