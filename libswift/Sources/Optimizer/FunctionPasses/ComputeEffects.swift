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

let computeEffects = FunctionPass(name: "compute-effects", {
  (function: Function, context: PassContext) in

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)

  var newEffects = StackList<Effect>(context)

  for (argIdx, arg) in function.arguments.enumerated() {
    guard !arg.type.isTrivial(in: function) else {
      continue
    }
    let esc0 = escapeInfo.escapes(argument: arg, pattern: .noIndirection)
    let esc1 = escapeInfo.escapes(argument: arg, pattern: .oneOrMoreIndirections)
    switch (esc0, esc1) {
      case (.toGlobal, .toGlobal):
        break
      case (.noEscape, .noEscape):
        newEffects.push(Effect(kind: .escaping(Effect.ArgInfo(index: argIdx,
                        pattern: .anything), esc0), isComputed: true))
      case (_, .toGlobal):
        newEffects.push(Effect(kind: .escaping(Effect.ArgInfo(index: argIdx,
                        pattern: .noIndirection), esc0), isComputed: true))
      case (_, .toReturn), (_, .toArgument):
        fatalError("anything effects cannot escape to return or argument")
      default:
        fatalError("noIndirection effects don't include oneOrMoreIndirections effects")
    }
  }

  context.modifyEffects(in: function) { (effects: inout FunctionEffects) in
    effects.removeComputedEffects()
    effects.append(from: newEffects)
  }
  newEffects.removeAll()
})
