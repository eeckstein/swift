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
    let transEsc = escapeInfo.escapes(argument: arg, pattern: .transitive)
    let esc = escapeInfo.escapes(argument: arg, pattern: .firstLevel)
    switch (transEsc, esc) {
      case (.toGlobal, .toGlobal):
        break
      case (.toGlobal, _):
        newEffects.push(Effect(kind: .escaping(Effect.ArgInfo(index: argIdx,
                        pattern: .firstLevel), esc), isComputed: true))
      case (.noEscape, _):
        newEffects.push(Effect(kind: .escaping(Effect.ArgInfo(index: argIdx,
                        pattern: .transitive), transEsc), isComputed: true))
      case (_, .noEscape):
        newEffects.push(Effect(kind: .escaping(Effect.ArgInfo(index: argIdx,
                        pattern: .transitive), transEsc), isComputed: true))
        newEffects.push(Effect(kind: .escaping(Effect.ArgInfo(index: argIdx,
                        pattern: .firstLevel), esc), isComputed: true))
      default:
        newEffects.push(Effect(kind: .escaping(Effect.ArgInfo(index: argIdx,
                        pattern: .transitive), transEsc), isComputed: true))
    }
  }

  context.modifyEffects(in: function) { (effects: inout FunctionEffects) in
    effects.removeComputedEffects()
    effects.append(from: newEffects)
  }
  newEffects.removeAll()
})
