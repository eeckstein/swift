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

fileprivate extension Optional where Wrapped == ProjectionPath {
  mutating func merge(with rhs: ProjectionPath) {
    if let p = self {
      self = p.merge(with: rhs)
    } else {
      self = rhs
    }
  }
}

fileprivate extension Int {
  static var forReturn: Int { -1 }
  var isForReturn: Bool { self == -1 }
}

let computeEffects = FunctionPass(name: "compute-effects", {
  (function: Function, context: PassContext) in

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)
  var newEffects = StackList<Effect>(context)
  var effectsDict = Dictionary<Int, ProjectionPath?>()

  for arg in function.arguments {
    guard !arg.type.isTrivial(in: function) else {
      continue
    }
    if !escapeInfo.escapes(argument: arg, pattern: .anything) {
      newEffects.push(Effect(.notEscaping(ArgInfo(arg, pattern: .anything))))
      continue
    }
    if addArgEffects(arg, pattern: .noIndirection, to: &newEffects,
                     &escapeInfo, &effectsDict) {
      continue
    }
    _ = addArgEffects(arg, pattern: .oneIndirection, to: &newEffects,
                      &escapeInfo, &effectsDict)
  }

  context.modifyEffects(in: function) { (effects: inout FunctionEffects) in
    effects.removeComputedEffects()
    effects.append(contentsOf: newEffects)
  }
  newEffects.removeAll()
})

private
func addArgEffects(_ arg: FunctionArgument, pattern: Pattern,
                   to newEffects: inout StackList<Effect>,
                   _ escapeInfo: inout EscapeInfo,
                   _ effectsDict: inout Dictionary<Int, ProjectionPath?>) -> Bool {
  assert(effectsDict.isEmpty)

  if escapeInfo.escapes(argument: arg, pattern: pattern,
      visitUse: { (op, path) in
        if op.instruction is ReturnInst {
          effectsDict[Int.forReturn, default: nil].merge(with: path)
          return false
        }
        return true
      },
      visitArg: { destArg, path in
        effectsDict[destArg.index, default: nil].merge(with: path)
        return false
      }) {

    effectsDict.removeAll(keepingCapacity: true)
    return true
  }

  let argInfo = ArgInfo(arg, pattern: pattern)

  if effectsDict.isEmpty {
    newEffects.push(Effect(.notEscaping(argInfo)))
  } else {
    for argIdx in effectsDict.keys.sorted() {
      let path = effectsDict[argIdx]!!
      if argIdx.isForReturn {
        newEffects.push(Effect(.escapesToReturn(argInfo, path)))
      } else {
        newEffects.push(Effect(.escapesToArgument(argInfo, argIdx, path)))
      }
    }
    effectsDict.removeAll(keepingCapacity: true)
  }

  return false
}
