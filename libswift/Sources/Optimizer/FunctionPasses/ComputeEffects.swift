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

let computeEffects = FunctionPass(name: "compute-effects", {
  (function: Function, context: PassContext) in

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)
  var newEffects = StackList<Effect>(context)

  for arg in function.arguments {
    guard !arg.type.isTrivial(in: function) else {
      continue
    }
    if !escapeInfo.isEscaping(arg, projection: ProjectionPath(.anything)) {
      let argInfo = ArgInfo(arg, projection: ProjectionPath(.anything))
      newEffects.push(Effect(.notEscaping(argInfo)))
      continue
    }
    if addArgEffects(arg, projection: ProjectionPath(.anyValueFields),
                     to: &newEffects, &escapeInfo) {
      _ = addArgEffects(arg, projection: ProjectionPath(.anyValueFields)
                                                  .push(.anyClassField)
                                                  .push(.anyValueFields),
                        to: &newEffects, &escapeInfo)
    }
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

  var toArgIdxOrNil: Int?
  var foundReturn: ReturnInst?
  var toPathOrNil: ProjectionPath?

  if escapeInfo.isEscaping(arg, projection: projection,
      visitUse: { (op, path) in
        if let ret = op.instruction as? ReturnInst {
          if toArgIdxOrNil != nil { return .markEscaping }
          toPathOrNil = path.mergeOrAssign(with: toPathOrNil)
          assert(foundReturn == nil || foundReturn == ret)
          foundReturn = ret
          return .ignore
        }
        return .continueWalking
      },
      visitArg: { destArg, path, followStores in
        if followStores { return .markEscaping }
        let argIdx = destArg.index
        if let toArgIdx = toArgIdxOrNil, toArgIdx != argIdx { return .markEscaping }
        if foundReturn != nil { return .markEscaping }
        toArgIdxOrNil = argIdx
        toPathOrNil = path.mergeOrAssign(with: toPathOrNil)
        return .continueWalking
      }) {
    return false
  }

  let argInfo = ArgInfo(arg, projection: projection)

  guard let toPath = toPathOrNil else {
    newEffects.push(Effect(.notEscaping(argInfo)))
    return true
  }

  func isFromArgMatching(_ a: FunctionArgument,
                    _ p: ProjectionPath, _: Bool) -> EscapeInfo.CallbackResult {
    return a == arg && p.matches(pattern: projection) ? .ignore : .markEscaping
  }

  if let returnInst = foundReturn {
    let exclusive = !escapeInfo.isDefEscaping(
      of: returnInst.operand, projection: toPath, followStores: true,
      visitUse: { (op, path) in
        if op.instruction is ReturnInst {
          return path.matches(pattern: toPath) ? .ignore : .markEscaping
        }
        return .continueWalking
      },
      visitArg: { a, p, followStores in
        if a == arg && p.matches(pattern: projection) { return .continueWalking }
        return .markEscaping
      })
    
    let toArgInfo = ArgInfo(returnProjection: toPath)
    newEffects.push(Effect(.escaping(argInfo, toArgInfo, exclusive)))
  } else {
    let toArgIdx = toArgIdxOrNil!
    let toArg = arg.block.function.arguments[toArgIdx]
    let exclusive = !escapeInfo.isEscaping(
      toArg, projection: toPath, followStores: true,
      visitArg: { a, p, _ in
        if a == arg && p.matches(pattern: projection) { return .continueWalking }
        if a == toArg && p.matches(pattern: toPath) { return .continueWalking }
        return .markEscaping
      })

    let toArgInfo = ArgInfo(argIndex: toArgIdx, projection: toPath)
    newEffects.push(Effect(.escaping(argInfo, toArgInfo, exclusive)))
  }

  return true
}
