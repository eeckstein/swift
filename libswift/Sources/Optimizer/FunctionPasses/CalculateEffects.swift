//===--- CalculateEffects.swift - Calculate function effects --------------===//
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

let calculateEffects = FunctionPass(name: "calculate-effects", {
  (function: Function, context: PassContext) in

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)

  for arg in function.entryBlock.arguments {
  /*
    if !arg.type.isTrivial(in: function) {
      switch escapeInfo.escapes(argument: arg, pattern: .transitive) {
        case
      }
    }
  */
  }

})
