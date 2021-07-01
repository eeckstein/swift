//===--- EscapeInfoDumper.swift - Dumps escape information ----------------===//
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

let escapeInfoDumper = FunctionPass(name: "dump-escape-info", {
  (function: Function, context: PassContext) in

  print("Escape information for \(function.name):")

  var escapeInfo = EscapeInfo(calleeAnalysis: context.calleeAnalysis)

  for block in function.blocks {
    for inst in block.instructions {
      if let allocRef = inst as? AllocRefInst {
        let escapes = escapeInfo.escapes(allocRef)
        print("\(escapes): \(allocRef)")
      }
    }
  }
  print("End function \(function.name)\n")
})
