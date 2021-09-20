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
        var results = Set<String>() 
      
        let escapes = escapeInfo.isEscaping(allocRef,
          visitUse: { (op, path) in
            if op.instruction is ReturnInst {
              results.insert("return[\(path)]")
              return .ignore
            }
            return .continueWalking
          },
          visitArg: { arg, path, followStores in
            results.insert("arg\(arg.index)[\(path)]")
            return .continueWalking
          })
        
        let res: String
        if escapes {
          res = "global"
        } else if results.isEmpty {
          res = " -    "
        } else {
          res = Array(results).sorted().joined(separator: ",")
        }
        print("\(res): \(allocRef)")
      }
    }
  }
  print("End function \(function.name)\n")
})
