//===--- FunctionSignatureOptimization.swift -------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

let functionSignatureOptimization = FunctionPass(name: "function-signature-optimization") {
  (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    if let apply = inst as? FullApplySite {
      specialize(apply: apply, context)
    }
  }
}

private func specialize(apply: FullApplySite, _ context: FunctionPassContext) {
  
}
