//===--- SimplifyStrongRetainRelease.swift - strong_retain/release opt ----===//
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

let simplifyCopyBlockPass = InstructionPass<CopyBlockInst>(
  name: "simplify-copy_block", {
  (copyBlock: CopyBlockInst, context: PassContext) in

  guard let srcArg = copyBlock.operand as? FunctionArgument else {
    return
  }

  if copyBlock.isEscapingWhenWalkingDown(context) {
    return
  }

  let builder = Builder(at: copyBlock, context)
  if copyBlock.parentFunction.hasOwnership {
    let copiedArg = builder.createCopyValue(operand: srcArg)
    copyBlock.uses.replaceAll(with: copiedArg, context)
  } else {
    builder.createStrongRetain(operand: srcArg)
    copyBlock.uses.replaceAll(with: srcArg, context)
  }

  context.erase(instruction: copyBlock)
})
