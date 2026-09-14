//===--- SimplifyDestructure.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension DestructureTupleInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if replaceWithUndefs(context) {
      return
    }
    foldWithAggregateConstruction(context)
  }
}

extension DestructureStructInst : OnoneSimplifiable, SILCombineSimplifiable {
  func simplify(_ context: SimplifyContext) {
    if replaceWithUndefs(context) {
      return
    }
    foldWithAggregateConstruction(context)
  }
}

private protocol DestructureInstruction : MultipleValueInstruction, UnaryInstruction {
  func createExtract(of aggregate: Value, elementIndex: Int, using builder: Builder) -> Value
}

extension DestructureTupleInst: DestructureInstruction {
  func createExtract(of aggregate: Value, elementIndex: Int, using builder: Builder) -> Value {
    return builder.createTupleExtract(tuple: aggregate, elementIndex: elementIndex)
  }
}

extension DestructureStructInst: DestructureInstruction {
  func createExtract(of aggregate: Value, elementIndex: Int, using builder: Builder) -> Value {
    return builder.createStructExtract(struct: aggregate, fieldIndex: elementIndex)
  }
}

private protocol ConstructureInstruction : SingleValueInstruction {
  func clone(_ builder: Builder) -> ConstructureInstruction
}

extension TupleInst: ConstructureInstruction {
  fileprivate func clone(_ builder: Builder) -> ConstructureInstruction {
    return builder.createTuple(type: type, elements: Array(operands.values))
  }
}

extension StructInst: ConstructureInstruction {
  fileprivate func clone(_ builder: Builder) -> ConstructureInstruction {
    return builder.createStruct(type: type, elements: Array(operands.values))
  }
}

private extension DestructureInstruction {
  var aggregate: Value { operands[0].value }

  // Replaces a destructure of an undef operand with per-element undefs.
  //
  // ```
  //   (%0, %1) = destructure_tuple undef : $(Int, String)
  // ```
  // ->
  // ```
  //   // uses of %0 replaced with undef : $Int
  //   // uses of %1 replaced with undef : $String
  // ```
  func replaceWithUndefs(_ context: SimplifyContext) -> Bool {
    guard operand.value is Undef else {
      return false
    }
    for result in results {
      result.uses.replaceAll(with: Undef.get(type: result.type, context), context)
    }
    context.erase(instruction: self)
    return true
  }

  func foldWithAggregateConstruction(_ context: SimplifyContext) {

    switch aggregate {
    case let constructInst as ConstructureInstruction:
      // Eliminate the redundant instruction pair
      // ```
      //   %t = tuple (%0, %1, %2)
      //   (%3, %4, %5) = destructure_tuple %t
      // ```
      // and replace the results %3, %4, %5 with %0, %1, %2, respectively
      //
      tryFoldWithConstructure(constructure: constructInst, context)

    case let copy as CopyValueInst:
      // Similar to the pattern above, but with a copy_value:
      // Replace
      // ```
      //   %t = tuple (%0, %1, %2)
      //   %c = copy_value %t           // can also be a chain of multiple copies
      //   (%3, %4, %5) = destructure_tuple %c
      // ```
      // with
      // ```
      //   %c0 = copy_value %0
      //   %c1 = copy_value %1
      //   %c2 = copy_value %2
      //   %s = tuple (%0, %1, %2)      // keep the original tuple/struct instruction
      // ```
      // and replace the results %3, %4, %5 with %c0, %c1, %c2, respectively.
      //
      // This transformation has the advantage that we can do it even if the `tuple`/`struct`
      // has other uses than the `copy_value`.
      //
      tryFoldWithCopyOfConstructure(copy: copy, context)

    default:
      break
    }

    if !isDeleted,
       aggregate.type.isTrivial(in: parentFunction) || aggregate.ownership == .guaranteed
    {
      // ```
      //   (%1, %2) = destructure_tuple %t
      // ```
      // ->
      // ```
      //   %1 = tuple_extract %t, 0
      //   %2 = tuple_extract %t, 1
      // ```
      replaceWithAggregateExtract(context)
      return
    }
  }

  private func replaceWithAggregateExtract(_ context: SimplifyContext) {
    let builder = Builder(before: self, context)
    for (elementIdx, result) in results.enumerated() {
      let elementValue = createExtract(of: aggregate, elementIndex: elementIdx, using: builder)
      result.uses.replaceAll(with: elementValue, context)
    }
    context.erase(instruction: self)
  }

  private func tryFoldWithConstructure(constructure: ConstructureInstruction, _ context: SimplifyContext) {
    for use in constructure.uses where !use.endsLifetime {
      if !context.preserveDebugInfo && use.instruction is DebugValueInst {
        continue
      }
      if constructure.ownership == .owned {
        return
      }
    }

    for (result, operand) in zip(self.results, constructure.operands) {
      result.uses.replaceAll(with: operand.value, context)
    }

    if constructure.ownership == .owned {
      for use in constructure.uses where use.endsLifetime && use.instruction != self {
        let builder = Builder(before: use.instruction, context)
        let clonedConstructure = constructure.clone(builder)
        use.set(to: clonedConstructure, context)
      }
      context.erase(instruction: self)
      context.erase(instruction: constructure)
    } else {
      context.erase(instruction: self)
    }
  }

  private func tryFoldWithCopyOfConstructure(copy: CopyValueInst, _ context: SimplifyContext) {
    guard copy.uses.singleUse?.instruction == self,
          let constructure = copy.fromValue.lookThroughCopy as? ConstructureInstruction,
          constructure.parentBlock == self.parentBlock
    else {
      return
    }
    for (result, operand) in zip(self.results, constructure.operands) {
      if operand.value.type.isTrivial(in: parentFunction) {
        result.uses.replaceAll(with: operand.value, context)
      } else {
        let builder = Builder(before: constructure, context)
        let copiedOperand = builder.createCopyValue(operand: operand.value)
        result.uses.replaceAll(with: copiedOperand, context)
      }
    }
    context.erase(instruction: self)
    context.erase(instruction: copy)
  }
}
