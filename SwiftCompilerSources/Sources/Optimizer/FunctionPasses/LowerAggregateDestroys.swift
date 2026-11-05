//===--- LowerAggregateDestroys.swift -------------------------------------===//
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

/// Lowers a `destroy_value` of a struct or tuple which contains only a single non-trivial field
/// (recursively) to a `destroy_value` of that field:
///
/// ```
///   destroy_value %0 : $S
/// ->
///   (%1, %2) = destructure_struct %0    // %2 is trivial
///   destroy_value %1
/// ```
///
/// Otherwise IRGen would create an outlined destroy function for the aggregate.
///
/// This pass should run late in the pipeline, because the destructured value can prevent
/// other optimizations.
let lowerAggregateDestroys = FunctionPass(name: "lower-aggregate-destroys") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership else {
    return
  }

  for inst in function.instructions {
    if let destroy = inst as? DestroyValueInst {
      lower(destroy: destroy, context)
    }
  }
}

private func lower(destroy: DestroyValueInst, _ context: FunctionPassContext) {
  if destroy.isDeadEnd {
    // A dead-end `destroy_value` is a no-op anyway.
    return
  }
  let function = destroy.parentFunction
  let builder = Builder(before: destroy, context)
  var value = destroy.destroyedValue

  while true {
    let type = value.type
    if type.isTuple {
      guard let index = type.tupleElements.indexOfSingleNonTrivialElement(in: function) else {
        break
      }
      value = builder.createDestructureTuple(tuple: value).results[index]
    } else if type.isStruct,
              // The deinit of the struct would get lost.
              !type.isMoveOnly,
              !type.isValueTypeWithDeinit,
              let fields = type.getNominalFields(in: function),
              let index = fields.indexOfSingleNonTrivialElement(in: function)
    {
      value = builder.createDestructureStruct(struct: value).results[index]
    } else {
      break
    }
  }
  if value != destroy.destroyedValue {
    builder.createDestroyValue(operand: value)
    context.erase(instruction: destroy)
  }
}

private extension Collection where Element == Type {
  func indexOfSingleNonTrivialElement(in function: Function) -> Int? {
    var result: Int? = nil
    for (index, elementType) in self.enumerated() where !elementType.isTrivial(in: function) {
      if result != nil {
        return nil
      }
      result = index
    }
    return result
  }
}
