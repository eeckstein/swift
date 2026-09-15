//===--- OwnershipUtilities.swift -----------------------------------------===//
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

extension InstructionRange {
  /// Returns true if this instruction range is fully contained in the liverange of `value`.
  public func isFullyContainedIn(scopeOf value: Value) -> Bool {
    switch value.ownership {
    case .owned:
      return value.uses.endingLifetime.allSatisfy {
        !self.contains($0.instruction)
      }
    case .guaranteed:
      guard let beginBorrow = BeginBorrowValue(value.lookThroughForwardingInstructions) else {
        return false
      }
      if case .functionArgument = beginBorrow {
        // The lifetime of a guaranteed function argument spans over the whole function.
        return true
      }
      return beginBorrow.scopeEndingOperands.allSatisfy {
        !self.contains($0.instruction)
      }
    case .none, .unowned:
      return false
    }
  }
}

/// Extends the borrow scope of a guaranteed `value` to ensure it overlaps with a given instruction range.
///
/// This function attempts to move `end_borrow` instructions that fall within the specified range
/// to positions after the range ends, effectively extending the lifetime of the borrowed value
/// to cover the entire range. This is useful for optimizations where a borrowed value needs
/// to remain live throughout a specific code region.
///
/// Returns true if the borrow scope was successfully extended or already overlaps the range,
/// returns false if the extension is not possible due to conflicting lifetime constraints.
///
func extendBorrowScope(of value: Value,
                       toOverlap range: InstructionRange,
                       _ context: FunctionPassContext) -> Bool
{
  switch value.ownership {
  case .owned, .unowned:
    return false
  case .none:
    return true
  case .guaranteed:
    var beginBorrows = Stack<BeginBorrowValue>(context)
    defer { beginBorrows.deinitialize() }
    beginBorrows.append(contentsOf: value.getBorrowIntroducers(context))

    if let singleBeginBorrow = beginBorrows.singleElement {
      return extendBorrowScope(ofBeginBorrow: singleBeginBorrow, toOverlap: range, context)
    }

    for beginBorrow in beginBorrows {
      guard extendBorrowScope(ofBeginBorrow: beginBorrow, toOverlap: range, dryRun: true, context) else {
        return false
      }
    }
    for beginBorrow in beginBorrows {
      let success = extendBorrowScope(ofBeginBorrow: beginBorrow, toOverlap: range, context)
      assert(success, "extendBorrowScope failed in second run")
    }
    return true
  }
}


func extendBorrowScope(ofBeginBorrow beginBorrow: BeginBorrowValue,
                       toOverlap range: InstructionRange,
                       dryRun: Bool = false,
                       _ context: FunctionPassContext) -> Bool
{
  if case .functionArgument = beginBorrow {
    // The lifetime of a guaranteed function argument spans over the whole function.
    return true
  }

  guard var endBorrowsToMove = getEndBorrows(of: beginBorrow, inRange: range, context) else {
    return false
  }
  defer { endBorrowsToMove.deinitialize() }

  if endBorrowsToMove.isEmpty {
    // The borrow scope already completely overlaps the range. Nothing to do.
    return true
  }

  var rangeEndInstructions = InstructionSet(context)
  defer { rangeEndInstructions.deinitialize() }
  rangeEndInstructions.insert(contentsOf: range.ends)
  rangeEndInstructions.insert(contentsOf: range.exits)

  var insertionPoints: Stack<Instruction>

  // If the borrowed address is a projection of another borrowed reference, that borrow scope must
  // enclose this borrow scope. Therefore it needs to be extended as well:
  // ```
  //   %1 = begin_borrow %0
  //   %2 = ref_element_addr %1, #field
  //   %3 = load_borrow %2
  //   end_borrow %1
  //   end_borrow %3              // ill formed: use of the address after `end_borrow %1`
  // ```
  var enclosingBorrow: BeginBorrowValue? = nil

  switch beginBorrow {
  case .beginBorrow(let bbi):
    guard let ips = getInsertionPoints(for: bbi, endBorrowsToMove, rangeEndInstructions, context) else {
      return false
    }
    insertionPoints = ips

  case .loadBorrow(let loadBorrow):
    enclosingBorrow = loadBorrow.address.beginBorrowOfAddress
    guard let ips = getInsertionPoints(for: loadBorrow, endBorrowsToMove, rangeEndInstructions,
                                       ignoreEndBorrowsOf: enclosingBorrow, context) else {
      return false
    }
    insertionPoints = ips

  default:
    return false
  }
  defer { insertionPoints.deinitialize() }

  if let enclosingBorrow,
     !extendBorrowScope(ofBeginBorrow: enclosingBorrow, toOverlap: range, dryRun: true, context) {
    return false
  }

  if dryRun {
    return true
  }

  if let enclosingBorrow {
    let extended = extendBorrowScope(ofBeginBorrow: enclosingBorrow, toOverlap: range, context)
    assert(extended, "extending the enclosing borrow scope failed after a successful dry run")
  }

  // Move the `end_borrow`s out of the range.
  //
  context.erase(instructions: endBorrowsToMove)
  for insertionPoint in insertionPoints {
    // The `end_borrow`s of the enclosing scopes may just have been moved to this insertion point.
    // This scope is nested inside them, so its `end_borrow` must come first.
    var insertBefore = insertionPoint
    while let prev = insertBefore.previous, prev.isEndBorrow(ofScope: enclosingBorrow) {
      insertBefore = prev
    }
    Builder(before: insertBefore, context).createEndBorrow(of: beginBorrow.value)
  }
  return true
}

private func getEndBorrows(of beginBorrow: BeginBorrowValue,
                           inRange range: InstructionRange,
                           _ context: FunctionPassContext) -> Stack<Instruction>?
{
  var endBorrowsToMove = Stack<Instruction>(context)

  for scopeEndingOp in beginBorrow.scopeEndingOperands {
    if range.contains(scopeEndingOp.instruction) {
      guard let endBorrow = scopeEndingOp.instruction as? EndBorrowInst else {
        endBorrowsToMove.deinitialize()
        return nil
      }
      endBorrowsToMove.append(endBorrow)
    }
  }
  return endBorrowsToMove
}

private func getInsertionPoints(for beginBorrow: BeginBorrowInst,
                                _ endBorrowsToMove: Stack<Instruction>,
                                _ rangeEndInstructions: InstructionSet,
                                _ context: FunctionPassContext) -> Stack<Instruction>?
{
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(contentsOf: endBorrowsToMove.map { $0.next! })

  let enclosingValue = beginBorrow.borrowedValue
  guard enclosingValue.ownership == .owned else {
    return nil
  }
  var enclosingScopeEnds = InstructionSet(context)
  defer { enclosingScopeEnds.deinitialize() }
  enclosingScopeEnds.insert(contentsOf: enclosingValue.uses.endingLifetime.users)

  var insertionPoints = Stack<Instruction>(context)

  while let inst = worklist.pop() {
    if rangeEndInstructions.contains(inst) {
      insertionPoints.append(inst)
    } else {
      if enclosingScopeEnds.contains(inst) {
        insertionPoints.deinitialize()
        return nil
      }
      worklist.pushSuccessors(of: inst)
    }
  }
  return insertionPoints
}

private func getInsertionPoints(for loadBorrow: LoadBorrowInst,
                                _ endBorrowsToMove: Stack<Instruction>,
                                _ rangeEndInstructions: InstructionSet,
                                ignoreEndBorrowsOf enclosingBorrow: BeginBorrowValue?,
                                _ context: FunctionPassContext) -> Stack<Instruction>?
{
  var worklist = InstructionWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(contentsOf: endBorrowsToMove.map { $0.next! })

  let aliasAnalysis = context.aliasAnalysis
  var insertionPoints = Stack<Instruction>(context)

  while let inst = worklist.pop() {
    if rangeEndInstructions.contains(inst) {
      insertionPoints.append(inst)
    } else {
      // Alias analysis conservatively reports a "write" for the `end_borrow` of the reference which
      // holds the loaded address. That's exactly the borrow scope which the caller extends along
      // with this one, so it does not limit this borrow scope either.
      if inst.mayWrite(toAddress: loadBorrow.address, aliasAnalysis),
         !inst.isEndBorrow(ofScope: enclosingBorrow)
      {
        insertionPoints.deinitialize()
        return nil
      }
      worklist.pushSuccessors(of: inst)
    }
  }
  return insertionPoints
}

extension Instruction {
  /// True if this is an `end_borrow` which ends the borrow scope of `beginBorrow` - or the scope of a
  /// borrow which (transitively) holds the address that `beginBorrow` loads from.
  ///
  /// Such enclosing scopes must contain `beginBorrow`'s scope, therefore `extendBorrowScope` extends
  /// them together with it. That's why callers which extend `beginBorrow`'s scope can treat all of
  /// those `end_borrow`s alike: they neither limit the new scope nor may they precede its end.
  func isEndBorrow(ofScope beginBorrow: BeginBorrowValue?) -> Bool {
    guard let endBorrow = self as? EndBorrowInst else {
      return false
    }
    var scope = beginBorrow
    while let currentScope = scope {
      if endBorrow.borrow == currentScope.value {
        return true
      }
      // Only a `load_borrow`'s scope is enclosed by the borrow scope of the address' base reference.
      guard case .loadBorrow(let loadBorrow) = currentScope else {
        return false
      }
      scope = loadBorrow.address.beginBorrowOfAddress
    }
    return false
  }
}

extension Value {
  /// If this address is a projection of a borrowed reference, the `begin_borrow`/`load_borrow` which
  /// introduces that reference's borrow scope. That scope must enclose the lifetime of anything
  /// loaded from this address.
  var beginBorrowOfAddress: BeginBorrowValue? {
    if let baseReference = accessBase.reference {
      return BeginBorrowValue(baseReference.lookThroughForwardingInstructions)
    }
    return nil
  }

  /// Looks through forwarding instructions in the use-def chain and returns the original forwarded value.
  /// It looks through phi-arguments, terminator instructions and all kind of forwarding instructions
  /// which forward exactly one (non-trivial) operand.
  public var lookThroughForwardingInstructions: Value {
    if let bfi = definingInstruction as? BorrowedFromInst,
       !bfi.borrowedPhi.isReborrow,
       bfi.enclosingValues.count == 1
    {
      // Return the single forwarded enclosingValue
      return bfi.enclosingValues[0]
    }
    if let fi = definingInstruction as? ForwardingInstruction,
       let forwardedOp = fi.singleForwardedOperand
    {
       return forwardedOp.value.lookThroughForwardingInstructions
    } else if let termResult = TerminatorResult(self),
              let fi = termResult.terminator as? ForwardingInstruction,
              let forwardedOp = fi.singleForwardedOperand
    {
      return forwardedOp.value.lookThroughForwardingInstructions
    }
    return self
  }
}

//===----------------------------------------------------------------------===//
//                               Unit Tests
//===----------------------------------------------------------------------===//

let extendBorrowScopeTest = FunctionTest("extend_borrow_scope") {
    function, arguments, context in

  let borrowValue = arguments.takeValue()
  let rangeBegin = arguments.takeValue() as! SingleValueInstruction
  let expectedResult = arguments.takeBool()

  var range = InstructionRange(begin: rangeBegin, ends: rangeBegin.users, context)
  defer { range.deinitialize() }

  let result = extendBorrowScope(of: borrowValue, toOverlap: range, context)
  precondition(result == expectedResult)
}
