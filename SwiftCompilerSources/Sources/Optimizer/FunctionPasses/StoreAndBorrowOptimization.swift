//===--- StoreAndBorrowOptimization.swift ----------------------------------==//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

let storeAndBorrowOptimization = FunctionPass(name: "store-and-borrow-optimization") {
    (function: Function, context: FunctionPassContext) in

  for inst in function.instructions {
    if let ebat = inst as? EndBorrowAndTakeInst {
      optimize(endBorrowAndTake: ebat, context)
    }
  }
}

private func optimize(endBorrowAndTake: EndBorrowAndTakeInst, _ context: FunctionPassContext) {
  var worklist = ValueWorklist(context)
  defer { worklist.deinitialize() }
  worklist.pushIfNotVisited(endBorrowAndTake.borrow)

  var startInsts = InstructionSet(context)
  defer { startInsts.deinitialize() }

  var liverange = InstructionWorklist(context)
  defer { liverange.deinitialize() }

  var storeAndBorrows = Stack<StoreAndBorrowInst>(context)
  defer { storeAndBorrows.deinitialize() }
  var loadBorrows = Stack<LoadBorrowInst>(context)
  defer { loadBorrows.deinitialize() }
  var borrowedFroms = Stack<BorrowedFromInst>(context)
  defer { borrowedFroms.deinitialize() }
  var endBorrowAndTakes = Stack<EndBorrowAndTakeInst>(context)
  defer { endBorrowAndTakes.deinitialize() }

  while let beginBorrow = worklist.pop() {
    switch beginBorrow {
    case let sab as StoreAndBorrowInst:
      storeAndBorrows.append(sab)
      startInsts.insert(sab)
    case let lb as LoadBorrowInst where lb.address.accessBase == endBorrowAndTake.address.accessBase:
      loadBorrows.append(lb)
      startInsts.insert(lb)
    case let bb as BorrowedFromInst:
      borrowedFroms.append(bb)
      for incoming in bb.borrowedPhi.incomingValues {
        switch incoming {
        case is StoreAndBorrowInst, is BorrowedFromInst:
          worklist.pushIfNotVisited(incoming)
        default:
          return
        }
      }
    default:
      fatalError("unexpected value in worklist")
    }

    for endOp in beginBorrow.uses.endingLifetime {
      switch endOp.instruction {
      case let ebat as EndBorrowAndTakeInst:
        liverange.pushPredecessors(of: ebat)
        endBorrowAndTakes.append(ebat)
      case is BranchInst:
        let bf = Phi(using: endOp)!.borrowedFrom!
        worklist.pushIfNotVisited(bf)
      default:
        return
      }
    }
  }

  let aliasAnalysis = context.aliasAnalysis
  let deadEndBlocks = context.deadEndBlocks

  while let inst = liverange.pop() {
    if startInsts.contains(inst) {
      continue
    }
    if inst.next == nil,
       inst.parentBlock.successors.contains(where: { deadEndBlocks.isDeadEnd($0) })
    {
      return
    }
    if inst.mayReadOrWrite(address: endBorrowAndTake.address, aliasAnalysis) {
      return
    }
    liverange.pushPredecessors(of: inst)
  }

  // First, create owned phi arguments for borrowed phis
  for bf in borrowedFroms {
    let guaranteedPhi = bf.borrowedPhi.value
    let targetBlock = guaranteedPhi.parentBlock
    
    // Create a new owned phi argument adjacent to the guaranteed one
    _ = targetBlock.insertPhiArgument(
      atPosition: guaranteedPhi.index + 1,
      type: guaranteedPhi.type,
      ownership: .owned,
      context)
  }

  // Create new borrow scopes for each StoreAndBorrowInst
  for sab in storeAndBorrows {
    // Create a new borrow scope for the source value
    let builder = Builder(before: sab, context)
    let beginBorrow = builder.createBeginBorrow(of: sab.source)
    sab.replace(with: beginBorrow, context)
  }

  for lb in loadBorrows {
    let builder = Builder(before: lb, context)
    let newLoad = builder.createLoad(fromAddress: lb.address, ownership: .take)
    let beginBorrow = builder.createBeginBorrow(of: newLoad)
    lb.replace(with: beginBorrow, context)
  }


  for bf in borrowedFroms {
    for branchOp in bf.borrowedPhi.incomingOperands {
      let branch = branchOp.instruction as! BranchInst

      var newArgs = Array(branch.operands.values)

      let newArg: Value
      switch branchOp.value {
      case let bb as BeginBorrowInst:
        newArg = bb.borrowedValue
      case let bf as BorrowedFromInst:
        newArg = bf.parentBlock.arguments[bf.borrowedPhi.value.index + 1]
      default:
        fatalError("unknown source of phi argument")
      }

      // Insert the owned source value at the position corresponding to the new phi argument
      newArgs.insert(newArg, at: branchOp.index + 1)

      let builder = Builder(before: branch, context)
      builder.createBranch(to: bf.parentBlock, arguments: newArgs)
      context.erase(instruction: branch)
    }
  }

  // Handle remaining end_borrow_and_take instructions that weren't updated above
  for ebat in endBorrowAndTakes {
    let ownedValue: Value
    let borrowedValue: Value
    switch ebat.borrow {
    case let bb as BeginBorrowInst:
      ownedValue = bb.borrowedValue
      borrowedValue = bb
    case let bf as BorrowedFromInst:
      ownedValue = bf.parentBlock.arguments[bf.borrowedPhi.value.index + 1]
      borrowedValue = bf
    default:
      fatalError("unknown source of end_borrow_and_take")
    }
    let builder = Builder(before: ebat, context)
    builder.createEndBorrow(of: borrowedValue)
    ebat.replace(with: ownedValue, context)
  }

  updateBorrowedFrom(for: borrowedFroms.map{ $0.borrowedPhi }, context)
}
