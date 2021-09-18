//===--- EscapeInfo.swift - the alias analysis -------------------------===//
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

struct EscapeInfo {

  typealias Path = ProjectionPath

  private struct CacheEntry {
    private(set) var path = Path()
    private(set) var followStores = false
    private var valid = false

    mutating func needWalk(path: Path, followStores: Bool) -> CacheEntry? {
      if !valid {
        valid = true
        self.path = path
        self.followStores = followStores
        return self
      }
      if self.path != path || (!self.followStores && followStores) {
        let newFS = self.followStores || followStores
        let newPath = self.path.merge(with: path)
        if newPath != self.path || newFS != self.followStores {
          self.followStores = newFS
          self.path = newPath
          return self
        }
      }
      return nil
    }
  }

  private var walkedDownCache = Dictionary<HashableValue, CacheEntry>()
  private var walkedUpCache = Dictionary<HashableValue, CacheEntry>()
  
  private let calleeAnalysis: CalleeAnalysis
  
  init(calleeAnalysis: CalleeAnalysis) {
    self.calleeAnalysis = calleeAnalysis
    
    // TODO: move this to a separate test file
    Path.unitTest()
  }

  mutating
  func escapes(_ allocRef: AllocRefInst,
               visitUse: (Operand, Path) -> Bool = { _, _ in true },
               visitArg: (FunctionArgument, Path) -> Bool = { _, _ in true }) -> Bool {
    start()
    let result = walkDownAndCache(allocRef,
                              path: Path(), followStores: false,
                              visitUse: visitUse, visitArg: visitArg)
    cleanup()
    return result
  }

   mutating
   func escapes(argument: FunctionArgument,
                pattern: Effect.Pattern,
                visitUse: (Operand, Path) -> Bool = { _, _ in true },
                visitArg: (FunctionArgument, Path) -> Bool = { _, _ in true }) -> Bool {
    start()
    let result = walkDownAndCache(argument,
                              path: Path(pattern: pattern), followStores: false,
                              visitUse: visitUse, visitArg: visitArg)
    cleanup()
    return result
  }

  private func start() {
    assert(walkedDownCache.isEmpty && walkedUpCache.isEmpty)
  }

  private mutating func cleanup() {
    walkedDownCache.removeAll(keepingCapacity: true)
    walkedUpCache.removeAll(keepingCapacity: true)
  }

  private mutating
  func walkDownAndCache(_ value: Value,
                        path: Path, followStores: Bool,
                        visitUse: (Operand, Path) -> Bool,
                        visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    if let entry = walkedDownCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkDown(value, path: entry.path, followStores: entry.followStores,
                      visitUse: visitUse, visitArg: visitArg)
    }
    return false
  }

  private mutating
  func walkUpAndCache(_ value: Value,
                      path: Path, followStores: Bool,
                      visitUse: (Operand, Path) -> Bool,
                      visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    if let entry = walkedUpCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkUp(value, path: entry.path, followStores: entry.followStores,
                    visitUse: visitUse, visitArg: visitArg)
    }
    return false
  }

  private mutating func walkDown(_ value: Value,
                                 path: Path, followStores: Bool,
                                 visitUse: (Operand, Path) -> Bool,
                                 visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    for use in value.uses {
      if use.isTypeDependent { continue}
    
      if !visitUse(use, path) { continue }

      let user = use.instruction
      switch user {
        case let rta as RefTailAddrInst:
          if let newPath = path.popIfMatches(.tailElements) {
            if walkDown(rta, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let rea as RefElementAddrInst:
          if let newPath = path.popIfMatches(.classField, index: rea.fieldIndex) {
            if walkDown(rea, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let str as StructInst:
          if walkDown(str, path: path.push(.structField, index: use.index),
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let se as StructExtractInst:
          if let newPath = path.popIfMatches(.structField, index: se.fieldIndex) {
            if walkDown(se, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let ds as DestructureStructInst:
          if walkDownInstructionResults(results: ds.results,
                fieldKind: .structField, path: path, followStores: followStores,
                visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let dt as DestructureTupleInst:
          if walkDownInstructionResults(results: dt.results,
                fieldKind: .tupleField, path: path, followStores: followStores,
                visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let sea as StructElementAddrInst:
          if let newPath = path.popIfMatches(.structField, index: sea.fieldIndex) {
            if walkDown(sea, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let t as TupleInst:
          if walkDown(t, path: path.push(.tupleField, index: use.index),
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let te as TupleExtractInst:
          if let newPath = path.popIfMatches(.tupleField, index: te.fieldIndex) {
            if walkDown(te, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let tea as TupleElementAddrInst:
          if let newPath = path.popIfMatches(.tupleField, index: tea.fieldIndex) {
            if walkDown(tea, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let e as EnumInst:
          if walkDown(e, path: path.push(.enumCase, index: e.caseIndex),
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let ued as UncheckedEnumDataInst:
          if let newPath = path.popIfMatches(.enumCase, index: ued.caseIndex) {
            if walkDown(ued, path: newPath, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let se as SwitchEnumInst:
          if let (caseIdx, newPath) = path.pop(kind: .enumCase) {
            if let succBlock = se.getUniqueSuccessor(forCaseIndex: caseIdx) {
              if let payload = succBlock.arguments.first {
                if walkDown(payload, path: newPath, followStores: followStores,
                            visitUse: visitUse, visitArg: visitArg) {
                  return true
                }
              }
            }
          } else if path.matchesAnyValueField {
            for succBlock in se.block.successors {
              if let payload = succBlock.arguments.first {
                if walkDown(payload, path: path, followStores: followStores,
                            visitUse: visitUse, visitArg: visitArg) {
                  return true
                }
              }
            }
          } else {
            return isEscaping
          }
        case let store as StoreInst:
          if use == store.sourceOperand {
            if walkUp(store.destination, path: path,
                          followStores: followStores,
                          visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          } else if followStores {
            assert(use == store.destinationOperand)
            if walkUp(store.source, path: path, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case let copyAddr as CopyAddrInst:
          if use == copyAddr.sourceOperand {
            if walkUp(copyAddr.destination, path: path, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          } else if followStores {
            assert(use == copyAddr.destinationOperand)
            if walkUp(copyAddr.source, path: path, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
              return true
            }
          }
        case is DestroyValueInst, is ReleaseValueInst, is StrongReleaseInst,
             is DestroyAddrInst:
          // Destroying cannot escape the value/reference itself, but its
          // contents (in the destructor).
          let p = path.popAllValueFields()
          if let _ = p.popIfMatches(.classField) {
            return isEscaping
          }
        case let br as BranchInst:
          if walkDownAndCache(br.getArgument(for: use), path: path,
                              followStores: followStores,
                              visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case is ReturnInst:
          return isEscaping
        case let ap as ApplyInst:
          if handleArgumentEffects(argOp: use, apply: ap, path: path,
                                   followStores: followStores,
                                   visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let tap as TryApplyInst:
          if handleArgumentEffects(argOp: use, apply: tap, path: path,
                                   followStores: followStores,
                                   visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case is LoadInst,
             is InitExistentialRefInst, is OpenExistentialRefInst,
             is InitExistentialAddrInst, is OpenExistentialAddrInst,
             is BeginAccessInst, is BeginBorrowInst, is CopyValueInst,
             is UpcastInst, is UncheckedRefCastInst, is EndCOWMutationInst,
             is PointerToAddressInst, is IndexAddrInst, is BridgeObjectToRefInst:
          if walkDown(user as! SingleValueInstruction, path: path,
                      followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case let bcm as BeginCOWMutationInst:
          if walkDown(bcm.bufferResult, path: path, followStores: followStores,
                          visitUse: visitUse, visitArg: visitArg) {
            return true
          }
        case is DeallocStackInst, is StrongRetainInst, is RetainValueInst,
             is DebugValueInst, is ValueMetatypeInst,
             is InitExistentialMetatypeInst, is OpenExistentialMetatypeInst,
             is ExistentialMetatypeInst, is DeallocRefInst, is SetDeallocatingInst,
             is FixLifetimeInst, is ClassifyBridgeObjectInst,
             is EndBorrowInst, is EndAccessInst,
             is StrongRetainInst, is RetainValueInst,
             is ClassMethodInst, is SuperMethodInst, is ObjCMethodInst,
             is ObjCSuperMethodInst, is WitnessMethodInst:
          break
        default:
          return isEscaping
      }
    }
    return false
  }
  
  private mutating
  func walkDownInstructionResults(results: Instruction.Results,
                                  fieldKind: Path.FieldKind,
                                  path: Path, followStores: Bool,
                                  visitUse: (Operand, Path) -> Bool,
                                  visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    if let (index, newPath) = path.pop(kind: fieldKind) {
      return walkDown(results[index], path: newPath, followStores: followStores,
                      visitUse: visitUse, visitArg: visitArg)
    }
    if path.matchesAnyValueField {
      for elem in results {
        if walkDown(elem, path: path, followStores: followStores,
                    visitUse: visitUse, visitArg: visitArg) {
          return true
        }
      }
      return false
    }
    return isEscaping
  }

  private mutating
  func handleArgumentEffects(argOp: Operand, apply: FullApplySite,
                             path: Path, followStores: Bool,
                             visitUse: (Operand, Path) -> Bool,
                             visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    guard let argIdx = apply.argumentIndex(of: argOp) else {
      // The callee or a type dependend operand does not let escape anything.
      return false
    }
    
    if followStores {
      // We don't know if something is stored to an argument.
      return isEscaping
    }
  
    let callees = calleeAnalysis.getCallees(callee: apply.callee)
    if !callees.allCalleesKnown {
      return isEscaping
    }

    for callee in callees {
      if handleArgument(argIdx: argIdx, argPath: path, apply: apply,
                        callee: callee, visitUse: visitUse, visitArg: visitArg) {
        return true
      }
    }
    return false
  }
  
  private mutating
  func handleArgument(argIdx: Int, argPath: Path,
                      apply: FullApplySite, callee: Function,
                      visitUse: (Operand, Path) -> Bool,
                      visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    for effect in callee.effects {
      switch effect.kind {
        case .notEscaping(let argInfo):
          if argInfo.matches(argIdx, argPath) {
            return false
          }
        case .escapesToReturn(let argInfo, let returnPath):
          if argInfo.matches(argIdx, argPath) {
            if let result = apply.singleDirectResult {
              return walkDown(result,
                              path: returnPath, followStores: false,
                              visitUse: visitUse, visitArg: visitArg)
            }
            return isEscaping
          }
        case .escapesToArgument(let argInfo, let destArgIdx, let destArgPath):
          if argInfo.matches(argIdx, argPath) {
            return walkUp(apply.arguments[destArgIdx],
                          path: destArgPath, followStores: false,
                          visitUse: visitUse, visitArg: visitArg)
          }
        default:
          break
      }
    }
    return isEscaping
  }

  private mutating func walkUp(_ value: Value,
                               path: Path, followStores: Bool,
                               visitUse: (Operand, Path) -> Bool,
                               visitArg: (FunctionArgument, Path) -> Bool) -> Bool {
    var val = value
    var p = path
    var fSt = followStores
    while true {
      switch val {
        case is AllocRefInst, is AllocStackInst:
          return walkDownAndCache(val, path: p, followStores: fSt,
                                  visitUse: visitUse, visitArg: visitArg)
        case let arg as FunctionArgument:
          if visitArg(arg, p) {
            return isEscaping
          }
          return walkDownAndCache(arg, path: p, followStores: fSt,
                                  visitUse: visitUse, visitArg: visitArg)
        case let arg as BlockArgument:
          if arg.isPhiArgument {
            for incoming in arg.incomingPhiValues {
              if walkUpAndCache(incoming, path: p, followStores: fSt,
                                 visitUse: visitUse, visitArg: visitArg) {
                return true
              }
            }
            return false
          }
          let block = arg.block
          switch block.singlePredecessor!.terminator {
            case let se as SwitchEnumInst:
              guard let caseIdx = se.getUniqueCase(forSuccessor: block) else {
                return isEscaping
              }
              val = se.enumOp
              p = p.push(.enumCase, index: caseIdx)
            default:
              return isEscaping
          }
        case let str as StructInst:
          guard let (structField, poppedPath) = p.pop(kind: .structField) else {
            // TODO: handle anyValueField by iterating over all operands
            return isEscaping
          }
          val = str.operands[structField].value
          p = poppedPath
        case let se as StructExtractInst:
          val = se.operand
          p = p.push(.structField, index: se.fieldIndex)
        case let t as TupleInst:
          guard let (tupleField, poppedPath) = p.pop(kind: .tupleField) else {
            // TODO: handle anyValueField by iterating over all operands
            return isEscaping
          }
          val = t.operands[tupleField].value
          p = poppedPath
        case let te as TupleExtractInst:
          val = te.operand
          p = p.push(.tupleField, index: te.fieldIndex)
        case let load as LoadInst:
          val = load.operand
          // When the value is loaded from somewhere it also matters what's
          // stored to that memory location.
          fSt = true
        case let rta as RefTailAddrInst:
          val = rta.operand
          p = p.push(.tailElements)
        case let rea as RefElementAddrInst:
          val = rea.operand
          p = p.push(.classField, index: rea.fieldIndex)
        case let sea as StructElementAddrInst:
          p = p.push(.structField, index: sea.fieldIndex)
          val = sea.operand
        case let tea as TupleElementAddrInst:
          p = p.push(.tupleField, index: tea.fieldIndex)
          val = tea.operand
        case let e as EnumInst:
          guard let newPath = p.popIfMatches(.enumCase, index: e.caseIndex) else {
            return false
          }
          p = newPath
          val = e.operand
        case let ued as UncheckedEnumDataInst:
          val = ued.operand
          p = p.push(.enumCase, index: ued.caseIndex)
        case is UpcastInst, is UncheckedRefCastInst,
             is InitExistentialRefInst, is OpenExistentialRefInst,
             is InitExistentialAddrInst, is OpenExistentialAddrInst,
             is BeginAccessInst, is BeginBorrowInst,
             is EndCOWMutationInst, is BridgeObjectToRefInst,
             is IndexAddrInst:
          val = (val as! Instruction).operands[0].value
        case let mvr as MultipleValueInstructionResult:
          let inst = mvr.instruction
          switch inst {
            case is DestructureStructInst, is DestructureTupleInst:
              val = inst.operands[0].value
              // TODO: only push the specific result index.
              // But currently there is no O(0) method to get the result index
              // from a result value.
              p = p.popAllValueFields().push(.anyValueField)
            case let bcm as BeginCOWMutationInst:
              val = bcm.operand
            default:
              return isEscaping
          }
        default:
          return isEscaping
      }
    }
  }
  
  // For convenient setting a breakpoing to whatever lets something escape.
  private var isEscaping: Bool {
    true
  }
}
