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
  
  public enum CallbackResult {
    case ignore
    case continueWalking
    case markEscaping
  }
  
  typealias UseCallback = (Operand, Path) -> CallbackResult
  typealias ArgCallback = (FunctionArgument, Path, Bool) -> CallbackResult

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
  }

   mutating
   func isEscaping(_ value: Value,
                   projection: Path = Path(),
                   followStores: Bool = false,
                   visitUse: UseCallback = { _, _ in .continueWalking },
                   visitArg: ArgCallback = { _, _, _ in .markEscaping }) -> Bool {
    start()
    let result = walkDownAndCache(value, path: projection,
                                  followStores: followStores,
                                  visitUse: visitUse, visitArg: visitArg)
    cleanup()
    return result
  }

   mutating
   func isDefEscaping(of value: Value,
                      projection: Path,
                      followStores: Bool = false,
                      visitUse: UseCallback = { _, _ in .continueWalking },
                      visitArg: ArgCallback = { _, _, _ in .markEscaping }) -> Bool {
    start()
    let result = walkUpAndCache(value, path: projection,
                                followStores: followStores,
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
  func walkDownAndCache(_ value: Value, path: Path, followStores: Bool,
                        visitUse: UseCallback, visitArg: ArgCallback) -> Bool {
    if let entry = walkedDownCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkDown(value, path: entry.path, followStores: entry.followStores,
                      visitUse: visitUse, visitArg: visitArg)
    }
    return false
  }

  private mutating
  func walkUpAndCache(_ value: Value, path: Path, followStores: Bool,
                      visitUse: UseCallback, visitArg: ArgCallback) -> Bool {
    if let entry = walkedUpCache[value.hashable, default: CacheEntry()].needWalk(path: path, followStores: followStores) {
      return walkUp(value, path: entry.path, followStores: entry.followStores,
                    visitUse: visitUse, visitArg: visitArg)
    }
    return false
  }

  private mutating
  func walkDown(_ value: Value, path: Path, followStores: Bool,
                visitUse: UseCallback, visitArg: ArgCallback) -> Bool {
    for use in value.uses {
      if use.isTypeDependent { continue}
    
      switch visitUse(use, path) {
        case .ignore: continue
        case .continueWalking: break
        case .markEscaping: return isEscaping
      }

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
          } else {
            if followStores {
              assert(use == store.destinationOperand)
              if walkUp(store.source, path: path, followStores: followStores,
                        visitUse: visitUse, visitArg: visitArg) {
                return true
              }
            }
            if store.destinationOwnership == .assign {
              if let _ = path.popAllValueFields().popIfMatches(.anyClassField) {
                return isEscaping
              }
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
          if let _ = path.popAllValueFields().popIfMatches(.anyClassField) {
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
                                  visitUse: UseCallback,
                                  visitArg: ArgCallback) -> Bool {
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
                             visitUse: UseCallback, visitArg: ArgCallback) -> Bool {
    guard let argIdx = apply.argumentIndex(of: argOp) else {
      // The callee or a type dependend operand does not let escape anything.
      return false
    }
    
    let callees = calleeAnalysis.getCallees(callee: apply.callee)
    if !callees.allCalleesKnown {
      return isEscaping
    }

    for callee in callees {
      if handleArgument(argIdx: argIdx, argPath: path, followStores: followStores,
                        apply: apply, callee: callee,
                        visitUse: visitUse, visitArg: visitArg) {
        return true
      }
    }
    return false
  }
  
  private mutating
  func handleArgument(argIdx: Int, argPath: Path, followStores: Bool,
                      apply: FullApplySite, callee: Function,
                      visitUse: UseCallback, visitArg: ArgCallback) -> Bool {
    for effect in callee.effects {
      switch effect.kind {
        case .notEscaping(let argInfo):
          if followStores { return isEscaping }
          if argInfo.matches(argIndex: argIdx, argPath) {
            return false
          }
        case .escaping(let from, let to, let exclusive):
          if from.matches(argIndex: argIdx, argPath) {
            if followStores && !exclusive {
              // We don't know if something is stored to an argument.
              return isEscaping
            }

            if to.isForReturn {
              guard let result = apply.singleDirectResult else { return isEscaping }
              
              return walkDown(result,
                              path: to.projection, followStores: followStores,
                              visitUse: visitUse, visitArg: visitArg)
            } else {
              return walkUp(apply.arguments[to.argIndex],
                            path: to.projection, followStores: followStores,
                            visitUse: visitUse, visitArg: visitArg)
            
            }
          } else if exclusive && to.matches(argIndex: argIdx, argPath) {
              return walkUp(apply.arguments[from.argIndex],
                            path: from.projection, followStores: followStores,
                            visitUse: visitUse, visitArg: visitArg)
          }
        default:
          break
      }
    }
    return isEscaping
  }

  private mutating
  func walkUp(_ value: Value, path: Path, followStores: Bool,
              visitUse: UseCallback, visitArg: ArgCallback) -> Bool {
    var val = value
    var p = path
    var fSt = followStores
    while true {
      switch val {
        case is AllocRefInst, is AllocStackInst:
          return walkDownAndCache(val, path: p, followStores: fSt,
                                  visitUse: visitUse, visitArg: visitArg)
        case let arg as FunctionArgument:
          switch visitArg(arg, p, followStores) {
            case .ignore:       return false
            case .markEscaping: return isEscaping
            case .continueWalking:
              return walkDownAndCache(arg, path: p, followStores: fSt,
                                      visitUse: visitUse, visitArg: visitArg)
          }
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
            case let ta as TryApplyInst:
              if block != ta.normalBlock { return isEscaping }
              return walkUpApplyResult(apply: ta,
                                       path: p, followStores: followStores,
                                       visitUse: visitUse, visitArg: visitArg)
            default:
              return isEscaping
          }
        case let ap as ApplyInst:
          return walkUpApplyResult(apply: ap,
                                   path: p, followStores: followStores,
                                   visitUse: visitUse, visitArg: visitArg)
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
          guard let op = e.operand else { return false }
          p = newPath
          val = op
        case let ued as UncheckedEnumDataInst:
          val = ued.operand
          p = p.push(.enumCase, index: ued.caseIndex)
        case is UpcastInst, is UncheckedRefCastInst,
             is InitExistentialRefInst, is OpenExistentialRefInst,
             is InitExistentialAddrInst, is OpenExistentialAddrInst,
             is BeginAccessInst, is BeginBorrowInst,
             is EndCOWMutationInst, is BridgeObjectToRefInst,
             is IndexAddrInst, is CopyValueInst:
          val = (val as! Instruction).operands[0].value
        case let mvr as MultipleValueInstructionResult:
          let inst = mvr.instruction
          switch inst {
            case is DestructureStructInst, is DestructureTupleInst:
              val = inst.operands[0].value
              // TODO: only push the specific result index.
              // But currently there is no O(0) method to get the result index
              // from a result value.
              p = p.popAllValueFields().push(.anyValueFields)
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
 
  private mutating
  func walkUpApplyResult(apply: FullApplySite,
                         path: Path, followStores: Bool,
                         visitUse: UseCallback, visitArg: ArgCallback) -> Bool {
    if followStores {
      // We don't know if something is stored to an argument.
      return isEscaping
    }

    let callees = calleeAnalysis.getCallees(callee: apply.callee)
    if !callees.allCalleesKnown {
      return isEscaping
    }

    for callee in callees {
      for effect in callee.effects {
        switch effect.kind {
          case .escaping(let from, let to, let exclusive):
            if exclusive && to.matchesReturn(path) {
              return walkUp(apply.arguments[from.argIndex],
                            path: from.projection, followStores: false,
                            visitUse: visitUse, visitArg: visitArg)
            }
          default:
            break
        }
      }
      return isEscaping
    }
    return false
  }

  // For convenient setting a breakpoing to whatever lets something escape.
  private var isEscaping: Bool {
    true
  }
}
