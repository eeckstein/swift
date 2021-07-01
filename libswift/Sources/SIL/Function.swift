//===--- Function.swift - Defines the Function class ----------------------===//
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

import SILBridging

final public class Function : CustomStringConvertible {
  public private(set) var effects = FunctionEffects()

  public var name: String {
    return SILFunction_getName(bridged).string
  }

  final public var description: String {
    return SILFunction_debugDescription(bridged).takeString()
  }

  public var entryBlock: BasicBlock {
    SILFunction_firstBlock(bridged).block!
  }

  public var blocks : List<BasicBlock> {
    return List(startAt: SILFunction_firstBlock(bridged).block)
  }

  public var reverseBlocks : ReverseList<BasicBlock> {
    return ReverseList(startAt: SILFunction_lastBlock(bridged).block)
  }

  static func register() {
    func checkLayout(_ p: UnsafeMutablePointer<FunctionEffects>,
                     data: UnsafeMutableRawPointer, size: Int) {
      assert(MemoryLayout<FunctionEffects>.size <= size, "wrong FunctionInfo size")
      assert(UnsafeMutableRawPointer(p) == data, "wrong FunctionInfo layout")
    }

    let metatype = unsafeBitCast(Function.self, to: SwiftMetatype.self)
    Function_register(metatype,
      { (f: BridgedFunction, data: UnsafeMutableRawPointer, size: Int) in
        checkLayout(&f.function.effects, data: data, size: size)
        data.initializeMemory(as: FunctionEffects.self, repeating: FunctionEffects(), count: 1)
      },
      { (f: BridgedFunction, data: UnsafeMutableRawPointer, size: Int) in
        checkLayout(&f.function.effects, data: data, size: size)
        data.assumingMemoryBound(to: FunctionEffects.self).deinitialize(count: 1)
      },
      { (f: BridgedFunction, os: BridgedOStream) in
        var s = f.function.effects.description
        if !s.isEmpty {
          s += " "
        }
        s.withBridgedStringRef { OStream_write(os, $0) }
      },
      { (f: BridgedFunction, str: BridgedStringRef) -> Int in
        var parser = StringParser(str.string)
        if f.function.effects.parse(parser: &parser, for: f.function) {
          return 1
        }
        return 0
      }
    )
  }

  public var bridged: BridgedFunction { BridgedFunction(obj: SwiftObject(self)) }
}

// Bridging utilities

extension BridgedFunction {
  public var function: Function { obj.getAs(Function.self) }
}
