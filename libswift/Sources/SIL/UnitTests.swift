//===--- UnitTests.swift - SIL unit tests ---------------------------------===//
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

@_cdecl("unitTestLibSwift")
public func unitTestLibSwift() {
  testProjectionPath()
}

private func testProjectionPath() {
  func basicPushPop() {
    let p1 = ProjectionPath(.structField, index: 3)
                      .push(.classField, index: 12345678)
    let (k2, i2, p2) = p1.pop()
    assert(k2 == .classField && i2 == 12345678)
    let (k3, i3, p3) = p2.pop()
    assert(k3 == .structField && i3 == 3)
    assert(p3.isEmpty)
    let (k4, i4, _) = p2.push(.enumCase, index: 876).pop()
    assert(k4 == .enumCase && i4 == 876)
    let p5 = ProjectionPath(.anything)
    assert(p5.pop().path.isEmpty)
  }
  
   func testParse(_ pathStr: String, expect: ProjectionPath) {
    var parser = StringParser(pathStr)
    let path = ProjectionPath(parser: &parser)!
    assert(path == expect)
    let str = path.description
    assert(str == pathStr)
  }
 
 func parsing() {
    testParse("*", expect: ProjectionPath(.anyValueFields)
                                    .push(.anyClassField)
                                    .push(.anyValueFields))
    testParse("s3.*.s1", expect: ProjectionPath(.structField, index: 1)
                                       .push(.anyValueFields)
                                       .push(.anyClassField)
                                       .push(.structField, index: 3))
    testParse("2.c*.e6.ct.**", expect: ProjectionPath(.anything)
                                       .push(.tailElements)
                                       .push(.enumCase, index: 6)
                                       .push(.anyClassField)
                                       .push(.tupleField, index: 2))
  }

  func testMerge(_ lhsStr: String, _ rhsStr: String,
                 expect expectStr: String) {
    var lhsParser = StringParser(lhsStr)
    let lhs = ProjectionPath(parser: &lhsParser)!
    var rhsParser = StringParser(rhsStr)
    let rhs = ProjectionPath(parser: &rhsParser)!
    var expectParser = StringParser(expectStr)
    let expect = ProjectionPath(parser: &expectParser)!

    let result = lhs.merge(with: rhs)
    assert(result == expect)
     let result2 = rhs.merge(with: lhs)
    assert(result2 == expect)
  }
 
  func merging() {
    testMerge("ct.s0.e0.v*.c0",
              "ct.s0.e0.v*.c0",
      expect: "ct.s0.e0.v*.c0")

    testMerge("ct.s0.s0.c0",
              "ct.s0.e0.s0.c0",
      expect: "ct.s0.v*.c0")

    testMerge("c1.c0",
              "c0",
      expect: "c*.**")

    testMerge("c2.c1",
              "c2",
      expect: "c2.**")

    testMerge("s3.c0",
              "v*.c0",
      expect: "v*.c0")

    testMerge("c0",
              "s2.c1",
      expect: "v*.c*")

    testMerge("s1.s1.c2",
              "s1.c2",
      expect: "s1.v*.c2")

    testMerge("s1.s0",
              "s2.s0",
      expect: "v*")
  }

  basicPushPop()
  parsing()
  merging()
}
