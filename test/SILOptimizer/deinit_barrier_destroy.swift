// RUN: %target-run-simple-swift(-O) | %FileCheck %s

// REQUIRES: executable_test

// https://github.com/swiftlang/swift/issues/93240
// The lifetime of a lexical value must not be shortened to end before a destroy whose
// de-initializer reads the value through an unowned reference.

final class C {
  var x: Int

  init?(_ fail: Bool) {
    x = 42
    let token = NCToken(self)
    if fail { return nil }
    borrowToken(token)
  }

  init() {
    x = 27
  }
}

struct NCToken: ~Copyable {
  unowned var owner: C
  init(_ owner: C) { self.owner = owner }
  deinit { print("NCToken", owner.x) }
}

final class ClassToken {
  unowned var owner: C
  init(_ owner: C) { self.owner = owner }
  deinit { print("ClassToken", owner.x) }
}

@inline(never)
func borrowToken(_ x: borrowing NCToken) {}

@inline(never)
func useToken(_ x: ClassToken) {}

@inline(never)
func testNoncopyableToken(_ b: Bool) {
  let c = C()
  let token = NCToken(c)
  if b { return }
  borrowToken(token)
}

@inline(never)
func testClassToken(_ b: Bool) {
  let c = C()
  let token = ClassToken(c)
  if b { return }
  useToken(token)
}

// CHECK:      NCToken 42
// CHECK-NEXT: nil
print(C(true) as Any)

// CHECK-NEXT: NCToken 27
testNoncopyableToken(true)

// CHECK-NEXT: ClassToken 27
testClassToken(true)
