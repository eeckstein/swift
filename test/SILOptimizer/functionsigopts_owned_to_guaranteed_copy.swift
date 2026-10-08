// RUN: %target-swift-frontend -O -module-name test -emit-sil -primary-file %s | %FileCheck %s

// Check that the copy of the guaranteed argument, which is left in the caller
// after owned-to-guaranteed specialization, is removed.

final class C {
  var x = 0
}

public struct S {
  var a: C
  var b: C
}

@inline(never)
func takeOwned(_ c: __owned C) -> Int {
  return c.x
}

// CHECK-LABEL: sil @$s4test0A11CopiedValueySiAA1SVF :
// CHECK:         [[A:%.*]] = struct_extract %0, #S.a
// CHECK-NOT:     retain
// CHECK:         [[F:%.*]] = function_ref @$s4test9takeOwnedySiAA1CCnFTf4g_n
// CHECK:         apply [[F]]([[A]])
// CHECK-NOT:     release
// CHECK:       } // end sil function '$s4test0A11CopiedValueySiAA1SVF'
public func testCopiedValue(_ s: S) -> Int {
  return takeOwned(s.a)
}
