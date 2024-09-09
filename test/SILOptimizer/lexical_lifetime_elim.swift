// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-copy-propagation=false -Xllvm -sil-print-function=s4main11testLexicalyXlyF -module-name=main %s | %FileCheck %s --check-prefixes CHECK,CHECK-NOCOPYPROP
// RUN: %target-swift-frontend -emit-sil -O -parse-as-library -enable-lexical-lifetimes=false -Xllvm -sil-print-function=s4main11testLexicalyXlyF -module-name=main %s | %FileCheck %s --check-prefixes CHECK,CHECK-COPYPROP

// REQUIRES: swift_in_compiler

@inline(never)
func takeGuaranteed(_ a: AnyObject) -> AnyObject {
  return a
}

@_silgen_name("getOwned")
@inline(never)
func getOwned() -> AnyObject

// CHECK-LABEL: // testLexical()
// CHECK: [[A:%.*]] = apply %{{.*}}()
// CHECK: [[B:%.*]] = move_value [lexical] [var_decl] [[A]]
// CHECK: apply %{{.*}}([[B]])
// CHECK: apply
// CHECK: destroy_value [[B]]
// CHECK-LABEL: } // end sil function

// LexicalLifetimeEliminator must strip the [lexical] flag
// before the first round of SemanticARCOpts.

// CHECK-NOT: *** function after {{.*}} semantic-arc-opts

// CHECK-LABEL: *** function after {{.*}} sil-lexical-lifetime-eliminator
// CHECK-LABEL: // testLexical()
// CHECK: [[A:%.*]] = apply %{{.*}}()
// CHECK: [[B:%.*]] = move_value [var_decl] [[A]]
// CHECK: apply %{{.*}}([[B]])
// CHECK: apply
// CHECK: destroy_value [[B]]
// CHECK-LABEL: } // end sil function

// The first round of SemanticARCOpts/CopyPropagation must eliminate the
// redundant move_value that was only needed for a lexical lifetime.

// CHECK-NOCOPYPROP-LABEL: *** function after {{.*}} semantic-arc-opts
// CHECK-COPYPROP-LABEL: *** function after {{.*}} copy-propagation
// CHECK-LABEL: // testLexical()
// CHECK: [[A:%.*]] = apply %{{.*}}()
// CHECK: apply %{{.*}}([[A]])
// CHECK-LABEL: } // end sil function
public func testLexical() -> AnyObject {
  let a = getOwned()
  // Without lexical lifetimes, the lifetime of 'a' ends in between the two calls:
  return takeGuaranteed(takeGuaranteed(a))
}
