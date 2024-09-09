// RUN: %target-swift-frontend -Xllvm -sil-print-before=definite-init -emit-sil %s -o /dev/null 2>&1 | %FileCheck --check-prefix=BEFORE %s
// RUN: %target-swift-frontend -Xllvm -sil-print-after=definite-init -emit-sil %s -o /dev/null 2>&1 | %FileCheck --check-prefix=AFTER %s
// RUN: %target-swift-frontend -Xllvm -sil-disable-pass=definite-init -Xllvm -sil-print-pass-name -emit-sil %s -o /dev/null 2>&1 | %FileCheck --check-prefix=DISABLE %s

// BEFORE: function before #{{[0-9]+}}, stage  Mandatory Passes, pass {{[0-9]+}}: definite-init
// AFTER: function after #{{[0-9]+}}, stage  Mandatory Passes, pass {{[0-9]+}}: definite-init
// DISABLE: (Disabled) #{{[0-9]+}}, stage  Mandatory Passes, pass {{[0-9]+}}: definite-init
func foo() {}
