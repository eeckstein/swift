// RUN: %target-swift-emit-silgen -parse-stdlib %s | %FileCheck %s

//CHECK: [readonly] [ossa] @func1
@_effects(readonly) @_silgen_name("func1") func func1() { }

//CHECK: [readnone] [ossa] @func2
@_effects(readnone) @_silgen_name("func2") func func2() { }

//CHECK: [readwrite] [ossa] @func3
@_effects(readwrite) @_silgen_name("func3") func func3() { }

//CHECK: [releasenone] [ossa] @func4
@_effects(releasenone) @_silgen_name("func4") func func4() { }

//CHECK: [noescape(0, **)] [ossa] @func5
@_effects(noescape(0)) @_silgen_name("func5") func func4<T>(_ t: T) { }

//CHECK: [escapes_to_arg(1, *.*, 0), noescape(0, **)] [ossa] @func6
@_effects(escapes_to_return(0, *.*)) @_silgen_name("func6") func func4<T>(_ t: T) -> T { }

//CHECK: [escapes_to_arg(2, *.*, 1, [s0.v*])] [ossa] @func7
@_effects(escapes_to_arg(1, *.*, 0, [s0.v*])) @_silgen_name("func7") func func4<T>(_ t: inout T, _ s: T) -> T { }
