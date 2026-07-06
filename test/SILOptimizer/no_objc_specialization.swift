// RUN: %target-swift-frontend -import-objc-header %S/Inputs/NoObjCSpecialization.h -O -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

final class NoObjCSpecializationImpl<Value>: NSObject, NoObjCSpecialization {

    // CHECK-NOT: @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFToyt_Tg5 : $@convention(objc_method)
    // CHECK-LABEL: sil shared @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFyt_Tg5Tf4dnd_n : $@convention(thin) (Bool) -> @error any Error {
    // CHECK-NOT: @$s22no_objc_specialization24NoObjCSpecializationImplC3foo9withValue7andBoolyyp_SbtKFToyt_Tg5 : $@convention(objc_method)
    @_specialize(where Value == Void)
    func foo(withValue value: Any, andBool bool: Bool) throws {
        if Value.self == Void.self {
            print("Is Void with \(bool)")
        } else {
            print("Is \(value) with \(bool)")
        }
    }
}
