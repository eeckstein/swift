# Swift implemented in Swift

_Libswift_ is the part of the Swift compiler, which is implemented in Swift.

With _libswift_ it is possible to add SIL optimization passes written in Swift. It allows to gradually migrate the SIL optimizer from C++ to Swift.

## Building

_Libswift_ is a static library and it is built as part of the swift compiler using build-script and CMake.

### CMake integration

_Libswift_ is either built with the Swift package manager or with CMake (see `CMakeLists.txt` in the top-level _libswift_ directory). Currently, the build is done with CMake in case of release-builds or if no package manager is installed on the host system.

The CMake build is done with custom commands and not with CMake's integrated swift support because due to a CMake bug it's not possible to build non top-level static swift libraries.

Debug builds are done with the package manager (if installed), because it works better for incremental builds.

### Build modes

There are two build modes for _libswift_, which can be selected with the `--libswift <mode>` build-script option:

* `disable`: the compiler is not built with _libswift_. In this early stage of development this mode is the default. Right now _libswift_ does not contain any optimizations or features which are not available in the existing optimizer. Therefore a compiler with disabled _libswift_ still behaves as a compiler with enabled _libswift_. This will change soon.
* `hosttools`: _libswift_ is built with a pre-installed swift toolchain, using a `swiftc` which is expected to be in the command search path. This mode is the preferred way to build for local development, because it is the fastest way to build. It requires a 5.3 (or newer) swift toolchain to be installed on the host system.


IMPORTANT: **Whenever a _libswift_ source file is added, removed or renamed, cmake needs to re-run**, e.g. by touching any of the swift's CMakeLists.txt file. Otherwise the build dependencies are not tracked correctly.

Currently the `swift-frontend` and `sil-opt` tools use _libswift_.

Tools, which don't use any optimization passes from _libswift_ don't need to link _libswift_. For example, all tools, which compile Swift source code, but don't optimize it, like SourceKit or lldb, don't need to link _libswift_. As long as `initializeLibSwift()` is not called there is no dependency on _libswift_.

This also means that currently it is not possible to implement mandatory passes in _libswift_, because this would break tools which compile Swift code but don't use _libswift_. When we want to implement mandatory passes in _libswift_ in the future, we'll need to link _libswift_ to all those tools.


