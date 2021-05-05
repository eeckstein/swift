# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

function(add_libswift name module_files_var)
  cmake_parse_arguments(LIBSWIFT
                        ""
                        "SWIFT_EXEC;BUILD_DIR"
                        ""
                        ${ARGN})

  set(libswiftModules "SIL" "Optimizer")
  find_program(swiftpm_executable "swift-build")

  # It would be nice if we could use swiftpm for all builds. But this is
  # currently not possible. Mainly because we cannot assume that a swift
  # toolchain is installed on the host system.
  if(swiftpm_executable AND CMAKE_BUILD_TYPE STREQUAL "Debug")

    if(CMAKE_BUILD_TYPE STREQUAL Debug)
      set(configuration "debug")
    else()
      set(configuration "release")
    endif()

    set(build_dir "${LIBSWIFT_BUILD_DIR}/${configuration}")
    file(GLOB_RECURSE sources "*.swift" "${CMAKE_SOURCE_DIR}/include/swift/*Bridging.h")

    # Compile the libswift package using swiftpm.
    #
    # Explicitly add dependencies, so that the "cmake" ninja will trigger
    # swiftpm if any of the libswift files changed.

    if(NOT LIBSWIFT_SWIFT_EXEC STREQUAL "swiftc")
      set(env_opts "env" "SWIFT_EXEC=${LIBSWIFT_SWIFT_EXEC}")
    endif()

    # Use the installed swift tools.
    # TODO: maybe not hardcode "swift", but use a CMAKE variable?
    add_custom_command(OUTPUT "${build_dir}/libSwift.a"
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      DEPENDS ${sources}
      COMMAND ${CMAKE_COMMAND} "-E" ${env_opts}
        "swift" "build" "-c" "${configuration}" "--build-path" "${LIBSWIFT_BUILD_DIR}"
      COMMENT "Building libswift")

    # We need a target to "link" the custom command with the library.
    add_custom_target("${name}-swiftpm" DEPENDS "${build_dir}/libSwift.a")

    # Import the built library 
    add_library(${name} STATIC IMPORTED GLOBAL)
    add_dependencies(${name} "${name}-swiftpm")
    set_target_properties(${name}
        PROPERTIES
        IMPORTED_LOCATION "${build_dir}/libSwift.a")
  else()

    # Build with cmake commands instead of using swiftpm.

    if(CMAKE_BUILD_TYPE STREQUAL Debug)
      set(libswift_compile_options "-g")
    else()
      set(libswift_compile_options "-O" "-cross-module-optimization")
    endif()

    set(build_dir ${LIBSWIFT_BUILD_DIR})

    # Due to a cmake bug it's not possible to use cmake's builtin swift support
    # for building. There is a problem with static swift libraries which are
    # not in a top-level project.
    # Therefore we invoke the swift build commands explicitly to create the
    # libswift library. Fortunately this is not too complicated.
    file(MAKE_DIRECTORY ${build_dir})
    set(Optimizer_dependencies "${build_dir}/SIL.o")

    if(SWIFT_HOST_VARIANT_SDK IN_LIST SWIFT_APPLE_PLATFORMS)
      set(deployment_version "${SWIFT_SDK_${SWIFT_HOST_VARIANT_SDK}_DEPLOYMENT_VERSION}")
    endif()
    get_versioned_target_triple(target ${SWIFT_HOST_VARIANT_SDK}
        ${SWIFT_HOST_VARIANT_ARCH} "${deployment_version}")

    foreach(module ${libswiftModules})

      file(GLOB_RECURSE module_sources "Sources/${module}/*.swift")
   
      set(module_obj_file "${build_dir}/${module}.o")
      set(all_obj_files ${all_obj_files} ${module_obj_file})

      # Compile the libswift module into an object file
      add_custom_command(OUTPUT ${module_obj_file}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${module_sources} ${${module}_dependencies}
        COMMAND ${LIBSWIFT_SWIFT_EXEC} "-c" "-o" ${module_obj_file}
                "-target" ${target}
                "-module-name" ${module} "-emit-module"
                "-emit-module-path" "${build_dir}/${module}.swiftmodule"
                "-parse-as-library" ${module_sources}
                "-wmo" ${libswift_compile_options}
                "-I" "${CMAKE_SOURCE_DIR}/include/swift"
                "-I" "${build_dir}"
        COMMENT "Building libswift module ${module}")

    endforeach()

    # Create a static libswift library containing all module object files.
    add_library(${name} STATIC ${all_obj_files})
    set_target_properties(${name} PROPERTIES LINKER_LANGUAGE CXX)
  endif()

  set(module_files "")
  foreach(module ${libswiftModules})
    list(APPEND module_files "${build_dir}/${module}.swiftmodule")
  endforeach()
  set("${module_files_var}" ${module_files} PARENT_SCOPE)

endfunction()


# A dummy libswift if libswift is disabled

add_swift_host_library(libswiftStub STATIC DisabledLibswiftStub.c)

if(${LIBSWIFT_BUILD_MODE} STREQUAL "DISABLE")

  add_library(libswift ALIAS libswiftStub)

elseif(${LIBSWIFT_BUILD_MODE} STREQUAL "HOSTTOOLS")

  add_libswift(libswift libswift_modules
      SWIFT_EXEC ${HOST_SWIFT_EXEC}
      BUILD_DIR "${CMAKE_BINARY_DIR}/libswift")

elseif(${LIBSWIFT_BUILD_MODE} STREQUAL "BOOTSTRAPPING" OR
       ${LIBSWIFT_BUILD_MODE} STREQUAL "BOOTSTRAPPING-WITH-HOSTLIBS")

  # Bootstrapping - stage 1, using the compiler from level 0

  add_libswift(libswift-bootstrapping1 unused_var
      SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping0/bin/swiftc"
      BUILD_DIR "${CMAKE_BINARY_DIR}/bootstrapping1/libswift")

  add_dependencies(libswift-bootstrapping1 bootstrapping0-all)

  # The final build, using the compiler from stage 1

  add_libswift(libswift libswift_modules
      SWIFT_EXEC "${CMAKE_BINARY_DIR}/bootstrapping1/bin/swiftc"
      BUILD_DIR "${CMAKE_BINARY_DIR}/libswift")

  add_dependencies(libswift bootstrapping1-all)

else()
  message(FATAL_ERROR "Unknown LIBSWIFT_BUILD_MODE '${LIBSWIFT_BUILD_MODE}'")
endif()

set(LIBSWIFT_MODULE_FILES ${libswift_modules} PARENT_SCOPE)

