if(SWIFT_BUILD_DYNAMIC_STDLIB AND SWIFT_INCLUDE_TESTS AND NOT SWIFT_TINY)
  add_subdirectory(swift-reflection-test)
endif()
