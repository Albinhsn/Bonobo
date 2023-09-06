cmake_minimum_required(VERSION 3.14)
set(This bonobo)
project(${This})

# GoogleTest requires at least C++14
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

enable_testing()

add_subdirectory(src)
add_subdirectory(test)
include(FetchContent)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

set(Sources
  ./src/scanner.cpp
  ./src/scanner.h
  ./src/debug.cpp
  ./src/debug.h
  ./src/opcode.h
  ./src/memory.cpp
  ./src/memory.h
  ./src/value.cpp
  ./src/value.h
  ./src/vm.cpp
  ./src/vm.h
  ./src/compiler.h
  ./src/compiler.cpp
  ./src/main.cpp
  ./src/common.h
  ./src/object.h
  ./src/object.cpp
  )
add_executable(
  main ${Sources}
  )

add_library(${This} STATIC ${Sources})
