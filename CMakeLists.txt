cmake_minimum_required(VERSION 3.20)
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
  GIT_REPOSITORY https://github.com/google/googletest.git 
  GIT_TAG f8d7d77c06936315286eb55f8de22cd23c188571 # release 1.14
)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)


set(Sources
  ./src/common.h
  ./src/compiler.h
  ./src/compiler.cpp
  ./src/debug.cpp
  ./src/debug.h
  ./src/expr.h
  ./src/llvm.h
  ./src/scanner.cpp
  ./src/scanner.h
  ./src/stmt.h
  ./src/trie.h
  )

add_library(${This} STATIC ${Sources})
