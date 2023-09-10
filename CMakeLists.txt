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
  GIT_REPOSITORY https://github.com/google/googletest.git 
  GIT_TAG f8d7d77c06936315286eb55f8de22cd23c188571 # release 1.14
)

set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

set(Sources
  # ./src/common.h
  # ./src/compiler.h
  # ./src/compiler.cpp
  # ./src/debug.cpp
  # ./src/debug.h
  # ./src/main.cpp
  # ./src/memory.cpp
  # ./src/memory.h
  ./src/scanner.cpp
  ./src/scanner.h
  # ./src/table.h
  # ./src/table.cpp
  # ./src/opcode.h
  # ./src/value.cpp
  # ./src/value.h
  # ./src/vm.cpp
  # ./src/vm.h
  # ./src/object.h
  # ./src/object.cpp
  )
# add_executable(
#   main ${Sources}
#   )

add_library(${This} STATIC ${Sources})
