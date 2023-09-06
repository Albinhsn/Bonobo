cmake_minimum_required(VERSION 3.14)
set(This bonoboTests)

set(test_SOURCES
  ./TestScanner.cpp
  ./TestLogical.cpp
  # ./TestVM.cpp
  ./TestCompiler.cpp
  )
add_executable(${This} ${test_SOURCES})

target_link_libraries(${This} PUBLIC
  gtest_main
  bonobo
)

include(GoogleTest)

add_test(
  NAME ${This}
  COMMAND ${This}
  )
