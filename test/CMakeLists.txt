cmake_minimum_required(VERSION 3.14)
include(GoogleTest)

set(ThisScanner ScannerTests)

set(testScanner_SOURCES
  ./TestScanner.cpp
  )

add_executable(${ThisScanner} ${testScanner_SOURCES})

target_link_libraries(${ThisScanner} PUBLIC
  gtest_main
  bonobo
)


add_test(
  NAME ${ThisScanner}
  COMMAND ${ThisScanner}
  )
