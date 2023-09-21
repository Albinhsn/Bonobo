cmake_minimum_required(VERSION 3.14)
include(GoogleTest)

set(ThisAst AstTests)
set(ThisScanner ScannerTests)

set(testAst_SOURCES
  ./TestComparison.cpp
  ./TestLogical.cpp
  ./TestBinary.cpp
  ./TestAst.cpp
  )
set(testScanner_SOURCES
  ./TestScanner.cpp
  )

add_executable(${ThisAst} ${testAst_SOURCES})
add_executable(${ThisScanner} ${testScanner_SOURCES})

target_link_libraries(${ThisAst} PUBLIC
  gtest_main
  bonobo
)
target_link_libraries(${ThisScanner} PUBLIC
  gtest_main
  bonobo
)


add_test(
  NAME ${ThisAst}
  COMMAND ${ThisAst}
  )

add_test(
  NAME ${ThisScanner}
  COMMAND ${ThisScanner}
  )
