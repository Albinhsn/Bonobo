cmake_minimum_required(VERSION 3.14)
include(GoogleTest)

set(ThisAst AstTests)
set(ThisScanner ScannerTests)

set(testAst_SOURCES
  ./testCommon.h
  ./testCommon.cpp
  ./TestAst.cpp
  ./TestArray.cpp
  ./TestBinary.cpp
  ./TestComparison.cpp
  ./TestFor.cpp
  ./TestFun.cpp
  ./TestLogical.cpp
  ./TestIf.cpp
  ./TestIndex.cpp
  ./TestMap.cpp
  ./TestStruct.cpp
  ./TestWhile.cpp
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
