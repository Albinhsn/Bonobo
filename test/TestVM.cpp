

#include "../src/vm.h"

#include <gtest/gtest.h>

TEST(TestVM, TestPrint) {
  std::string source = "print 5;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "5\n");
}


TEST(TestVM, TestFor) {
  std::string source = "for(var i = 0; i < 5; i = i + 1){print i;}";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "0\n1\n2\n3\n4\n");
}

TEST(TestVM, TestWhile) {
  std::string source = "var i = 0; while(i < 5){print i; i = i + 1;}";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "0\n1\n2\n3\n4\n");
}

TEST(TestVM, TestFun) {
  std::string source = "fun fib(a){if(a <= 2){return 1;} return fib(a-1) + "
                       "fib(a-2);} print fib(5);";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "5\n");
}

TEST(TestVM, TestEmptyReturn) {
  std::string source = "fun fib(a){} print fib(5);";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "nil\n");
}

TEST(TestVM, TestStruct) {
  std::string source = "struct point{x;y;};\n print(point);\n var p = "
                       "point(5,10);\n print(p); print(p.x); print(p.y);";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "point struct\npoint instance\n5\n10\n");
}

TEST(TestVM, TestMap) {
  std::string source = "var m = {\"a\":1, \"b\": 2}; print m; print m[\"a\"]; m[\"c\"] = 5; print m[\"c\"];";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "{'a':1,'b':2}\n1\n5\n");
}

TEST(TestVM, TestArray) {
  std::string source = "var m = [1,2,3]; print m; m[1] = 5; print m[1];";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "[1,2,3]\n5\n");
}
