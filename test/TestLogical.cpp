#include "../src/vm.h"

#include <gtest/gtest.h>

TEST(TestVM, TestFalse) {
  std::string source = "print false;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestVM, TestLess) {
  std::string source = "print 5 < 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestVM, TestLessEqual) {
  std::string source = "print 5 <= 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestVM, TestEqual) {
  std::string source = "print 5 == 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestVM, TestGreater) {
  std::string source = "print 5 > 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestVM, TestGreaterEqual) {
  std::string source = "print 5 >= 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestVM, TestBang) {
  std::string source = "print !nil;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestVM, TestBangEqual) {
  std::string source = "print nil != nil;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestVM, TestLogicalBinary) {
  std::string source = "print 5 * 2 < 3 * 5;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestVM, TestLogicalBinary2) {
  std::string source = "print 5 + 5 * 2 < 3 * 5;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}
