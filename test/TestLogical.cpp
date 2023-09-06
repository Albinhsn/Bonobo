#include "../src/vm.h"

#include <gtest/gtest.h>

TEST(TestLogical, TestFalse) {
  std::string source = "print false;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestLogical, TestLess) {
  std::string source = "print 5 < 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestLogical, TestLessEqual) {
  std::string source = "print 5 <= 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestLogical, TestEqual) {
  std::string source = "print 5 == 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestLogical, TestGreater) {
  std::string source = "print 5 > 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestLogical, TestGreaterEqual) {
  std::string source = "print 5 >= 3;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestLogical, TestBang) {
  std::string source = "print !nil;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestLogical, TestBangEqual) {
  std::string source = "print nil != nil;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}

TEST(TestLogical, TestLogicalBinary) {
  std::string source = "print 5 * 2 < 3 * 5;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "true\n");
}

TEST(TestLogical, TestLogicalBinary2) {
  std::string source = "print 5 + 5 * 2 < 3 * 5;";
  testing::internal::CaptureStdout();
  interpret(source.c_str());
  std::string output = testing::internal::GetCapturedStdout();
  EXPECT_EQ(output, "false\n");
}
