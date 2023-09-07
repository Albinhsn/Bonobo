

#include "../src/compiler.h"
#include "../src/debug.h"
#include <gtest/gtest.h>
#include <string>

TEST(TestCompiler, TestEmpty) {
  std::string source = "";
  Compiler *compiler = compile(source.c_str());

  EXPECT_EQ(compiler->type, TYPE_SCRIPT);
  EXPECT_EQ(compiler->localLen, 0);
  EXPECT_EQ(compiler->scopeDepth, 0);
  EXPECT_EQ(compiler->function->codeP, 2);
  EXPECT_EQ(compiler->function->code[0], (uint16_t)12);
  EXPECT_EQ(compiler->function->code[1], (uint16_t)34);
}
