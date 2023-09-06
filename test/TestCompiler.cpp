

#include "../src/compiler.h"
#include "../src/debug.h"
#include <gtest/gtest.h>
#include <string>

TEST(TestCompiler, TestEmpty) {
  std::string source = "";
  Compiler *compiler = compile(source.c_str());

  EXPECT_EQ(compiler->type, TYPE_SCRIPT);
  EXPECT_EQ(compiler->locals.size(), 0);
  EXPECT_EQ(compiler->scopeDepth, 0);
  EXPECT_EQ(compiler->function->cp, 2);
  EXPECT_EQ(compiler->function->code[0], (uint8_t)12);
  EXPECT_EQ(compiler->function->code[1], (uint8_t)34);
}
