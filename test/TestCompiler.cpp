

#include "../src/compiler.h"
#include "../src/debug.h"
#include <gtest/gtest.h>
#include <string>

TEST(TestCompiler, TestEmpty) {
  std::string source = "";
  Compiler *compiler = compile(source);

  EXPECT_EQ(compiler->type, TYPE_SCRIPT);
  EXPECT_EQ(compiler->locals.size(), 0);
  EXPECT_EQ(compiler->scopeDepth, 0);
  EXPECT_EQ(compiler->function->chunk->cp, 2);
  EXPECT_EQ(compiler->function->chunk->code[0], (uint8_t)11);
  EXPECT_EQ(compiler->function->chunk->code[1], (uint8_t)31);
}
