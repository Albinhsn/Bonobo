#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestAst, TestIntVar) {
  std::string source = "var a: int = 5;";

  std::vector<Stmt *> result = compile(source.c_str());
  EXPECT_EQ(result.size(), 1);

  EXPECT_EQ(result[0]->type, VAR_STMT);
  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(std::string(varStmt->name.lexeme, varStmt->name.length), "a");
  EXPECT_EQ(varStmt->initializer->type, LITERAL_EXPR);

  LiteralExpr *literalExpr = (LiteralExpr *)varStmt->initializer;
  EXPECT_EQ(literalExpr->literalType, INT_LITERAL);
  EXPECT_EQ(
      std::string(literalExpr->literal.lexeme, literalExpr->literal.length),
      "5");
}

TEST(TestAst, TestAddOp) {
  std::string source = "var a: int = 5 + 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

  BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
  EXPECT_EQ(binaryExpr->op, ADD);
  EXPECT_EQ(binaryExpr->left->type, LITERAL_EXPR);
  LiteralExpr *left = (LiteralExpr *)binaryExpr->left;
  EXPECT_EQ(left->literalType, INT_LITERAL);
  EXPECT_EQ(std::string(left->literal.lexeme, left->literal.length), "5");

  EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);
  LiteralExpr *right = (LiteralExpr *)binaryExpr->right;
  EXPECT_EQ(right->literalType, INT_LITERAL);
  EXPECT_EQ(std::string(right->literal.lexeme, right->literal.length), "3");
}

TEST(TestAst, TestDivOp) {
  std::string source = "var a: int = 5 / 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

  BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
  EXPECT_EQ(binaryExpr->op, DIV);
}

TEST(TestAst, TestMulOp) {
  std::string source = "var a: int = 5 * 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

  BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
  EXPECT_EQ(binaryExpr->op, MUL);
}
