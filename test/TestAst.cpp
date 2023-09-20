#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestBinaryOp, TestIntVar) {
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

TEST(TestBinaryOp, TestAddOp) {
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

TEST(TestBinaryOp, TestDivOp) {
  std::string source = "var a: int = 5 / 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

  BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
  EXPECT_EQ(binaryExpr->op, DIV);
}

TEST(TestBinaryOp, TestMulOp) {
  std::string source = "var a: int = 5 * 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

  BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
  EXPECT_EQ(binaryExpr->op, MUL);
}

TEST(TestComparisonOp, TestLess) {
  std::string source = "var a: int = 5 < 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

  ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
  EXPECT_EQ(expr->op, LESS_COMPARISON);
  EXPECT_EQ(expr->left->type, LITERAL_EXPR);
  EXPECT_EQ(expr->right->type, LITERAL_EXPR);
}

TEST(TestComparisonOp, TestLessEqual) {
  std::string source = "var a: int = 5 <= 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

  ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
  EXPECT_EQ(expr->op, LESS_EQUAL_COMPARISON);
}

TEST(TestComparisonOp, TestGreater) {
  std::string source = "var a: int = 5 > 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

  ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
  EXPECT_EQ(expr->op, GREATER_COMPARISON);
}

TEST(TestComparisonOp, TestGreaterEqual) {
  std::string source = "var a: int = 5 >= 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

  ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
  EXPECT_EQ(expr->op, GREATER_EQUAL_COMPARISON);
}

TEST(TestComparisonOp, TestEqualEqual) {
  std::string source = "var a: int = 5 == 3;";

  std::vector<Stmt *> result = compile(source.c_str());

  VarStmt *varStmt = (VarStmt *)result[0];
  EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

  ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
  EXPECT_EQ(expr->op, EQUAL_EQUAL_COMPARISON);
}
