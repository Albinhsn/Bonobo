#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

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

TEST(TestComparisonOp, TestPrecedenceComparison) {
    std::string source = "var a: int = 5 <= 3 == 2;";
    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, EQUAL_EQUAL_COMPARISON);

    ComparisonExpr *left = (ComparisonExpr *)expr->left;
    EXPECT_EQ(left->op, LESS_EQUAL_COMPARISON);
}
