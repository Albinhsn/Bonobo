#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestIntVariable, TestIntVar) {
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

TEST(TestLogicalOp, TestAnd) {
    std::string source = "var a: int = 5 and 3;";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, LOGICAL_EXPR);

    LogicalExpr *expr = (LogicalExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, AND_LOGICAL);
    EXPECT_EQ(expr->left->type, LITERAL_EXPR);
    EXPECT_EQ(expr->right->type, LITERAL_EXPR);
}

TEST(TestLogicalOp, TestOr) {
    std::string source = "var a: int = 5 or 3;";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, LOGICAL_EXPR);

    LogicalExpr *expr = (LogicalExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, OR_LOGICAL);
}

TEST(TestUnaryOp, TestBang) {
    std::string source = "var a: int = !5;";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, UNARY_EXPR);

    UnaryExpr *expr = (UnaryExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, BANG_UNARY);
}

TEST(TestGroupingOp, TestGrouping) {
    std::string source = "var a: int = !(5);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, UNARY_EXPR);

    UnaryExpr *expr = (UnaryExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, BANG_UNARY);
    EXPECT_EQ(expr->right->type, GROUPING_EXPR);
    GroupingExpr *groupingExpr = (GroupingExpr *)expr->right;
    EXPECT_EQ(groupingExpr->expression->type, LITERAL_EXPR);
}
