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
    EXPECT_EQ(std::string(varStmt->var->name.lexeme, varStmt->var->name.length),
              "a");
    EXPECT_EQ(varStmt->initializer->type, LITERAL_EXPR);

    LiteralExpr *literalExpr = (LiteralExpr *)varStmt->initializer;
    EXPECT_EQ(literalExpr->literalType, INT_LITERAL);
    EXPECT_EQ(
        std::string(literalExpr->literal.lexeme, literalExpr->literal.length),
        "5");
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
