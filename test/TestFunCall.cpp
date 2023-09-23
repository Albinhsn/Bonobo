#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestFunCall, TestEmptyFunCall) {
    std::string source = "foo();";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, EXPR_STMT);

    ExprStmt *exprStmt = (ExprStmt *)result[0];
    EXPECT_EQ(exprStmt->expression->type, CALL_EXPR);

    CallExpr *callExpr = (CallExpr *)exprStmt->expression;

    EXPECT_EQ(callExpr->callee.lexeme, "foo");
    EXPECT_EQ(callExpr->arguments.size(), 0);
}

TEST(TestFunCall, TestParamFunCall) {
    std::string source = "foo(a,b);";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, EXPR_STMT);

    ExprStmt *exprStmt = (ExprStmt *)result[0];
    EXPECT_EQ(exprStmt->expression->type, CALL_EXPR);

    CallExpr *callExpr = (CallExpr *)exprStmt->expression;

    EXPECT_EQ(callExpr->callee.lexeme, "foo");
    EXPECT_EQ(callExpr->arguments.size(), 2);
}
