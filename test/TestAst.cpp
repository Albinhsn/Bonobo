#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/llvm.h"
#include "../src/stmt.h"
#include "./testCommon.h"
#include <gtest/gtest.h>

TEST(TestIntVariable, TestIntVar) {
    std::string source = "var a: int = 5; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->var->name.lexeme, "a");
    EXPECT_EQ(varStmt->initializer->type, LITERAL_EXPR);

    LiteralExpr *literalExpr = (LiteralExpr *)varStmt->initializer;
    EXPECT_EQ(literalExpr->literalType, INT_LITERAL);
    EXPECT_EQ(literalExpr->literal.lexeme, "5");

    std::string resultTxt = runLLVMBackend(result); 
    EXPECT_EQ(resultTxt, "5");
}

TEST(TestUnaryOp, TestBang) {
    std::string source = "var a: bool = !true; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, UNARY_EXPR);

    UnaryExpr *expr = (UnaryExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, BANG_UNARY);

    std::string resultTxt = runLLVMBackend(result); 
    EXPECT_EQ(resultTxt, "0");
}

TEST(TestGroupingOp, TestGrouping) {
    std::string source = "var a: bool = !(false); printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, UNARY_EXPR);

    UnaryExpr *expr = (UnaryExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, BANG_UNARY);
    EXPECT_EQ(expr->right->type, GROUPING_EXPR);
    GroupingExpr *groupingExpr = (GroupingExpr *)expr->right;
    EXPECT_EQ(groupingExpr->expression->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result); 
    EXPECT_EQ(resultTxt, "1");
}
