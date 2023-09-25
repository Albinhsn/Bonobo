#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include "../src/llvm.h"
#include "./testCommon.h"
#include <gtest/gtest.h>


TEST(TestLogicalOp, TestAnd) {
    std::string source = "var a: bool = true and false; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, LOGICAL_EXPR);

    LogicalExpr *expr = (LogicalExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, AND_LOGICAL);
    EXPECT_EQ(expr->left->type, LITERAL_EXPR);
    EXPECT_EQ(expr->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result); 
    EXPECT_EQ(resultTxt, "0");
}

TEST(TestLogicalOp, TestOr) {
    std::string source = "var a: bool = true or false; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, LOGICAL_EXPR);

    LogicalExpr *expr = (LogicalExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, OR_LOGICAL);

    std::string resultTxt = runLLVMBackend(result); 
    EXPECT_EQ(resultTxt, "1");
}
