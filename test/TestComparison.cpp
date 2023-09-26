#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/llvm.h"
#include "../src/stmt.h"
#include "./testCommon.h"
#include <gtest/gtest.h>

TEST(TestComparisonOp, TestLess) {
    std::string source = "var a: bool = 5 < 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, LESS_COMPARISON);
    EXPECT_EQ(expr->left->type, LITERAL_EXPR);
    EXPECT_EQ(expr->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "0");
}

TEST(TestComparisonOp, TestLessEqual) {
    std::string source = "var a: bool = 5 <= 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, LESS_EQUAL_COMPARISON);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "0");
}

TEST(TestComparisonOp, TestGreater) {
    std::string source = "var a: bool = 5 > 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, GREATER_COMPARISON);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "1");
}

TEST(TestComparisonOp, TestGreaterEqual) {
    std::string source = "var a: bool = 5 >= 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, GREATER_EQUAL_COMPARISON);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "1");
}

TEST(TestComparisonOp, TestEqualEqual) {
    std::string source = "var a: bool = 5 == 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, EQUAL_EQUAL_COMPARISON);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "0");
}

TEST(TestComparisonOp, TestDoubleIntEquality) {
    std::string source = "var a: bool = 5.0 == 5; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, EQUAL_EQUAL_COMPARISON);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "1");
}

TEST(TestComparisonOp, TestDoubleDoubleLE) {
    std::string source = "var a: bool = 3.45 < 3.45001; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "1");
}

TEST(TestComparisonOp, TestPrecedenceComparison) {
    std::string source = "var a: bool = 5 <= 3 == 2;";
    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, COMPARISON_EXPR);

    ComparisonExpr *expr = (ComparisonExpr *)varStmt->initializer;
    EXPECT_EQ(expr->op, EQUAL_EQUAL_COMPARISON);

    ComparisonExpr *left = (ComparisonExpr *)expr->left;
    EXPECT_EQ(left->op, LESS_EQUAL_COMPARISON);
}
