
#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/llvm.h"
#include "../src/stmt.h"
#include "testCommon.h"
#include <gtest/gtest.h>

TEST(TestBinaryOp, TestIntAddOp) {
    std::string source = "var a: int = 5 + 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, ADD);
    EXPECT_EQ(binaryExpr->left->type, LITERAL_EXPR);

    LiteralExpr *left = (LiteralExpr *)binaryExpr->left;
    EXPECT_EQ(left->literalType, INT_LITERAL);
    EXPECT_EQ(left->literal.lexeme, "5");

    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    LiteralExpr *right = (LiteralExpr *)binaryExpr->right;
    EXPECT_EQ(right->literalType, INT_LITERAL);
    EXPECT_EQ(right->literal.lexeme, "3");

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "8");
}

TEST(TestBinaryOp, TestIntDivOp) {
    std::string source = "var a: int = 5 / 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, DIV);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "1");
}

TEST(TestBinaryOp, TestIntMulOp) {
    std::string source = "var a: int = 5 * 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, MUL);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "15");
}

TEST(TestBinaryOp, TestIntPrecedenceAddAdd) {
    std::string source = "var a: int = 5 + 3 + 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, ADD);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, ADD);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "10");
}

TEST(TestBinaryOp, TestIntPrecedenceAddSub) {
    std::string source = "var a: int = 5 + 3 - 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, SUB);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, ADD);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "6");
}

TEST(TestBinaryOp, TestIntPrecedenceAddMul) {
    std::string source = "var a: int = 5 + 3 * 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, ADD);
    EXPECT_EQ(binaryExpr->left->type, LITERAL_EXPR);
    EXPECT_EQ(binaryExpr->right->type, BINARY_EXPR);

    BinaryExpr *right = (BinaryExpr *)binaryExpr->right;
    EXPECT_EQ(right->op, MUL);
    EXPECT_EQ(right->left->type, LITERAL_EXPR);
    EXPECT_EQ(right->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "11");
}

TEST(TestBinaryOp, TestIntPrecedenceAddDiv) {
    std::string source = "var a: int = 5 + 3 / 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, ADD);
    EXPECT_EQ(binaryExpr->left->type, LITERAL_EXPR);
    EXPECT_EQ(binaryExpr->right->type, BINARY_EXPR);

    BinaryExpr *right = (BinaryExpr *)binaryExpr->right;
    EXPECT_EQ(right->op, DIV);
    EXPECT_EQ(right->left->type, LITERAL_EXPR);
    EXPECT_EQ(right->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "6");
}

TEST(TestBinaryOp, TestIntPrecedenceSubAdd) {
    std::string source = "var a: int = 5 - 3 + 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, ADD);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, SUB);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "4");
}

TEST(TestBinaryOp, TestIntPrecedenceSubSub) {
    std::string source = "var a: int = 5 - 3 - 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, SUB);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, SUB);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "0");
}

TEST(TestBinaryOp, TestIntPrecedenceSubMul) {
    std::string source = "var a: int = 5 - 3 * 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, SUB);
    EXPECT_EQ(binaryExpr->left->type, LITERAL_EXPR);
    EXPECT_EQ(binaryExpr->right->type, BINARY_EXPR);

    BinaryExpr *right = (BinaryExpr *)binaryExpr->right;
    EXPECT_EQ(right->op, MUL);
    EXPECT_EQ(right->left->type, LITERAL_EXPR);
    EXPECT_EQ(right->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "-1");
}

TEST(TestBinaryOp, TestIntPrecedenceSubDiv) {
    std::string source = "var a: int = 5 - 3 / 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, SUB);
    EXPECT_EQ(binaryExpr->left->type, LITERAL_EXPR);
    EXPECT_EQ(binaryExpr->right->type, BINARY_EXPR);

    BinaryExpr *right = (BinaryExpr *)binaryExpr->right;
    EXPECT_EQ(right->op, DIV);
    EXPECT_EQ(right->left->type, LITERAL_EXPR);
    EXPECT_EQ(right->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "4");
}

TEST(TestBinaryOp, TestIntPrecedenceMulAdd) {
    std::string source = "var a: int = 5 * 3 + 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, ADD);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, MUL);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "17");
}

TEST(TestBinaryOp, TestIntPrecedenceMulSub) {
    std::string source = "var a: int = 5 * 3 - 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, SUB);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, MUL);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "13");
}

TEST(TestBinaryOp, TestIntPrecedenceMulMul) {
    std::string source = "var a: int = 5 * 3 * 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, MUL);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, MUL);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    LiteralExpr *right = (LiteralExpr *)binaryExpr->right;
    EXPECT_EQ(right->literal.lexeme, "2");

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "30");
}

TEST(TestBinaryOp, TestPrecedenceMulDiv) {
    std::string source = "var a: int = 5 * 3 / 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, DIV);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, MUL);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    LiteralExpr *right = (LiteralExpr *)binaryExpr->right;
    EXPECT_EQ(right->literal.lexeme, "2");

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "7");
}

TEST(TestBinaryOp, TestIntPrecedenceDivAdd) {
    std::string source = "var a: int = 5 / 3 + 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, ADD);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, DIV);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "3");
}

TEST(TestBinaryOp, TestIntPrecedenceDivSub) {
    std::string source = "var a: int = 5 / 3 - 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, SUB);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, DIV);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "-1");
}

TEST(TestBinaryOp, TestIntPrecedenceDivMul) {
    std::string source = "var a: int = 5 / 3 * 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, MUL);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, DIV);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    LiteralExpr *right = (LiteralExpr *)binaryExpr->right;
    EXPECT_EQ(right->literal.lexeme, "2");

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "2");
}

TEST(TestBinaryOp, TestIntPrecedenceDivDiv) {
    std::string source = "var a: int = 5 / 3 / 2; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, DIV);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, LITERAL_EXPR);

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, DIV);
    EXPECT_EQ(left->left->type, LITERAL_EXPR);
    EXPECT_EQ(left->right->type, LITERAL_EXPR);

    LiteralExpr *right = (LiteralExpr *)binaryExpr->right;
    EXPECT_EQ(right->literal.lexeme, "2");

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "0");
}
TEST(TestBinaryOp, TestDoubleIntDiv) {
    std::string source = "var a: double = 5.0 / 2; printf(\"%lf\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "2.500000");
}

TEST(TestBinaryOp, TestDoubleDoubleDiv) {
    std::string source = "var a: double = 5.0 / 2.0; printf(\"%lf\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "2.500000");
}

TEST(TestBinaryOp, TestDoubleMulMul) {
    std::string source = "var a: double = 1.5 * 2.0; printf(\"%lf\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "3.000000");
}

TEST(TestBinaryOp, TestBinaryOpVar) {
    std::string source = "var a: int = a / b - c;";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, SUB);
    EXPECT_EQ(binaryExpr->left->type, BINARY_EXPR);
    EXPECT_EQ(binaryExpr->right->type, VAR_EXPR);
    LiteralExpr *right = (LiteralExpr *)binaryExpr->right;
    EXPECT_EQ(right->literal.lexeme, "c");

    BinaryExpr *left = (BinaryExpr *)binaryExpr->left;
    EXPECT_EQ(left->op, DIV);
    EXPECT_EQ(left->left->type, VAR_EXPR);
    LiteralExpr *leftLeft = (LiteralExpr *)left->left;
    EXPECT_EQ(leftLeft->literal.lexeme, "a");

    LiteralExpr *leftRight = (LiteralExpr *)left->right;
    EXPECT_EQ(leftRight->literal.lexeme, "b");
    EXPECT_EQ(left->right->type, VAR_EXPR);
}
