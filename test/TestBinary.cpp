
#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/llvm.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

static std::string readFile(const char *path) {
    std::ifstream t(path);
    std::stringstream buffer;
    if (t.fail()) {
        std::cout << "file doesn't exist\n";
        exit(1);
    }
    buffer << t.rdbuf();
    t.close();
    return buffer.str();
}

TEST(TestBinaryOp, TestAddOp) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "8");
}

TEST(TestBinaryOp, TestDivOp) {
    std::string source = "var a: int = 5 / 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, DIV);

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "1");
}

TEST(TestBinaryOp, TestMulOp) {
    std::string source = "var a: int = 5 * 3; printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    VarStmt *varStmt = (VarStmt *)result[0];
    EXPECT_EQ(varStmt->initializer->type, BINARY_EXPR);

    BinaryExpr *binaryExpr = (BinaryExpr *)varStmt->initializer;
    EXPECT_EQ(binaryExpr->op, MUL);

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "15");
}

TEST(TestBinaryOp, TestPrecedenceAddAdd) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "10");
}

TEST(TestBinaryOp, TestPrecedenceAddSub) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "6");
}

TEST(TestBinaryOp, TestPrecedenceAddMul) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "11");
}

TEST(TestBinaryOp, TestPrecedenceAddDiv) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "6");
}

TEST(TestBinaryOp, TestPrecedenceSubAdd) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "4");
}

TEST(TestBinaryOp, TestPrecedenceSubSub) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "0");
}

TEST(TestBinaryOp, TestPrecedenceSubMul) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "1");
}

TEST(TestBinaryOp, TestPrecedenceSubDiv) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "4");
}

TEST(TestBinaryOp, TestPrecedenceMulAdd) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "17");
}

TEST(TestBinaryOp, TestPrecedenceMulSub) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "13");
}

TEST(TestBinaryOp, TestPrecedenceMulMul) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "7");
}

TEST(TestBinaryOp, TestPrecedenceDivAdd) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "3");
}

TEST(TestBinaryOp, TestPrecedenceDivSub) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "-1");
}

TEST(TestBinaryOp, TestPrecedenceDivMul) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "2");
}

TEST(TestBinaryOp, TestPrecedenceDivDiv) {
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

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "0");
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
