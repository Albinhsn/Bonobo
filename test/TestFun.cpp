#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include "../src/llvm.h"
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

TEST(TestFunction, TestBasicFunc) {
    std::string source =
        "fun foo(a:int, b:int) -> int{return a + b;} printf(\"%d\", foo(1,2));";

    std::vector<Stmt *> result = compile(source.c_str());

    EXPECT_EQ(result[0]->type, FUNC_STMT);

    FuncStmt *funStmt = (FuncStmt *)result[0];
    EXPECT_EQ(funStmt->name.lexeme, "foo");

    EXPECT_EQ(funStmt->params.size(), 2);
    EXPECT_EQ(funStmt->body.size(), 1);
    EXPECT_EQ(funStmt->returnType->type, INT_VAR);

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "3");
}

TEST(TestFunction, TestEmptyFunc) {
    std::string source = "fun foo() -> int{return 1;}";

    std::vector<Stmt *> result = compile(source.c_str());

    EXPECT_EQ(result[0]->type, FUNC_STMT);

    FuncStmt *funStmt = (FuncStmt *)result[0];
    EXPECT_EQ(funStmt->name.lexeme, "foo");

    EXPECT_EQ(funStmt->params.size(), 0);
    EXPECT_EQ(funStmt->body.size(), 1);
    EXPECT_EQ(funStmt->returnType->type, INT_VAR);

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "");
}
TEST(TestFunction, TestFuncReturn) {
    std::string source = "fun foo() -> int{return 5;} printf(\"%d\", foo());";

    std::vector<Stmt *> result = compile(source.c_str());

    EXPECT_EQ(result[0]->type, FUNC_STMT);

    FuncStmt *funStmt = (FuncStmt *)result[0];
    EXPECT_EQ(funStmt->name.lexeme, "foo");

    EXPECT_EQ(funStmt->params.size(), 0);
    EXPECT_EQ(funStmt->body.size(), 1);
    EXPECT_EQ(funStmt->returnType->type, INT_VAR);

    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    std::string resultTxt = readFile("result.txt");
    EXPECT_EQ(resultTxt, "5");
}
