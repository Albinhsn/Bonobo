#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestFunction, TestBasicFunc) {
    std::string source = "fun foo(a:int, b:int) -> int{a + b;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, FUNC_STMT);

    FuncStmt *funStmt = (FuncStmt *)result[0];
    EXPECT_EQ(funStmt->name.lexeme, "foo");

    EXPECT_EQ(funStmt->params.size(), 2);
    EXPECT_EQ(funStmt->body.size(), 1);
    EXPECT_EQ(funStmt->returnType->type, INT_VAR);
}

TEST(TestFunction, TestEmptyFunc) {
    std::string source = "fun foo() -> int{}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, FUNC_STMT);

    FuncStmt *funStmt = (FuncStmt *)result[0];
    EXPECT_EQ(funStmt->name.lexeme, "foo");

    EXPECT_EQ(funStmt->params.size(), 0);
    EXPECT_EQ(funStmt->body.size(), 0);
    EXPECT_EQ(funStmt->returnType->type, INT_VAR);
}
TEST(TestFunction, TestFuncReturn) {
    std::string source = "fun foo() -> int{return 5;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, FUNC_STMT);

    FuncStmt *funStmt = (FuncStmt *)result[0];
    EXPECT_EQ(funStmt->name.lexeme, "foo");

    EXPECT_EQ(funStmt->params.size(), 0);
    EXPECT_EQ(funStmt->body.size(), 1);
    EXPECT_EQ(funStmt->returnType->type, INT_VAR);
}
