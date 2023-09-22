#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestArray, TestEmptyArray) {
    std::string source = "var a: arr[int] = [];";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *stmt = (VarStmt *)result[0];

    EXPECT_EQ(stmt->initializer->type, ARRAY_EXPR);

    ArrayExpr *arrayExpr = (ArrayExpr *)stmt->initializer;
    EXPECT_EQ(arrayExpr->items.size(), 0);
}

TEST(TestArray, Test1DArray) {
    std::string source = "var a: arr[int] = [1,2,3];";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *stmt = (VarStmt *)result[0];

    EXPECT_EQ(stmt->initializer->type, ARRAY_EXPR);

    ArrayExpr *arrayExpr = (ArrayExpr *)stmt->initializer;
    EXPECT_EQ(arrayExpr->items.size(), 3);
}

TEST(TestArray, Test2DArray) {
    std::string source = "var a: arr[arr[int]] = [[], []];";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *stmt = (VarStmt *)result[0];

    EXPECT_EQ(stmt->initializer->type, ARRAY_EXPR);

    ArrayExpr *arrayExpr = (ArrayExpr *)stmt->initializer;
    EXPECT_EQ(arrayExpr->items.size(), 2);
}
