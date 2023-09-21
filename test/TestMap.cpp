#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestMap, TestEmptyMap) {
    std::string source = "var foo: map = {};";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *varStmt = (VarStmt *)result[0];

    EXPECT_EQ(varStmt->initializer->type, MAP_EXPR);

    MapExpr *mapExpr = (MapExpr *)varStmt->initializer;

    EXPECT_EQ(mapExpr->keys.size(), 0);
    EXPECT_EQ(mapExpr->values.size(), 0);
}

TEST(TestMap, TestBasicMap) {
    std::string source = "var foo: map = {1:2, 3:4};";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *varStmt = (VarStmt *)result[0];

    EXPECT_EQ(varStmt->initializer->type, MAP_EXPR);

    MapExpr *mapExpr = (MapExpr *)varStmt->initializer;

    EXPECT_EQ(mapExpr->keys.size(), 2);
    EXPECT_EQ(mapExpr->values.size(), 2);
}
