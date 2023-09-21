#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestIndex, TestIntIndex) {
    std::string source = "var foo: int = baz[1];";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *varStmt = (VarStmt *)result[0];

    EXPECT_EQ(varStmt->initializer->type, INDEX_EXPR);

    IndexExpr *indexExpr = (IndexExpr *)varStmt->initializer;

    EXPECT_EQ(std::string(indexExpr->variable->name.lexeme,
                          indexExpr->variable->name.length),
              "baz");
    EXPECT_EQ(indexExpr->index->type, LITERAL_EXPR);
}

TEST(TestIndex, TestRecIndex) {
    std::string source = "var foo: int = baz[baz[1]];";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, VAR_STMT);
    VarStmt *varStmt = (VarStmt *)result[0];

    EXPECT_EQ(varStmt->initializer->type, INDEX_EXPR);

    IndexExpr *indexExpr = (IndexExpr *)varStmt->initializer;

    EXPECT_EQ(indexExpr->index->type, INDEX_EXPR);
}
