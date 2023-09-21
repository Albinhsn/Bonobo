#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestIf, TestBasicIfElse) {
    std::string source =
        "if (i < 2) {var a : int = 5;}else {var b : int = 3; var a: int = 4;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, IF_STMT);
    IfStmt *ifStmt = (IfStmt *)result[0];

    EXPECT_EQ(ifStmt->condition->type, GROUPING_EXPR);
    EXPECT_EQ(ifStmt->thenBranch.size(), 1);
    EXPECT_EQ(ifStmt->thenBranch[0]->type, VAR_STMT);


    EXPECT_EQ(ifStmt->elseBranch.size(), 2);
    EXPECT_EQ(ifStmt->elseBranch[0]->type, VAR_STMT);
    EXPECT_EQ(ifStmt->elseBranch[1]->type, VAR_STMT);
}

TEST(TestIf, TestBasicIf) {
    std::string source =
        "if (i < 2) {var a : int = 5;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, IF_STMT);
    IfStmt *ifStmt = (IfStmt *)result[0];

    EXPECT_EQ(ifStmt->condition->type, GROUPING_EXPR);
    EXPECT_EQ(ifStmt->thenBranch.size(), 1);
    EXPECT_EQ(ifStmt->thenBranch[0]->type, VAR_STMT);


    EXPECT_EQ(ifStmt->elseBranch.size(), 0);
}
