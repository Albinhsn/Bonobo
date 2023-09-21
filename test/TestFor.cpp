#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestForLoop, TestBasicFor) {
    std::string source =
        "for(var i: int = 0; i < 5; i = i + 1){var a: int = 5;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, FOR_STMT);

    ForStmt *forStmt = (ForStmt *)result[0];
    EXPECT_EQ(forStmt->initializer->type, VAR_STMT);
    EXPECT_EQ(forStmt->condition->type, EXPR_STMT);
    EXPECT_EQ(forStmt->increment->type, ASSIGN_STMT);
    EXPECT_EQ(forStmt->body.size(), 1);
}

TEST(TestForLoop, TestEmptyFor) {
    std::string source = "for(;;){var a: int = 5;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, FOR_STMT);
    ForStmt *forStmt = (ForStmt *)result[0];
    EXPECT_EQ(forStmt->initializer, nullptr);
    EXPECT_EQ(forStmt->condition, nullptr);
    EXPECT_EQ(forStmt->increment, nullptr);
    EXPECT_EQ(forStmt->body.size(), 1);
}