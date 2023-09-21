#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestWhile, TestWhileBasic) {
    std::string source = "while(i < 5){i = i + 1;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, WHILE_STMT);
    WhileStmt *whileStmt = (WhileStmt *)result[0];

    EXPECT_EQ(whileStmt->condition->type, GROUPING_EXPR);
    EXPECT_EQ(whileStmt->body.size(), 1);
}
