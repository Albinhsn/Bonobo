#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include <gtest/gtest.h>

TEST(TestStruct, TestStructFields) {
    std::string source = "struct foo{a: int; b: str;};";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, STRUCT_STMT);
    StructStmt *structStmt = (StructStmt *)result[0];

    EXPECT_EQ(std::string(structStmt->name.lexeme, structStmt->name.length),
              "foo");

    EXPECT_EQ(structStmt->fieldNames.size(), 2);
}
