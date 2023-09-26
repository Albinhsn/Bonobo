#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/stmt.h"
#include "./testCommon.h"
#include <gtest/gtest.h>

TEST(TestStruct, TestStructFields) {
    std::string source = "struct foo{a: int; b: double;}; var a: foo = foo(1, 2.25); printf(\"%d \", a.a);";

    std::vector<Stmt *> result = compile(source.c_str());
    std::string resultTxt = runLLVMBackend(result); 
    EXPECT_EQ(resultTxt, "1, \"hi\"");
}
