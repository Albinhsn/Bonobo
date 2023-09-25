#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/llvm.h"
#include "../src/stmt.h"
#include "./testCommon.h"
#include <gtest/gtest.h>


TEST(TestWhile, TestWhileBasic) {
    std::string source =
        "var i: int = 0; while(i < 5){printf(\"%d\", i);i = i + 1;}";

    std::vector<Stmt *> result = compile(source.c_str());

    std::string resultTxt = runLLVMBackend(result); 
    EXPECT_EQ(resultTxt, "01234");
}
