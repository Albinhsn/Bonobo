#include "../src/common.h"
#include "../src/compiler.h"
#include "../src/llvm.h"
#include "../src/stmt.h"
#include "./testCommon.h"
#include <gtest/gtest.h>

TEST(TestIf, TestBasicIfElse) {
    std::string source = "var i:int = 3; var a:int = 0; \n if (i < 2) {a = "
                         "5;}else {a = 4;} printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "4");
}

TEST(TestIf, TestBasicIfElse2) {
    std::string source = "var i:int = 1; var a:int = 0; \n if (i < 2) {a = "
                         "5;}else {a = 4;} printf(\"%d\", a);";

    std::vector<Stmt *> result = compile(source.c_str());

    std::string resultTxt = runLLVMBackend(result);
    EXPECT_EQ(resultTxt, "5");
}

// TEST(TestIf, TestIfScope) {
//     std::string source =
//         "if (1 < 2) {var a: int = 5;}else {var a:int = 4;} printf(\"%d\", a);";

//     std::vector<Stmt *> result = compile(source.c_str());

//     std::string resultTxt = runLLVMBackend(result);
//     EXPECT_EQ(resultTxt, "5");
// }

TEST(TestIf, TestBasicIf) {
    std::string source = "if (i < 2) {var a : int = 5;}";

    std::vector<Stmt *> result = compile(source.c_str());
    EXPECT_EQ(result.size(), 1);

    EXPECT_EQ(result[0]->type, IF_STMT);
    IfStmt *ifStmt = (IfStmt *)result[0];

    EXPECT_EQ(ifStmt->condition->type, GROUPING_EXPR);
    EXPECT_EQ(ifStmt->thenBranch.size(), 1);
    EXPECT_EQ(ifStmt->thenBranch[0]->type, VAR_STMT);

    EXPECT_EQ(ifStmt->elseBranch.size(), 0);
}
