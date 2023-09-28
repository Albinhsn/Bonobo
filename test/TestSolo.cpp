#include "../src/llvm.h"

static std::string readFile(const char *path) {
    std::ifstream t(path);
    std::stringstream buffer;
    if (t.fail()) {
        std::cout << "file doesn't exist\n";
        exit(1);
    }
    buffer << t.rdbuf();
    t.close();
    return buffer.str();
}
bool runTest(std::string name, std::string source, std::string expected, std::vector<std::string> &failed) {

    printf("Running: %s\n", name.c_str());
    std::vector<Stmt *> stmts = compile(source);
    LLVMCompiler *llvmCompiler = new LLVMCompiler(stmts);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    if (readFile("result.txt") == expected) {
        printf("OK: %s\n", name.c_str());
        return true;
    }
    printf("FAILED: %s\n", name.c_str());
    printf("\tExpected: %s\n", expected.c_str());
    printf("\tGot: %s\n", readFile("result.txt").c_str());
    failed.push_back(name);
    return false;
}

int main() {
    int nmbr_of_tests = 0;
    std::vector<std::string> failed;
    // Array test

    // std::string first = "var a: arr[int] = [1,2,3];";
    // nmbr_of_tests++;
    // runTest("IntArrayTest", first, "", failed);

    // std::string second = "var a: arr[bool] = [true, false];";
    // nmbr_of_tests++;
    // runTest("BoolArrayTest", second, "", failed);

    // std::string third = "var a: arr[str] = [\"Hi\", \"Mom\"];";
    // nmbr_of_tests++;
    // runTest("StrArrayTest", third, "", failed);

    // String test
    std::string str1 = "var s: str = \"Hello World\"; printf(\"%s\", s);";
    nmbr_of_tests++;
    runTest("StrTest", str1, "Hello World", failed);

    // Binary op test
    std::string bin1 = "var a: int = 5 * 2 + 1; printf(\"%d\", a);";
    nmbr_of_tests++;
    runTest("BinTest1", bin1, "11", failed);

    // FP test
    std::string fp1 = "var a: double = 5.0 * 2.5; printf(\"%lf\", a);";
    nmbr_of_tests++;
    runTest("FPTest1", fp1, "12.500000", failed);

    std::string fp2 = "var a: double = 5 * 2.5; printf(\"%lf\", a);";
    nmbr_of_tests++;
    runTest("FPTest2", fp2, "12.500000", failed);

    // Struct test
    std::string strukt1 = "struct foo{bar: int;};var f:foo = foo(1); printf(\"%d\", f.bar);";
    nmbr_of_tests++;
    runTest("StruktTest2", strukt1, "1", failed);

    std::string strukt2 = "struct foo{bar: double;};var f:foo = foo(2.5); printf(\"%lf\", f.bar);";
    nmbr_of_tests++;
    runTest("StruktTest2", strukt2, "2.500000", failed);

    // For test
    std::string for1 = "for(var i: int = 0; i < 5; i = i + 1){printf(\"%d\", i);}";
    nmbr_of_tests++;
    runTest("ForTest", for1, "01234", failed);

    // While test
    std::string while1 = "var i: int = 0; while(i < 5){printf(\"%d\", i); i = i + 1;}";
    nmbr_of_tests++;
    runTest("WhileTest", while1, "01234", failed);

    if (failed.size() == 0) {
        printf("\nAll test passed\n");
    } else {
        printf("----------\n");
        for (int i = 0; i < failed.size(); ++i) {
            printf("%s\n", failed[i].c_str());
        }
        printf("Failed %d tests\n", (int)failed.size());
    }
}
