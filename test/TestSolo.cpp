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
    Compiler *compiler = compile(source);
    LLVMCompiler *llvmCompiler = new LLVMCompiler(compiler->statements, compiler->variables);
    delete (compiler);
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

    std::string arr1 = "var a: arr[int] = [1,2,3]; printf(\"%d %d %d\", a[0], a[1], a[2]);";
    nmbr_of_tests++;
    runTest("IntArrayTest", arr1, "1 2 3", failed);

    std::string arr2 = "var a: arr[bool] = [true, false];";
    nmbr_of_tests++;
    runTest("BoolArrayTest", arr2, "", failed);

    std::string arr3 = "var a: arr[str] = [\"Hi\", \"Mom\"]; printf(\"%s\", a[0]);";
    nmbr_of_tests++;
    runTest("StrArrayTest", arr3, "Hi", failed);

    std::string arr4 = "var a: arr[arr[int]] = [[1], [2,3]]; var b: arr[int] = a[1]; printf(\"%d\", b[1]);";
    nmbr_of_tests++;
    runTest("IntArrArrayTest", arr4, "3", failed);

    std::string arr7 = "var a: arr[arr[arr[int]]] = [[[1]]]; var b: arr[arr[int]] = a[0]; var c: arr[int] = b[0]; printf(\"%d\", c[0]);";
    nmbr_of_tests++;
    runTest("3DIntArrayTest", arr7, "1", failed);

    std::string arr5 =
        "struct foo{bar:int;}; var a: arr[foo] = [foo(1), foo(2), foo(3)]; var b: foo = a[2]; printf(\"%d\", b.bar);";
    nmbr_of_tests++;
    runTest("StructArrayTest", arr5, "3", failed);

    // std::string arr6 = "var a: arr[arr[arr[int]]] = [[[1]]];";
    // nmbr_of_tests++;
    // runTest("3D int array", arr6, "", failed);

    // Index test
    std::string index = "var a: arr[int] = [1,2,3];var b: str = \"Hi!\";var e: arr[bool] = [true, false, "
                        "true];printf(\"%d %c %d %d\", a[2], b[0], e[0], e[1]);";
    nmbr_of_tests++;
    runTest("IndexTest", index, "3 H 1 0", failed);

    std::string index2 = "var a: arr[arr[int]] = [[1], [2]]; var b: arr[int] = a[0]; printf(\"%d\", b[0]);";
    nmbr_of_tests++;
    runTest("IndexTest2", index2, "1", failed);

    // std::string index3 = "var a: arr[arr[int]] = [[1], [2]];printf(\"%d\", a[0][0]);";
    // nmbr_of_tests++;
    // runTest("IndexTest3", index3, "1", failed);

    // String test
    std::string str1 = "var s: str = \"Hello World\"; printf(\"%s\", s);";
    nmbr_of_tests++;
    runTest("StrTest", str1, "Hello World", failed);

    // Binary op test
    std::string bin1 = "var a: int = 5 * 2 + 1; printf(\"%d\", a);";
    nmbr_of_tests++;
    runTest("BinTest1", bin1, "11", failed);

    // Conc string
    std::string conc1 = "var a: str = \"Hi \" + \"Mom\"; printf(\"%s\", a);";
    nmbr_of_tests++;
    runTest("ConcTest1", conc1, "Hi Mom", failed);

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
    runTest("StruktTest1", strukt1, "1", failed);

    std::string strukt2 = "struct foo{bar: double;};var f:foo = foo(2.5); printf(\"%lf\", f.bar);";
    nmbr_of_tests++;
    runTest("StruktTest2", strukt2, "2.500000", failed);

    // For test
    std::string for1 = "for(var i: int = 0; i < 5; i = i + 1){printf(\"%d\", i);}";
    nmbr_of_tests++;
    runTest("ForTest", for1, "01234", failed);

    std::string for2 = "for(var i: int = 0; i < 5; i = i + 1){if(i > 3){break;}printf(\"%d\", i);}";
    nmbr_of_tests++;
    runTest("ForTest", for2, "0123", failed);

    // While test
    std::string while1 = "var i: int = 0; while(i < 5){printf(\"%d\", i); i = i + 1;}";
    nmbr_of_tests++;
    runTest("WhileTest", while1, "01234", failed);

    std::string while2 = "var i: int = 0; while(i < 5){if(i > 3){break;}printf(\"%d\", i); i = i + 1;}";
    nmbr_of_tests++;
    runTest("WhileTest", while2, "0123", failed);

    // Func test
    std::string fun1 = "fun foo() -> int {return 1;} printf(\"%d\", foo());";
    nmbr_of_tests++;
    runTest("Func1Test", fun1, "1", failed);

    std::string fun2 = "fun foo(a: int, b:int) -> int {return a + b;} printf(\"%d\", foo(1,2));";
    nmbr_of_tests++;
    runTest("Func2Test", fun2, "3", failed);

    // std::string fun3 = "struct bar{bar:int;}; fun foo() -> bar {return bar(2);} printf(\"%d\", foo().bar);";
    // nmbr_of_tests++;
    // runTest("Func3Test", fun3, "2", failed);

    std::string fun4 =
        "struct bar{bar:int;}; fun foo() -> bar {return bar(2);} var f: bar = foo(); printf(\"%d\", f.bar);";
    nmbr_of_tests++;
    runTest("Func4Test", fun4, "2", failed);

    std::string fun5 = "fun foo() -> str {return \"Hi\";} var s: str = foo(); printf(\"%s\", s);";
    nmbr_of_tests++;
    runTest("Func5Test", fun5, "Hi", failed);

    std::string fun6 = "fun foo() -> str {return \"Hi\";} printf(\"%s\", foo());";
    nmbr_of_tests++;
    runTest("Func6Test", fun6, "Hi", failed);

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
