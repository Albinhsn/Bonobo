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
    runTest("Array - Int array", arr1, "1 2 3", failed);

    std::string arr2 = "var a: arr[bool] = [true, false];";
    nmbr_of_tests++;
    runTest("Array - Bool array", arr2, "", failed);

    std::string arr5 =
        "struct foo{bar:int;}; var a: arr[foo] = [foo(1), foo(2), foo(3)]; var b: foo = a[2]; printf(\"%d\", b.bar);";
    nmbr_of_tests++;
    runTest("Array - Struct array", arr5, "3", failed);

    std::string arr6 = "var a: arr[arr[arr[int]]] = [[[1]]]; var b: arr[arr[int]] = a[0]; var c: arr[int] = b[0]; "
                       "printf(\"%d\", c[0]);";
    nmbr_of_tests++;
    runTest("Array - 3D int array", arr6, "1", failed);

    // Index test
    std::string index =
        "var a: arr[int] = [1,2,3];var b: str = \"Hi!\";var e: arr[bool] = [true, false, "
        "true]; var f: arr[double] = [3.14, 5.25];printf(\"%d %c %d %d %.2lf\", a[2], b[0], e[0], e[1], f[0]);";
    nmbr_of_tests++;
    runTest("Index - 1D int, str, bool, double array", index, "3 H 1 0 3.14", failed);

    std::string index2 = "var a: arr[arr[int]] = [[1], [2]]; var b: arr[int] = a[0]; printf(\"%d\", b[0]);";
    nmbr_of_tests++;
    runTest("Index - 2D int array ", index2, "1", failed);

    std::string index3 = "var a: arr[arr[int]] = [[1], [2]];printf(\"%d\", a[0][0]);";
    nmbr_of_tests++;
    runTest("Index - 2D int array a[0][0]", index3, "1", failed);

    std::string index4 = "var a: arr[arr[arr[str]]] = [[[\"Hi\"]]];printf(\"%s\", a[0][0][0]);";
    nmbr_of_tests++;
    runTest("Index - 3D str array", index4, "Hi", failed);

    // Assign to index
    std::string assignIndex1 =
        "var a: arr[arr[int]] = [[0,1,2]]; var b:arr[int] = [0,1,5]; a[0] = b; printf(\"%d\", a[0][2]);";
    nmbr_of_tests++;
    runTest("AssignIndex - Assign array to 2D array", assignIndex1, "5", failed);

    std::string assignIndex2 = "var a: arr[int] = [0,1,2];  a[2] = 5; printf(\"%d\", a[2]);";
    nmbr_of_tests++;
    runTest("AssignIndex - Assign int to int array", assignIndex2, "5", failed);

    std::string assignIndex3 = "var a: arr[arr[int]] = [[0,1,2]];  a[0][2] = 5; printf(\"%d\", a[0][2]);";
    nmbr_of_tests++;
    runTest("AssignIndex - Assign int to 2D array", assignIndex3, "5", failed);

    std::string assignIndex4 =
        "var a: arr[arr[double]] = [[0.0,1.0,2.5]];  a[0][2] = 5.25; printf(\"%.3lf\", a[0][2]);";
    nmbr_of_tests++;
    runTest("AssignIndex - Assign double to 2D array", assignIndex4, "5.250", failed);

    std::string assignIndex5 =
        "var a: arr[arr[bool]] = [[true, false, true]];  a[0][2] = false; printf(\"%d\", a[0][2]);";
    nmbr_of_tests++;
    runTest("AssignIndex - Assign bool to 2D array", assignIndex5, "0", failed);

    std::string assignIndex6 =
        "var a: arr[arr[str]] = [[\"Hi\", \"Mom\"]];  a[0][1] = \"Sailor\"; printf(\"%s\", a[0][1]);";
    nmbr_of_tests++;
    runTest("AssignIndex - Assign str to 2D array", assignIndex6, "Sailor", failed);

    std::string assignIndex7 = "struct foo{bar:int;};var a: arr[arr[foo]] = [[foo(1), foo(2)]];  a[0][1] = foo(3); "
                               "printf(\"%d\", a[0][1].bar);";
    nmbr_of_tests++;
    runTest("AssignIndex - Assign struct to 2D array", assignIndex7, "3", failed);

    // std::string assignIndex8 =
    //     "struct foo{bar:int;};var a: arr[arr[arr[int]]] = [[[0], [1]]];  a[0][2] = [2]; printf(\"%d\", a[0][2][0]);";
    // nmbr_of_tests++;
    // runTest("AssignIndex - Assign arr to 3D array", assignIndex8, "2", failed);

    // String test
    std::string str1 = "var s: str = \"Hello World\"; printf(\"%s\", s);";
    nmbr_of_tests++;
    runTest("Str - print Hello World", str1, "Hello World", failed);

    // Binary op test
    std::string bin1 = "var a: int = 5 * 2 + 1; printf(\"%d\", a);";
    nmbr_of_tests++;
    runTest("BinaryOp - Mul Add ", bin1, "11", failed);

    // Conc string
    std::string conc1 = "var a: str = \"Hi \" + \"Mom\"; printf(\"%s\", a);";
    nmbr_of_tests++;
    runTest("Concat - 2 strings", conc1, "Hi Mom", failed);

    // FP test
    std::string fp1 = "var a: double = 5.0 * 2.5; printf(\"%lf\", a);";
    nmbr_of_tests++;
    runTest("FP - FP mul", fp1, "12.500000", failed);

    std::string fp2 = "var a: double = 5 * 2.5; printf(\"%lf\", a);";
    nmbr_of_tests++;
    runTest("FP - Int and FP mul", fp2, "12.500000", failed);

    // Struct test
    std::string strukt1 = "struct foo{bar: int;};var f:foo = foo(1); printf(\"%d\", f.bar);";
    nmbr_of_tests++;
    runTest("Strukt - Int struct", strukt1, "1", failed);

    std::string strukt2 = "struct foo{bar: double;};var f:foo = foo(2.5); printf(\"%lf\", f.bar);";
    nmbr_of_tests++;
    runTest("Strukt - Double struct", strukt2, "2.500000", failed);

    std::string strukt3 = "struct foo{bar: double; bar: int;};var f:foo = foo(2.5, 1); printf(\"%lf\", f.bar);";
    nmbr_of_tests++;
    runTest("Strukt - Int, Double struct", strukt3, "2.500000", failed);

    // Property test
    std::string prop1= "struct foo{bar: int;};var f:foo = foo(1); f.bar = 5; printf(\"%d\", f.bar);";
    nmbr_of_tests++;
    runTest("Strukt Property Assignment - Assign to var", prop1, "5", failed);

    // For tes++t
    std::string for1 = "for(var i: int = 0; i < 5; i++){printf(\"%d\", i);}";
    nmbr_of_tests++;
    runTest("For - 0 to 4, i++", for1, "01234", failed);

    std::string for3 = "for(var i: int = 0; i < 5; i+=1){printf(\"%d\", i);}";
    nmbr_of_tests++;
    runTest("For - 0 to 4, i += 1", for3, "01234", failed);

    std::string for2 = "for(var i: int = 0; i < 5; i++){if(i > 3){break;}printf(\"%d\", i);}";
    nmbr_of_tests++;
    runTest("For - break", for2, "0123", failed);

    // While test
    std::string while1 = "var i: int = 0; while(i < 5){printf(\"%d\", i); i++;}";
    nmbr_of_tests++;
    runTest("While - i++", while1, "01234", failed);

    std::string while2 = "var i: int = 0; while(i < 5){if(i > 3){break;}printf(\"%d\", i); i++;}";
    nmbr_of_tests++;
    runTest("While - break", while2, "0123", failed);

    // Func test
    std::string fun1 = "fun foo() -> int {return 1;} printf(\"%d\", foo());";
    nmbr_of_tests++;
    runTest("Func - return int", fun1, "1", failed);

    std::string fun2 = "fun foo(a: int, b:int) -> int {return a + b;} printf(\"%d\", foo(1,2));";
    nmbr_of_tests++;
    runTest("Func - Add func", fun2, "3", failed);

    std::string fun3 = "struct bar{bar:int;}; fun foo() -> bar {return bar(2);} printf(\"%d\", foo().bar);";
    nmbr_of_tests++;
    runTest("Func3Test", fun3, "2", failed);

    std::string fun4 =
        "struct bar{bar:int;}; fun foo() -> bar {return bar(2);} var f: bar = foo(); printf(\"%d\", f.bar);";
    nmbr_of_tests++;
    runTest("Func - return struct", fun4, "2", failed);

    std::string fun6 = "fun foo() -> str {return \"Hi\";} printf(\"%s\", foo());";
    nmbr_of_tests++;
    runTest("Func - return str", fun6, "Hi", failed);

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
