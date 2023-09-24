
#include "../../src/common.h"
#include "../../src/compiler.h"
#include "../../src/llvm.h"

static std::string readFile(std::string path) {
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

int main() {
    std::map<std::string, std::string> inputs = {
        {"array.bo", ""},
        {"ast.bo", "5\n0\n1\n"},
        {"binary.bo",
         "8\n1\n15\n10\n6\n11\n6\n4\n0\n-1\n4\n17\n13\n30\n7\n3\n-1\n2\n0\n"},
        {"comparison.bo", "0\n0\n1\n1\n0\n0\n"},
        {"for.bo", "0\n1\n2\n3\n4\n"},
        {"fun.bo", "5\n3\n"},
        {"if.bo", "4\n5\n"},
        {"index.bo", "2\n"},
        {"logical.bo", "0\n1\n"},
        {"map.bo", ""},
        {"struct.bo", ""},
        {"while.bo", "0\n1\n2\n3\n4\n"}};
    for (const auto &[key, value] : inputs) {
        std::string source = readFile(key);
        size_t found = source.find("\\n");
        while (found != std::string::npos) {
            source.replace(found, 2, "\n");
            found = source.find("\\n");
        }
        found = source.find("\\t");
        while (found != std::string::npos) {
            source.replace(found, 2, "\t");
            found = source.find("\\t");
        }
        std::vector<Stmt *> stmts = compile(source);
        LLVMCompiler *llvmCompiler = new LLVMCompiler(stmts);
        std::cout << "TESTING: " << key << "\n";
        llvmCompiler->compile();
        auto x = system("lli out.ll > result.txt");
        std::string result = readFile("result.txt");
        if (result != value || x == -1) {
            std::cout << "TEST FAILED: " << key << "\n Expected: " << value
                      << "\n Got: " << result << "\n";
        } else {
            std::cout << "Test passed " << key << "\n";
        }
    }
}
