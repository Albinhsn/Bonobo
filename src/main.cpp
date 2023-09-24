#include "common.h"
#include "compiler.h"
#include "llvm.h"
#include "scanner.h"
#include "trie.h"

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

int main(int argc, const char *argv[]) {
    if (argc == 1) {
        printf("Need file name\n");
        exit(1);
    }

    std::string source = readFile(argv[1]);
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
    llvmCompiler->compile();
    return 0;
}
