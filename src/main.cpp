#include "common.h"
#include "compiler.h"
#include "llvm.h"
#include "scanner.h"
#include "trie.h"
#include <cstdlib>

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
    Compiler * compiler = compile(source);

    LLVMCompiler *llvmCompiler = new LLVMCompiler(compiler->statements, compiler->variables);
    llvmCompiler->compile();
    system("lli out.ll");

    return 0;
}
