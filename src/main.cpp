#include "compiler.h"
#include "llvm.h"
#include <fstream>
#include <sstream>

static std::string readFile(const char *path) {
    std::ifstream t(path);
    std::stringstream buffer;
    if (t.fail()) {
        printf("file doesn't exist\n");
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
    Compiler *compiler = compile(source);

    std::vector<Variable *> variables = compiler->variables;
    std::vector<Stmt *> stmts = compiler->statements;
    initCompiler(compiler->variables);
    compile(compiler->statements);
    system("lli out.ll");

    return 0;
}
