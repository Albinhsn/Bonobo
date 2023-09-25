#include "../src/common.h"
#include "../src/llvm.h"
#include "../src/stmt.h"

std::string readFile(std::string path) {
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

std::string runLLVMBackend(std::vector<Stmt*> result){
    LLVMCompiler *llvmCompiler = new LLVMCompiler(result);
    llvmCompiler->compile();
    system("lli out.ll > result.txt");
    delete(llvmCompiler);
    return readFile("result.txt");
}
