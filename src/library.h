#include "llvm.h"

void addInternalFuncs(LLVMCompiler *llvmCompiler);
void addLibraryFuncs(LLVMCompiler *llvmCompiler, llvm::IRBuilder<> *builder);
void addInternalStructs(LLVMCompiler * llvmCompiler, llvm::IRBuilder<>* builder);
