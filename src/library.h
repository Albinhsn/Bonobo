#include "llvm.h"

void addInternalFuncs(LLVMCompiler *llvmCompiler, llvm::IRBuilder<>* builder);
void addLibraryFuncs(LLVMCompiler *llvmCompiler, llvm::IRBuilder<> *builder);
void addInternalStructs(LLVMCompiler * llvmCompiler, llvm::IRBuilder<>* builder);
