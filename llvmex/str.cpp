#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <llvm/Support/raw_ostream.h>

int main(int argc, const char *argv[]) {
  llvm::LLVMContext *ctx = new llvm::LLVMContext();
  llvm::Module *module = new llvm::Module("Bonobo", *ctx);
  // Create a global string variable
  llvm::Constant *strConstant =
      llvm::ConstantDataArray::getString(*ctx, "Hello, LLVM IR!");
  llvm::GlobalVariable *strGlobal =
      new llvm::GlobalVariable(*module, strConstant->getType(), true,
                               llvm::GlobalValue::PrivateLinkage, strConstant);

  llvm::FunctionType *funcType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
  llvm::Function *function = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, "main", *module);

  llvm::BasicBlock *entryBlock =
      llvm::BasicBlock::Create(*ctx, "entry", function);

  llvm::IRBuilder<> builder(entryBlock);
  builder.CreateRet(builder.getInt32(0));

  std::error_code errorCode;
  llvm::raw_fd_ostream outLL("./out.ll", errorCode);
  module->print(outLL, nullptr);
  return 0;
}
