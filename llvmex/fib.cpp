
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <llvm/Support/raw_ostream.h>

int main(int argc, const char *argv[]) {
  llvm::LLVMContext *ctx = new llvm::LLVMContext();
  llvm::Module *module = new llvm::Module("Bonobo", *ctx);
  // Declare the external printf function
  std::vector<llvm::Type *> printfArgs;
  printfArgs.push_back(llvm::Type::getInt8PtrTy(*ctx)); // Format string
  llvm::FunctionType *printfType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), printfArgs, true);
  llvm::FunctionCallee printfFunc =
      module->getOrInsertFunction("printf", printfType);

  std::vector<llvm::Type *> paramTypes = {llvm::Type::getInt32Ty(*ctx)};
  llvm::FunctionType *fibFuncType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), paramTypes, false);
  llvm::Function *fibFunction = llvm::Function::Create(
      fibFuncType, llvm::Function::ExternalLinkage, "fib", *module);

  // Create an LLVM IR basic block
  llvm::BasicBlock *fibBlock =
      llvm::BasicBlock::Create(*ctx, "fib", fibFunction);

  llvm::IRBuilder<> fibBuilder(fibBlock);

  llvm::Function::arg_iterator args = fibFunction->arg_begin();
  llvm::Value *arg = &*args++;

  llvm::Value *condition =
      fibBuilder.CreateICmpSLE(arg, fibBuilder.getInt32(2));

  // Create basic blocks for "then" and "else" branches
  llvm::BasicBlock *thenBlock =
      llvm::BasicBlock::Create(*ctx, "then", fibFunction);
  llvm::BasicBlock *elseBlock =
      llvm::BasicBlock::Create(*ctx, "else", fibFunction);
  llvm::BasicBlock *mergeBlock =
      llvm::BasicBlock::Create(*ctx, "merge", fibFunction);

  // Emit the conditional branch instruction
  fibBuilder.CreateCondBr(condition, thenBlock, elseBlock);

  // Emit code in the "then" block
  fibBuilder.SetInsertPoint(thenBlock);
  llvm::Value *thenValue =
      fibBuilder.getInt32(1); // Some value to return in "then" block
  fibBuilder.CreateBr(mergeBlock);

  // Emit code in the "else" block
  fibBuilder.SetInsertPoint(elseBlock);
  llvm::Value *minTwo = fibBuilder.CreateSub(arg, fibBuilder.getInt32(2));
  llvm::Value *minOne = fibBuilder.CreateSub(arg, fibBuilder.getInt32(1));

  llvm::Value *elseValue =
      fibBuilder.CreateAdd(fibBuilder.CreateCall(fibFunction, {minTwo}),
                           fibBuilder.CreateCall(fibFunction, {minOne}));

  fibBuilder.CreateBr(mergeBlock);

  // Emit code in the merge block
  fibBuilder.SetInsertPoint(mergeBlock);
  llvm::PHINode *phiNode = fibBuilder.CreatePHI(llvm::Type::getInt32Ty(*ctx),
                                                2); // Two incoming values
  phiNode->addIncoming(thenValue, thenBlock);
  phiNode->addIncoming(elseValue, elseBlock);

  // Return the result
  fibBuilder.CreateRet(phiNode);

  // Create an LLVM IR function
  llvm::FunctionType *funcType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
  llvm::Function *function = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, "main", *module);

  // Create an LLVM IR basic block
  llvm::BasicBlock *entryBlock =
      llvm::BasicBlock::Create(*ctx, "entry", function);

  // Create an LLVM IR instruction
  llvm::IRBuilder<> builder(entryBlock);

  // Define the format string and arguments for printf
  llvm::Value *formatStr = builder.CreateGlobalStringPtr("%d\n");
  llvm::Value *valueToPrint =
      builder.CreateCall(fibFunction, {builder.getInt32(35)});

  // Call printf
  llvm::Value *printfResult =
      builder.CreateCall(printfFunc, {formatStr, valueToPrint});

  llvm::Value *Constant = builder.getInt32(0);
  builder.CreateRet(Constant);

  std::error_code errorCode;
  llvm::raw_fd_ostream outLL("./out.ll", errorCode);
  module->print(outLL, nullptr);
  return 0;
}
