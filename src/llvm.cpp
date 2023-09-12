#include "llvm.h"
#include "object.h"

llvm::Function *createFunction(llvm::LLVMContext *ctx, llvm::Module *module,
                               ObjString *fnName, llvm::FunctionType *fnType) {
  llvm::FunctionType *funcType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
  llvm::Function *function = llvm::Function::Create(
      funcType, llvm::Function::ExternalLinkage, "main", *module);

  // Create an LLVM IR basic block
  llvm::BasicBlock *entryBlock =
      llvm::BasicBlock::Create(*ctx, "entry", function);

  // Create an LLVM IR instruction
  llvm::IRBuilder<> builder(entryBlock);
  llvm::Value *constant = builder.getInt32(0);
  builder.CreateRet(constant);

  return function;
}

// auto bytePtrTy = builder.getInt8Ty()->getPointerTo();
// module->getOrInsertFunction(
//     "printf", llvm::FunctionType::get(builder.getInt32Ty(), bytePtrTy, true));

// auto str = builder.CreateGlobalStringPtr("Hello World!\n");

// auto printfFn = module->getFunction("printf");

// std::vector<llvm::Value *> args{str};

// builder.CreateCall(printfFn, args);

// llvm::Value *Constant = builder.getInt32(0);
// builder.CreateRet(Constant);
