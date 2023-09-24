#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/raw_ostream.h>

int main() {
    llvm::LLVMContext context;
    llvm::Module module("example", context);

    // Create the main function
    llvm::FunctionType *funcType =
        llvm::FunctionType::get(llvm::Type::getInt32Ty(context), false);
    llvm::Function *mainFunc = llvm::Function::Create(
        funcType, llvm::Function::ExternalLinkage, "main", module);

    // Create the entry basic block for the main function
    llvm::BasicBlock *entryBlock =
        llvm::BasicBlock::Create(context, "entry", mainFunc);

    // Create an IRBuilder and set it to the entry block
    llvm::IRBuilder<> builder(context);
    builder.SetInsertPoint(entryBlock);

    std::vector<llvm::Type *> printfArgs;
    printfArgs.push_back(llvm::Type::getInt8PtrTy(context)); // Format string
    llvm::Value *formatStr = builder.CreateGlobalStringPtr("%s\n");
    llvm::FunctionType *printfType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(context), printfArgs, true);
    llvm::FunctionCallee printfFunc =
        module.getOrInsertFunction("printf", printfType);

    // Initialize the loop variable (i = 0)
    llvm::Value *i = builder.getInt32(0);
    llvm::Value *hw = builder.CreateGlobalStringPtr("hello world!\n");

    // Create the loop header block
    llvm::BasicBlock *loopHeaderBlock =
        llvm::BasicBlock::Create(context, "loop.header", mainFunc);

    // Create the loop body block
    llvm::BasicBlock *loopBodyBlock =
        llvm::BasicBlock::Create(context, "loop.body", mainFunc);

    // Create the loop exit block
    llvm::BasicBlock *loopExitBlock =
        llvm::BasicBlock::Create(context, "loop.exit", mainFunc);

    // Branch to the loop header to start the loop
    builder.CreateBr(loopHeaderBlock);
    builder.SetInsertPoint(loopHeaderBlock);

    // Create a phi node to merge the loop variable values
    llvm::PHINode *phi = builder.CreatePHI(llvm::Type::getInt32Ty(context), 2);
    phi->addIncoming(i, entryBlock);

    // Loop condition (i < 10)
    llvm::Value *condition = builder.CreateICmpSLT(phi, builder.getInt32(10));

    // Conditional branch to the loop body or exit
    builder.CreateCondBr(condition, loopBodyBlock, loopExitBlock);

    // Set the insertion point to the loop body
    builder.SetInsertPoint(loopBodyBlock);

    // Loop body: Print the loop variable
    builder.CreateCall(
        printfFunc,
        {formatStr, hw}); // Assuming printfFunc and formatStr are defined

    // Increment the loop variable (i++)
    llvm::Value *incrementedI = builder.CreateAdd(phi, builder.getInt32(1));
    phi->addIncoming(incrementedI, loopBodyBlock);

    // Branch back to the loop header
    builder.CreateBr(loopHeaderBlock);

    // Set the insertion point to the loop exit
    builder.SetInsertPoint(loopExitBlock);

    // Return from the main function
    builder.CreateRet(builder.getInt32(0));

    // Print LLVM IR
    std::error_code errorCode;
    llvm::raw_fd_ostream outLL("./out.ll", errorCode);
    module.print(outLL, nullptr);

    return 0;
}
