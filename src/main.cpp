#include "common.h"
#include "debug.h"
#include "llvm.h"
#include "opcode.h"
#include "gtest/gtest.h"
#include <cstdio>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>

static std::string readFile(std::string path) {
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

static void runFile(std::string path) { std::string source = readFile(path); }

int main(int argc, const char *argv[]) {
  // if (argc == 2) {
  //   runFile(argv[1]);
  // } else {
  //   std::fprintf(stderr, "Usage: clox [path]\n");
  //   exit(64);
  // }
  llvm::LLVMContext *ctx = new llvm::LLVMContext();
  llvm::Module *module = new llvm::Module("Bonobo", *ctx);

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

  llvm::Value *Constant = builder.getInt32(0);
  builder.CreateRet(Constant);

  llvm::FunctionType *fibFuncType =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
  llvm::Function *fibFunction = llvm::Function::Create(
      fibFuncType, llvm::Function::ExternalLinkage, "fib", *module);

  // Create an LLVM IR basic block
  llvm::BasicBlock *fibBlock =
      llvm::BasicBlock::Create(*ctx, "fib", fibFunction);

  llvm::IRBuilder<> fibBuilder(fibBlock);

  llvm::Value *condV = fibBuilder.getInt32(0);
  condV = fibBuilder.CreateFCmpONE(
      llvm::ConstantFP::get(*ctx, llvm::APFloat(0.0)),
      llvm::ConstantFP::get(*ctx, llvm::APFloat(0.0)), "ifcond");

  llvm::Function *TheFunction = fibBuilder.GetInsertBlock()->getParent();

  llvm::BasicBlock *ThenBB =
      llvm::BasicBlock::Create(*ctx, "then", TheFunction);
  llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(*ctx, "else");
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*ctx, "ifcond");

  fibBuilder.CreateCondBr(condV, ThenBB, ElseBB);

  fibBuilder.SetInsertPoint(ThenBB);
  llvm::Value *ThenV = llvm::ConstantFP::get(*ctx, llvm::APFloat(0.0));

  builder.CreateBr(MergeBB);

  ThenBB = builder.GetInsertBlock();

  fibFunction->insert(fibFunction->end(), ElseBB);
  fibBuilder.SetInsertPoint(ElseBB);

  llvm::Value *ElseV = llvm::ConstantFP::get(*ctx, llvm::APFloat(0.0));
  fibBuilder.CreateBr(MergeBB);
  ElseBB = builder.GetInsertBlock();

  fibFunction->insert(fibFunction->end(), MergeBB);
  fibBuilder.SetInsertPoint(MergeBB);
  llvm::PHINode *PN =
      fibBuilder.CreatePHI(llvm::Type::getDoubleTy(*ctx), 2, "iftmp");
  PN->addIncoming(ThenV, ThenBB);
  PN->addIncoming(ElseV, ElseBB);

  llvm::Value *fibConstant = fibBuilder.getInt32(0);
  fibBuilder.CreateRet(fibConstant);

  std::error_code errorCode;
  llvm::raw_fd_ostream outLL("./out.ll", errorCode);
  module->print(outLL, nullptr);
  return 0;
}
