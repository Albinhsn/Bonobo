#include "library.h"

static llvm::Function *createIndexStrMap(LLVMCompiler *llvmCompiler, llvm::IRBuilder<> *llvmBuilder) {
    // Fix func type
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvmBuilder->getPtrTy(), {llvmCompiler->internalStructs["map"], llvmCompiler->internalStructs["array"]}, false);
    llvm::Function *function =
        llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "indexStrMap", *llvmCompiler->module);
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "entry", function);
    llvm::IRBuilder<> *builder = new llvm::IRBuilder<>(entryBlock);

    llvm::Function::arg_iterator arg = function->arg_begin();
    llvm::Value *mapArg = arg++;
    llvm::Value *keyArg = arg;

    llvm::Value *keyPtr = builder->CreateExtractValue(mapArg, 0);
    llvm::Value *loadedKeyArray = builder->CreateLoad(llvmCompiler->internalStructs["array"], keyPtr);

    llvm::Value *keyExists = builder->CreateCall(llvmCompiler->internalFuncs["findStrKey"], {loadedKeyArray, keyArg});

    llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "then", function);
    llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "merge", function);

    // Enter then block
    builder->CreateCondBr(builder->CreateICmpNE(keyExists, builder->getInt32(-1)), thenBlock, mergeBlock);
    builder->SetInsertPoint(thenBlock);
    // Index the value array

    llvm::Value *valuePtr = builder->CreateExtractValue(mapArg, 1);
    llvm::Value *loadedValuePtr = builder->CreateLoad(llvmCompiler->internalStructs["array"], valuePtr);
    llvm::Value *extractedArray = builder->CreateExtractValue(loadedValuePtr, 0);

    llvm::Value *value = builder->CreateInBoundsGEP(builder->getInt32Ty(), extractedArray, keyExists);
    builder->CreateRet(value);

    builder->SetInsertPoint(mergeBlock);
    llvm::Value *exitStr = builder->CreateGlobalString("Key didn't exist\n");
    builder->CreateCall(llvmCompiler->libraryFuncs["printf"], {exitStr});
    builder->CreateCall(llvmCompiler->libraryFuncs["exit"], {builder->getInt32(1)});
    builder->CreateRet(keyPtr);

    return function;
}

static llvm::Function *createIndexIntMap(LLVMCompiler *llvmCompiler, llvm::IRBuilder<> *llvmBuilder) {
    // Fix func type
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvmBuilder->getPtrTy(), {llvmCompiler->internalStructs["map"], llvm::Type::getInt32Ty(*llvmCompiler->ctx)},
        false);
    llvm::Function *function =
        llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "indexIntMap", *llvmCompiler->module);
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "entry", function);
    llvm::IRBuilder<> *builder = new llvm::IRBuilder<>(entryBlock);

    llvm::Function::arg_iterator arg = function->arg_begin();
    llvm::Value *mapArg = arg++;
    llvm::Value *keyArg = arg;

    llvm::Value *keyPtr = builder->CreateExtractValue(mapArg, 0);
    llvm::Value *loadedKeyArray = builder->CreateLoad(llvmCompiler->internalStructs["array"], keyPtr);

    llvm::Value *keyExists = builder->CreateCall(llvmCompiler->internalFuncs["findIntKey"], {loadedKeyArray, keyArg});

    llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "then", function);
    llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "merge", function);

    // Enter then block
    builder->CreateCondBr(builder->CreateICmpNE(keyExists, builder->getInt32(-1)), thenBlock, mergeBlock);
    builder->SetInsertPoint(thenBlock);
    // Index the value array

    llvm::Value *valuePtr = builder->CreateExtractValue(mapArg, 1);
    llvm::Value *loadedValuePtr = builder->CreateLoad(llvmCompiler->internalStructs["array"], valuePtr);
    llvm::Value *extractedArray = builder->CreateExtractValue(loadedValuePtr, 0);

    llvm::Value *value = builder->CreateInBoundsGEP(builder->getInt32Ty(), extractedArray, keyExists);
    builder->CreateRet(value);

    builder->SetInsertPoint(mergeBlock);
    llvm::Value *exitStr = builder->CreateGlobalString("Key didn't exist\n");
    builder->CreateCall(llvmCompiler->libraryFuncs["printf"], {exitStr});
    builder->CreateCall(llvmCompiler->libraryFuncs["exit"], {builder->getInt32(1)});
    builder->CreateRet(keyPtr);

    return function;
}

static llvm::Function *createFindStrKey(LLVMCompiler *llvmCompiler) {

    // Fix func type
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*llvmCompiler->ctx),
        {llvmCompiler->internalStructs["array"], llvmCompiler->internalStructs["array"]}, false);
    llvm::Function *function =
        llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "findStrKey", *llvmCompiler->module);
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "entry", function);
    llvm::IRBuilder<> *builder = new llvm::IRBuilder<>(entryBlock);
    llvm::Function::arg_iterator arg = function->arg_begin();
    llvm::Value *arrayArg = arg++;
    llvm::Value *arrayPtr = builder->CreateExtractValue(arrayArg, 0);
    llvm::Value *arraySize = builder->CreateExtractValue(arrayArg, 1);

    llvm::Value *key = arg;

    llvm::AllocaInst *loopVariable = builder->CreateAlloca(builder->getInt32Ty(), nullptr);
    builder->CreateStore(builder->getInt32(0), loopVariable);

    llvm::BasicBlock *headerBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "header", function);
    llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "body", function);
    llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "exit", function);

    builder->CreateBr(headerBlock);
    builder->SetInsertPoint(headerBlock);
    builder->CreateCondBr(builder->CreateICmpSLT(builder->CreateLoad(builder->getInt32Ty(), loopVariable), arraySize),
                          bodyBlock, exitBlock);
    builder->SetInsertPoint(bodyBlock);

    // Index the key array
    llvm::Value *loadedLoopVariable = builder->CreateLoad(builder->getInt32Ty(), loopVariable);
    llvm::Value *index = builder->CreateInBoundsGEP(builder->getPtrTy(), arrayPtr, loadedLoopVariable);
    llvm::Value *loadedKeyPtr = builder->CreateLoad(builder->getPtrTy(), index);
    llvm::Value *loadedKey = builder->CreateLoad(llvmCompiler->internalStructs["array"], loadedKeyPtr);

    llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "then", function);
    llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "merge", function);

    // Enter then block
    // This should be strcmp
    llvm::Value *key1Length = builder->CreateExtractValue(loadedKey, 1);
    llvm::Value *key2Length = builder->CreateExtractValue(key, 1);

    builder->CreateCondBr(builder->CreateICmpEQ(key1Length, key2Length), thenBlock, mergeBlock);
    builder->SetInsertPoint(thenBlock);

    llvm::BasicBlock *strCmpThenBlock1 = llvm::BasicBlock::Create(*llvmCompiler->ctx, "then", function);

    llvm::Value *key1StrPtr = builder->CreateExtractValue(loadedKey, 0);
    llvm::Value *key2StrPtr = builder->CreateExtractValue(key, 0);
    llvm::Value *memcmpValue =
        builder->CreateCall(llvmCompiler->libraryFuncs["memcmp"], {key1StrPtr, key2StrPtr, key1Length});
    builder->CreateCondBr(builder->CreateICmpEQ(memcmpValue, builder->getInt32(0)), strCmpThenBlock1, mergeBlock);
    builder->SetInsertPoint(strCmpThenBlock1);

    // Return the index of the key
    builder->CreateRet(loadedLoopVariable);

    builder->SetInsertPoint(mergeBlock);
    llvm::Value *newLoopVariable =
        builder->CreateAdd(builder->CreateLoad(builder->getInt32Ty(), loopVariable), builder->getInt32(1));
    builder->CreateStore(newLoopVariable, loopVariable);
    builder->CreateBr(headerBlock);

    builder->SetInsertPoint(exitBlock);
    builder->CreateRet(builder->getInt32(-1));

    return function;
}

static llvm::Function *createFindIntKey(LLVMCompiler *llvmCompiler) {

    // Fix func type
    llvm::FunctionType *funcType = llvm::FunctionType::get(
        llvm::Type::getInt32Ty(*llvmCompiler->ctx),
        {llvmCompiler->internalStructs["array"], llvm::Type::getInt32Ty(*llvmCompiler->ctx)}, false);
    llvm::Function *function =
        llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, "findIntKey", *llvmCompiler->module);
    llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "entry", function);
    llvm::IRBuilder<> *builder = new llvm::IRBuilder<>(entryBlock);

    llvm::Function::arg_iterator arg = function->arg_begin();
    llvm::Value *arrayArg = arg++;
    llvm::Value *arrayPtr = builder->CreateExtractValue(arrayArg, 0);
    llvm::Value *arraySize = builder->CreateExtractValue(arrayArg, 1);

    llvm::Value *key = arg;

    llvm::AllocaInst *loopVariable = builder->CreateAlloca(builder->getInt32Ty(), nullptr);
    builder->CreateStore(builder->getInt32(0), loopVariable);

    llvm::BasicBlock *headerBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "header", function);
    llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "body", function);
    llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "exit", function);

    builder->CreateBr(headerBlock);
    builder->SetInsertPoint(headerBlock);
    builder->CreateCondBr(builder->CreateICmpSLE(builder->CreateLoad(builder->getInt32Ty(), loopVariable), arraySize),
                          bodyBlock, exitBlock);
    builder->SetInsertPoint(bodyBlock);

    // Index the key array
    llvm::Value *loadedLoopVariable = builder->CreateLoad(builder->getInt32Ty(), loopVariable);
    llvm::Value *index = builder->CreateInBoundsGEP(builder->getInt32Ty(), arrayPtr, loadedLoopVariable);
    llvm::Value *loadedKey = builder->CreateLoad(builder->getInt32Ty(), index);

    llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "then", function);
    llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "merge", function);

    // Enter then block
    builder->CreateCondBr(builder->CreateICmpEQ(key, loadedKey), thenBlock, mergeBlock);
    builder->SetInsertPoint(thenBlock);
    builder->CreateRet(loadedLoopVariable);

    builder->SetInsertPoint(mergeBlock);
    llvm::Value *newLoopVariable =
        builder->CreateAdd(builder->CreateLoad(builder->getInt32Ty(), loopVariable), builder->getInt32(1));
    builder->CreateStore(newLoopVariable, loopVariable);
    builder->CreateBr(headerBlock);

    builder->SetInsertPoint(exitBlock);
    builder->CreateRet(builder->getInt32(-1));

    return function;
}

void addInternalFuncs(LLVMCompiler *llvmCompiler, llvm::IRBuilder<> *llvmBuilder) {

    llvmCompiler->internalFuncs = {};
    llvmCompiler->internalFuncs["findStrKey"] = createFindStrKey(llvmCompiler);
    llvmCompiler->internalFuncs["indexStrMap"] = createIndexStrMap(llvmCompiler, llvmBuilder);
    llvmCompiler->internalFuncs["findIntKey"] = createFindIntKey(llvmCompiler);
    llvmCompiler->internalFuncs["indexIntMap"] = createIndexIntMap(llvmCompiler, llvmBuilder);
}

void addLibraryFuncs(LLVMCompiler *llvmCompiler, llvm::IRBuilder<> *builder) {
    llvmCompiler->libraryFuncs = {};

    std::vector<llvm::Type *> args = {builder->getPtrTy()};
    llvm::FunctionType *type = llvm::FunctionType::get(builder->getVoidTy(), args, true);
    llvm::FunctionCallee func = llvmCompiler->module->getOrInsertFunction("printf", type);
    llvmCompiler->libraryFuncs["printf"] = func;

    args = {builder->getPtrTy(), builder->getPtrTy()};
    type = llvm::FunctionType::get(builder->getPtrTy(), args, true);
    func = llvmCompiler->module->getOrInsertFunction("strcat", type);
    llvmCompiler->libraryFuncs["strcat"] = func;

    args = {builder->getInt32Ty()};
    type = llvm::FunctionType::get(builder->getPtrTy(), args, true);
    func = llvmCompiler->module->getOrInsertFunction("malloc", type);
    llvmCompiler->libraryFuncs["malloc"] = func;

    args = {builder->getPtrTy(), builder->getPtrTy(), builder->getInt32Ty()};
    type = llvm::FunctionType::get(builder->getInt32Ty(), args, true);
    func = llvmCompiler->module->getOrInsertFunction("memcmp", type);
    llvmCompiler->libraryFuncs["memcmp"] = func;

    args = {builder->getInt32Ty()};
    type = llvm::FunctionType::get(builder->getVoidTy(), args, true);
    func = llvmCompiler->module->getOrInsertFunction("exit", type);
    llvmCompiler->libraryFuncs["exit"] = func;
}

void addInternalStructs(LLVMCompiler *llvmCompiler, llvm::IRBuilder<> *builder) {
    llvmCompiler->internalStructs = {};

    std::vector<llvm::Type *> fieldTypes = {builder->getPtrTy(), builder->getInt32Ty()};
    llvmCompiler->internalStructs["array"] = llvm::StructType::create(fieldTypes, "array");

    fieldTypes = {builder->getPtrTy(), builder->getPtrTy()};
    llvmCompiler->internalStructs["map"] = llvm::StructType::create(*llvmCompiler->ctx, fieldTypes, "map");
}
