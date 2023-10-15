#include "debug.h"
#include "library.h"

LLVMCompiler *llvmCompiler;
llvm::IRBuilder<> *builder;
LLVMFunction *llvmFunction;

static void errorAt(int line, const char *message, ...) {
    fprintf(stderr, "[line %d] Error", line);
    fprintf(stderr, ": %s\n", message);
    exit(1);
}
static llvm::Type *lookupArrayItemType(Variable *var) {
    switch (var->type) {
    case INT_VAR: {
        return builder->getInt32Ty();
    }
    case BOOL_VAR: {
        return builder->getInt1Ty();
    }
    case STR_VAR: {
        return builder->getInt8Ty();
    }
    case DOUBLE_VAR: {
        return builder->getDoubleTy();
    }
    case ARRAY_VAR: {
        ArrayVariable *arrayVariable = (ArrayVariable *)var;
        if (arrayVariable->items->type == ARRAY_VAR || arrayVariable->items->type == STR_VAR) {
            return llvmCompiler->internalStructs["array"];
        }
        return lookupArrayItemType(arrayVariable->items);
    }
    case STRUCT_VAR: {
        StructVariable *structVar = (StructVariable *)var;
        if (llvmCompiler->structs.count(structVar->structName)) {
            return llvmCompiler->structs[structVar->structName]->structType;
        }
        break;
    }
    case MAP_VAR: {
        return llvmCompiler->internalStructs["map"];
    }
    default: {
    }
    }
    errorAt(0, "Can't lookup this array item type?");
    exit(1);
}
static bool checkVariableValueMatch(Variable *var, llvm::Value *&value) {
    llvm::Type *type = value->getType();
    if (type == builder->getInt32Ty()) {
        if (var->type == DOUBLE_VAR) {
            value = builder->CreateUIToFP(value, builder->getDoubleTy());
            return true;
        }
        return var->type == INT_VAR;
    } else if (type == builder->getInt1Ty()) {
        return var->type == BOOL_VAR;
    } else if (type == builder->getDoubleTy()) {
        return var->type == DOUBLE_VAR;
    } else if (type->isStructTy()) {
        // ToDo llvmCompiler needs to check underlying type as well
        return var->type == STR_VAR || var->type == ARRAY_VAR || var->type == MAP_VAR || var->type == STRUCT_VAR;
    }
    return false;
}

static Variable *findVariableByName(std::string name) {
    for (int i = llvmCompiler->variables.size() - 1; i >= 0; i--) {
        std::map<std::string, Variable *> scope = llvmCompiler->variables[i];
        if (scope.count(name)) {
            return scope[name];
        }
    }
    errorAt(0, "Unable to find variable?\n");
    exit(1);
}

static llvm::Type *getTypeFromVariable(Variable *itemType) {
    if (itemType != nullptr) {
        switch (itemType->type) {
        case INT_VAR: {
            return builder->getInt32Ty();
        }
        case DOUBLE_VAR: {
            return builder->getDoubleTy();
        }
        case BOOL_VAR: {
            return builder->getInt1Ty();
        }
        case ARRAY_VAR: {
            return llvmCompiler->internalStructs["array"];
        }
        case STR_VAR: {
            return llvmCompiler->internalStructs["array"];
        }
        case STRUCT_VAR: {
            StructVariable *structVar = (StructVariable *)itemType;
            if (llvmCompiler->structs.count(structVar->structName)) {
                return llvmCompiler->structs[structVar->structName]->structType;
            }
            printf("trying to lookup unknown struct '%s'\n", structVar->structName.c_str());
            exit(1);
        }
        case MAP_VAR: {
            return llvmCompiler->internalStructs["map"];
        }
        default: {
        }
        }
    }
    return nullptr;
}

void initCompiler(std::vector<std::map<std::string, Variable *>> variables) {
    llvmCompiler = new LLVMCompiler;
    llvmCompiler->variables = variables;
    llvmCompiler->ctx = new llvm::LLVMContext();
    llvmCompiler->module = new llvm::Module("Bonobo", *llvmCompiler->ctx);
    llvmCompiler->callableFunctions = std::vector<llvm::Function *>();
    llvmCompiler->structs = std::map<std::string, LLVMStruct *>();
    llvmCompiler->strings = {};

    llvm::FunctionType *funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*llvmCompiler->ctx), false);
    llvmFunction = new LLVMFunction(nullptr, funcType, "main", {}, llvmCompiler->ctx, llvmCompiler->module);
    builder = new llvm::IRBuilder<>(llvmFunction->entryBlock);
    addLibraryFuncs(llvmCompiler, builder);
    addInternalStructs(llvmCompiler, builder);
    addInternalFuncs(llvmCompiler, builder);
}

static void endCompiler() {
    builder->CreateRet(builder->getInt32(0));

    std::error_code errorCode;
    llvm::raw_fd_ostream outLL("./out.ll", errorCode);
    llvmCompiler->module->print(outLL, nullptr);
}

llvm::Value *callMalloc(llvm::Value *size) { return builder->CreateCall(llvmCompiler->libraryFuncs["malloc"], {size}); }

llvm::Value *callMalloc(int size) {
    return builder->CreateCall(llvmCompiler->libraryFuncs["malloc"], {builder->getInt32(size)});
}

static bool nameIsAlreadyDeclared(std::string name) {
    // Check variables
    std::vector<llvm::AllocaInst *> lastScope = llvmFunction->scopedVariables.back();
    for (auto &var : lastScope) {
        if (var->getName().str() == name) {
            return true;
        }
    }
    // Check structs
    if (llvmCompiler->structs.count(name)) {
        return true;
    }
    // Check library functions
    if (llvmCompiler->libraryFuncs.count(name)) {
        return true;
    }
    // Check user declared functions
    for (auto &func : llvmCompiler->callableFunctions) {
        if (func->getName().str() == name) {
            return true;
        }
    }

    return false;
}

// Returns true if you return inside of the branch
static bool compileIfBranch(std::vector<Stmt *> branch) {
    for (auto &stmt : branch) {
        if (stmt->type == BREAK_STMT) {
            llvmFunction->broke = true;
            builder->CreateBr(llvmFunction->exitBlock->exitBlock);
            return false;
        }
        compileStatement(stmt);
        if (stmt->type == RETURN_STMT) {
            return true;
        }
    }
    return false;
}

static void enterMergeBlock(bool returned, llvm::BasicBlock *mergeBlock) {
    llvmFunction->scopedVariables.pop_back();
    if (!returned && !llvmFunction->broke) {
        builder->CreateBr(mergeBlock);
    }
    builder->SetInsertPoint(mergeBlock);
}

static void enterElseBlock(bool returned, llvm::BasicBlock *elseBlock, llvm::BasicBlock *mergeBlock) {}

static llvm::IRBuilder<> *enterFuncScope(FuncStmt *funcStmt) {
    // Fix params
    std::vector<llvm::Type *> params = std::vector<llvm::Type *>(funcStmt->params.size());
    std::map<std::string, int> funcArgs;
    for (int i = 0; i < funcStmt->params.size(); ++i) {
        params[i] = getTypeFromVariable(funcStmt->params[i]);
        funcArgs[funcStmt->params[i]->name] = i;
    }

    // Ret type
    llvm::Type *returnType = getTypeFromVariable(funcStmt->returnType);

    // Fix func type
    llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, params, false);
    llvmFunction =
        new LLVMFunction(llvmFunction, funcType, funcStmt->name, funcArgs, llvmCompiler->ctx, llvmCompiler->module);
    llvm::IRBuilder<> *prevBuilder = builder;
    builder = new llvm::IRBuilder<>(llvmFunction->entryBlock);
    return prevBuilder;
}

static llvm::MaybeAlign getAlignment(llvm::Type *type) { return llvm::MaybeAlign(type->getPrimitiveSizeInBits() / 8); }

static llvm::Function *lookupFunction(std::string name) {
    for (auto &func : llvmCompiler->callableFunctions) {
        if (func->getName() == name) {
            return func;
        }
    }
    return llvmFunction->function->getName() == name ? llvmFunction->function : nullptr;
}

static llvm::Value *lookupValue(std::string name, int line) {
    if (llvmFunction->enclosing && llvmFunction->functionArguments.count(name)) {
        int i = 0;
        for (llvm::Function::arg_iterator arg = llvmFunction->function->arg_begin();
             arg != llvmFunction->function->arg_end(); ++arg) {
            if (i == llvmFunction->functionArguments[name]) {
                return arg;
            }
            ++i;
        }
    }
    for (int i = llvmFunction->scopedVariables.size() - 1; i >= 0; i--) {
        std::vector<llvm::AllocaInst *> scopeVars = llvmFunction->scopedVariables[i];

        for (int j = 0; j < scopeVars.size(); ++j) {
            if (scopeVars[j]->getName().str() == name) {
                return scopeVars[j];
            }
        }
    }
    errorAt(line, ("Unknown variable " + name).c_str());
    exit(1);
}

static void compileLoopExit(llvm::BasicBlock *headerBlock, llvm::BasicBlock *exitBlock, Stmt *stmt = nullptr) {
    if (!llvmFunction->broke) {
        if (stmt != nullptr) {
            compileStatement(stmt);
        }
        llvmFunction->broke = false;
        builder->CreateBr(headerBlock);
    }

    llvmFunction->exitBlock = llvmFunction->exitBlock->prev;
    builder->SetInsertPoint(exitBlock);
}

static void compileLoopBody(llvm::BasicBlock *headerBlock, llvm::BasicBlock *exitBlock, std::vector<Stmt *> body) {
    for (auto &stmt : body) {
        if (stmt->type == BREAK_STMT) {
            llvmFunction->broke = true;
            builder->CreateBr(exitBlock);
            break;
        }
        compileStatement(stmt);
    }
}

static void compileLoopHeader(llvm::BasicBlock *headerBlock, llvm::BasicBlock *exitBlock, llvm::BasicBlock *bodyBlock,
                              Expr *condition) {
    builder->CreateBr(headerBlock);
    builder->SetInsertPoint(headerBlock);

    llvmFunction->exitBlock = new ExitBlock(llvmFunction->exitBlock, exitBlock);
    builder->CreateCondBr(compileExpression(condition), bodyBlock, exitBlock);
    builder->SetInsertPoint(bodyBlock);
}

static void checkCallParamCorrectness(llvm::Function *func, std::vector<llvm::Value *> params, std::string funcName) {
    if (func == nullptr) {
        printf("calling unknown func '%s'\n", funcName.c_str());
        exit(1);
    }
    if (((int)func->arg_size()) != params.size()) {
        printf("Calling %s requires %d params but got %d\n", funcName.c_str(), (int)func->arg_size(),
               (int)params.size());
    }
    int i = 0;
    for (llvm::Argument &arg : func->args()) {
        if (arg.getType() != params[i]->getType()) {
            printf("Invalid arg type in function %s with arg %d\n", funcName.c_str(), i + 1);
        }
        ++i;
    }
}

static void storeStructField(llvm::StructType *structType, llvm::Value *structInstance, llvm::Value *toStore,
                             uint field) {
    llvm::Value *gep = builder->CreateStructGEP(structType, structInstance, field);
    builder->CreateStore(toStore, gep);
}

static void storeArraySizeInStruct(llvm::Value *size, llvm::Value *arrayInstance) {
    storeStructField(llvmCompiler->internalStructs["array"], arrayInstance, size, 1);
}

static void storeArrayInStruct(llvm::Value *arrayToStore, llvm::Value *arrayInstance) {
    storeStructField(llvmCompiler->internalStructs["array"], arrayInstance, arrayToStore, 0);
}

static llvm::Value *compileLiteral(LiteralExpr *expr) {
    std::string stringLiteral = expr->literal;
    switch (expr->literalType) {
    case STR_LITERAL: {

        llvm::AllocaInst *stringInstance =
            builder->CreateAlloca(llvmCompiler->internalStructs["array"], nullptr, "string");
        llvmCompiler->strings.push_back(stringInstance);

        storeArrayInStruct(builder->CreateGlobalString(stringLiteral), stringInstance);
        storeArraySizeInStruct(builder->getInt32(stringLiteral.size() + 1), stringInstance);

        return stringInstance;
    }
    case INT_LITERAL: {
        return builder->getInt32(stoi(stringLiteral));
    }
    case BOOL_LITERAL: {
        return stringLiteral == "true" ? builder->getInt1(1) : builder->getInt1(0);
    }
    case DOUBLE_LITERAL: {
        return llvm::ConstantFP::get(builder->getDoubleTy(), stod(stringLiteral));
    }
    }
}

static llvm::Value *loadArrayFromArrayStruct(llvm::Value *arrayPtr) {
    llvm::Value *ptr = builder->CreateStructGEP(llvmCompiler->internalStructs["array"], arrayPtr, 0);
    return builder->CreateLoad(builder->getPtrTy(), ptr);
}

static llvm::Value *loadArraySizeFromArrayStruct(llvm::Value *arrayPtr) {
    llvm::Value *ptr = builder->CreateStructGEP(llvmCompiler->internalStructs["array"], arrayPtr, 1);
    return builder->CreateLoad(builder->getInt32Ty(), ptr);
}

static llvm::Value *loadAllocaInst(llvm::Value *value) {
    if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
        return builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
    }
    return value;
}

static uint32_t getStructSize(llvm::StructType *structType) {
    uint32_t size = 0;
    for (auto &subtype : structType->subtypes()) {
        size += subtype->isStructTy() ? getStructSize(llvm::dyn_cast<llvm::StructType>(subtype))
                                      : subtype->getPrimitiveSizeInBits();
    }
    return size;
}

static llvm::Value *getArraySizeInBytes(llvm::Type *itemType, llvm::Value *arraySize) {
    uint32_t size = itemType->isStructTy() ? size = getStructSize(llvm::dyn_cast<llvm::StructType>(itemType))
                                           : itemType->getPrimitiveSizeInBits() / 8;

    return builder->CreateMul(arraySize, builder->getInt32(size));
}

static void copyArray(llvm::AllocaInst *allocaVar, llvm::Value *value, Variable *var) {
    llvm::Value *sourceArraySize = builder->CreateExtractValue(value, 1);
    llvm::Value *sourceArrayPtr = builder->CreateExtractValue(value, 0);
    //
    // ToDo, if itemType is array, then recursively call this :)
    llvm::Type *itemType = lookupArrayItemType(var);
    llvm::Value *arraySize = getArraySizeInBytes(itemType, sourceArraySize);

    builder->CreateMul(sourceArraySize, builder->getInt32(itemType->getPrimitiveSizeInBits() / 8));

    llvm::Value *arrayAllocation = callMalloc(arraySize);
    builder->CreateMemCpy(arrayAllocation, llvm::MaybeAlign(4), sourceArrayPtr, llvm::MaybeAlign(4), arraySize);

    storeArrayInStruct(arrayAllocation, allocaVar);
    storeArraySizeInStruct(sourceArraySize, allocaVar);
}

static void copyAllocation(llvm::AllocaInst *destination, llvm::AllocaInst *source, Variable *var) {

    if (source->getAllocatedType()->isStructTy() && var->type != STRUCT_VAR) {
        copyArray(destination, builder->CreateLoad(source->getAllocatedType(), source), var);
    } else {
        builder->CreateMemCpy(destination, llvm::MaybeAlign(8), source, llvm::MaybeAlign(8), 16);
    }
}

static void storeArrayAtIndex(llvm::Value *value, llvm::Value *arrayPtr, int idx) {
    llvm::Value *arrayInboundPtr = builder->CreateInBoundsGEP(builder->getPtrTy(), arrayPtr, builder->getInt32(idx));
    builder->CreateStore(value, arrayInboundPtr);
}

static llvm::GlobalVariable *createGlobalArray(std::vector<llvm::Value *> arrayItems, llvm::ArrayType *arrayType) {
    std::vector<llvm::Constant *> constArrayItems = std::vector<llvm::Constant *>(arrayItems.size());
    for (uint64_t i = 0; i < arrayItems.size(); ++i) {
        constArrayItems[i] = llvm::dyn_cast<llvm::Constant>(arrayItems[i]);
    }

    return new llvm::GlobalVariable(*llvmCompiler->module, arrayType, false, llvm::GlobalValue::PrivateLinkage,
                                    llvm::ConstantArray::get(arrayType, constArrayItems));
}

static bool isStringTy(llvm::Value *value) {
    for (auto &str : llvmCompiler->strings) {
        if (str == value) {
            return true;
        }
    }
    return false;
}

static llvm::Value *concatStrings(llvm::Value *left, llvm::Value *right) {
    llvm::StructType *stringStruct = llvmCompiler->internalStructs["array"];

    llvm::Value *leftSize = loadArraySizeFromArrayStruct(left);
    llvm::Value *newSize = builder->CreateAdd(leftSize, loadArraySizeFromArrayStruct(right));

    llvm::AllocaInst *concStringInstance = builder->CreateAlloca(stringStruct, nullptr, "string");
    llvm::Value *mallocResult = builder->CreateCall(llvmCompiler->libraryFuncs["malloc"], {newSize});

    storeArraySizeInStruct(newSize, concStringInstance);
    storeArrayInStruct(mallocResult, concStringInstance);

    builder->CreateMemCpy(mallocResult, llvm::MaybeAlign(1), loadArrayFromArrayStruct(left), llvm::MaybeAlign(1),
                          leftSize);
    builder->CreateCall(llvmCompiler->libraryFuncs["strcat"], {mallocResult, loadArrayFromArrayStruct(right)});

    return concStringInstance;
}

static llvm::Value *createStruct(CallExpr *callExpr) {
    std::string name = callExpr->callee;
    LLVMStruct *strukt = llvmCompiler->structs[name];
    llvm::AllocaInst *structInstance = builder->CreateAlloca(strukt->structType, nullptr, name);

    if (callExpr->arguments.size() != strukt->fields.size()) {
        printf("Strukt has different amount of args, expected: "
               "%d but got %d",
               (int)strukt->fields.size(), (int)callExpr->arguments.size());
        exit(1);
    }

    for (int i = 0; i < callExpr->arguments.size(); ++i) {
        llvm::Value *paramValue = compileExpression(callExpr->arguments[i]);
        if (strukt->structType->getContainedType(i) != paramValue->getType()) {
            printf("Param %d does match it's type\n", i);
            exit(1);
        }
        storeStructField(strukt->structType, structInstance, paramValue, i);
    }
    return structInstance;
}

static void castIntDouble(llvm::Value *&left, llvm::Value *&right) {
    if (left->getType()->isIntegerTy()) {
        left = builder->CreateUIToFP(left, builder->getDoubleTy());
    } else if (right->getType()->isIntegerTy()) {
        right = builder->CreateUIToFP(right, builder->getDoubleTy());
    }
}

static llvm::Value *binaryOp(llvm::Value *left, llvm::Value *right, BinaryOp op, int line) {
    left = loadAllocaInst(left);
    right = loadAllocaInst(right);

    if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) {
        switch (op) {
        case ADD: {
            return builder->CreateAdd(left, right);
        }
        case SUB: {
            return builder->CreateSub(left, right);
        }
        case MUL: {
            return builder->CreateMul(left, right);
        }
        case DIV: {
            return builder->CreateUDiv(left, right);
        }
        }
    }

    castIntDouble(left, right);
    if (left->getType()->isDoubleTy() && right->getType()->isDoubleTy()) {
        switch (op) {
        case ADD: {
            return builder->CreateFAdd(left, right);
        }
        case SUB: {
            return builder->CreateFSub(left, right);
        }
        case MUL: {
            return builder->CreateFMul(left, right);
        }
        case DIV: {
            return builder->CreateFDiv(left, right);
        }
        }
    }
    errorAt(line, "Can't do binary op");
    exit(1);
}
static void checkIndexOutOfBounds(llvm::Value *loadedArrayStruct, llvm::Value *index) {

    // Check here whether or not it's out of bounds
    //    Just if index >= size
    //        then branch is just exiting?
    llvm::Value *arraySize = builder->CreateExtractValue(loadedArrayStruct, 1);
    // Check less then array size
    llvm::Value *condition1 = builder->CreateICmpSGE(index, arraySize);
    llvm::BasicBlock *thenBlock1 = llvm::BasicBlock::Create(*llvmCompiler->ctx, "then", llvmFunction->function);
    llvm::BasicBlock *mergeBlock1 = llvm::BasicBlock::Create(*llvmCompiler->ctx, "merge", llvmFunction->function);

    builder->CreateCondBr(condition1, thenBlock1, mergeBlock1);

    builder->SetInsertPoint(thenBlock1);
    llvm::Value *exitString = builder->CreateGlobalStringPtr("Trying to index outside of array\nsize: %d\nidx: %d\n");
    builder->CreateCall(llvmCompiler->libraryFuncs["printf"], {exitString, arraySize, index});
    builder->CreateRet(builder->getInt32(1));

    builder->SetInsertPoint(mergeBlock1);

    // Check not negative
    llvm::Value *condition2 = builder->CreateICmpSLT(index, builder->getInt32(0));
    llvm::BasicBlock *mergeBlock2 = llvm::BasicBlock::Create(*llvmCompiler->ctx, "merge", llvmFunction->function);
    builder->CreateCondBr(condition2, thenBlock1, mergeBlock2);
    builder->SetInsertPoint(mergeBlock2);
}

static llvm::Value *getArrayIndex(llvm::Type *type, llvm::Value *loadedArrayStruct, llvm::Value *index) {
    if (type == llvmCompiler->internalStructs["array"] || type->isStructTy()) {
        type = builder->getPtrTy();
    }
    checkIndexOutOfBounds(loadedArrayStruct, index);

    return builder->CreateInBoundsGEP(type, builder->CreateExtractValue(loadedArrayStruct, 0), index);
}

static llvm::Value *indexMap(llvm::Value *map, llvm::Value *index, Variable *var) {
    MapVariable *mapVar = (MapVariable *)var;
    if (mapVar->keys->type == STR_VAR) {
        return builder->CreateCall(llvmCompiler->internalFuncs["indexStrMap"],
                                   {map, builder->CreateLoad(llvmCompiler->internalStructs["array"], index)});
    } else {
        return builder->CreateCall(llvmCompiler->internalFuncs["indexIntMap"], {map, index});
    }
}

static llvm::Value *getIndexValue(IndexExpr *indexExpr, Variable *&var) {
    ExprType varType = indexExpr->variable->type;
    if (varType == INDEX_EXPR) {
        return loadIndex((IndexExpr *)indexExpr->variable, var);
    } else if (varType == VAR_EXPR) {
        VarExpr *varExpr = (VarExpr *)indexExpr->variable;
        var = findVariableByName(varExpr->name);
        return compileExpression(varExpr);
    } else if (varType == CALL_EXPR) {
        CallExpr *callExpr = (CallExpr *)indexExpr->variable;

        FuncVariable *funcVar = (FuncVariable *)findVariableByName(callExpr->callee);
        var = funcVar->returnType;
        return compileExpression(callExpr);
    }
    errorAt(indexExpr->line, "Can't index this type?");
    exit(1);
}

static llvm::Value *getPointerToArrayIndex(IndexExpr *indexExpr, Variable *&var) {
    // This should be a func that also checks out of bounds
    llvm::Value *indexValue = getIndexValue(indexExpr, var);
    llvm::Value *index = compileExpression(indexExpr->index);

    if (llvm::AllocaInst *castedVar = llvm::dyn_cast<llvm::AllocaInst>(indexValue)) {
        var = findVariableByName(castedVar->getName().str());
        if (castedVar->getAllocatedType() == llvmCompiler->internalStructs["map"]) {
            indexValue = builder->CreateLoad(llvmCompiler->internalStructs["map"], castedVar);
        } else if (castedVar->getAllocatedType() == llvmCompiler->internalStructs["array"]) {
            return getArrayIndex(lookupArrayItemType(var),
                                 builder->CreateLoad(castedVar->getAllocatedType(), castedVar), index);
        }
    }

    if (indexValue->getType() == llvmCompiler->internalStructs["array"]) {
        var = ((ArrayVariable *)var)->items;
        return getArrayIndex(lookupArrayItemType(var), indexValue, index);

    } else if (indexValue->getType() == llvmCompiler->internalStructs["map"]) {
        return indexMap(indexValue, index, var);
    }

    errorAt(indexExpr->line,
            ("Couldn't cast index variable, was type: " + debugValueType(indexValue->getType(), llvmCompiler->ctx))
                .c_str());
    exit(1);
}

llvm::Value *loadIndex(IndexExpr *indexExpr, Variable *&var) {
    llvm::Value *idxPtr = getPointerToArrayIndex(indexExpr, var);
    if (var->type == MAP_VAR) {
        llvm::Type *type = getTypeFromVariable(indexExpr->evaluatesTo);
        return builder->CreateLoad(type, idxPtr);
    }

    llvm::Type *arrayItemType = lookupArrayItemType(var);

    if (var->type == ARRAY_VAR) {
        ArrayVariable *arrayVar = (ArrayVariable *)var;
        llvm::Value *loadedPtr = builder->CreateLoad(builder->getPtrTy(), idxPtr);
        if (arrayVar->items->type == ARRAY_VAR || arrayVar->items->type == STR_VAR) {
            llvm::Value *loadedArrayPtr =
                builder->CreateInBoundsGEP(arrayItemType, loadedPtr, {builder->getInt32(0), builder->getInt32(0)});
            return builder->CreateLoad(arrayItemType, loadedArrayPtr);
        }
        if (arrayVar->items->type == STRUCT_VAR || arrayVar->items->type == MAP_VAR) {
            llvm::Value *loadedStructPtr = builder->CreateLoad(builder->getPtrTy(), idxPtr);
            return builder->CreateLoad(arrayItemType, loadedStructPtr);
        }
    }
    return builder->CreateLoad(arrayItemType, idxPtr);
}

static llvm::Type *getTypeFromNestedIndexExpr(Expr *expr) {
    if (expr->type == VAR_EXPR) {
        VarExpr *varExpr = (VarExpr *)expr;
        ArrayVariable *var = (ArrayVariable *)findVariableByName(varExpr->name);
        while (true) {
            ArrayVariable *items = (ArrayVariable *)var->items;
            if (items->items->type != ARRAY_VAR) {
                return lookupArrayItemType(items->items);
            }
            items = (ArrayVariable *)items->items;
        }
    }
    IndexExpr *indexExpr = (IndexExpr *)expr;
    return getTypeFromNestedIndexExpr(indexExpr->variable);
}

static void assignToIndexExpr(AssignStmt *assignStmt) {
    IndexExpr *indexExpr = (IndexExpr *)assignStmt->variable;
    Variable *var = new Variable();
    // Check whether it's a map
    // Check whether key exists,
    //    If it does then just replace at the index
    //    If it doesn't, append both keys and values array
    builder->CreateStore(compileExpression(assignStmt->value), getPointerToArrayIndex(indexExpr, var));
}

static void assignToVarExpr(AssignStmt *assignStmt) {
    llvm::Value *value = compileExpression(assignStmt->value);
    VarExpr *varExpr = (VarExpr *)assignStmt->variable;
    llvm::Value *variable = lookupValue(varExpr->name, varExpr->line);
    VarType evalType = varExpr->evaluatesTo->type;
    if (evalType == ARRAY_VAR || evalType == STR_VAR) {
        llvm::AllocaInst *allocVar = llvm::dyn_cast<llvm::AllocaInst>(variable);
        llvm::Value *loadedValue = builder->CreateLoad(llvmCompiler->internalStructs["array"], value);
        Variable *var = findVariableByName(varExpr->name);
        copyArray(allocVar, loadedValue, var);
        return;
    }

    builder->CreateStore(value, variable);
}

static llvm::Value *lookupStruct(DotExpr *dotExpr) {
    llvm::Value *value = compileExpression(dotExpr->name);

    if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
        return builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
    }

    return value;
}

static void storeArray(llvm::Type *elementType, llvm::AllocaInst *arrayInstance,
                       std::vector<llvm::Value *> arrayItems) {
    if (elementType == llvmCompiler->internalStructs["array"] || elementType->isStructTy()) {

        storeArrayInStruct(callMalloc(arrayItems.size() * 8), arrayInstance);
        for (int i = 0; i < arrayItems.size(); ++i) {
            storeArrayAtIndex(arrayItems[i], loadArrayFromArrayStruct(arrayInstance), i);
        }

    } else {
        llvm::ArrayType *arrayType = llvm::ArrayType::get(elementType, arrayItems.size());
        llvm::GlobalVariable *globalArray = createGlobalArray(arrayItems, arrayType);

        storeArrayInStruct(builder->CreateInBoundsGEP(arrayType, globalArray, builder->getInt32(0)), arrayInstance);
    }
    storeArraySizeInStruct(builder->getInt32(arrayItems.size()), arrayInstance);
}

llvm::AllocaInst *createAndStoreArray(llvm::Type *type, std::vector<llvm::Value *> items) {
    llvm::AllocaInst *instance = builder->CreateAlloca(llvmCompiler->internalStructs["array"], nullptr, "array");
    storeArray(type, instance, items);

    return instance;
}

static std::string findStructName(Expr *expr) {
    switch (expr->type) {
    case DOT_EXPR: {
        DotExpr *dotExpr = (DotExpr *)expr;
        return findStructName(dotExpr->name);
    }
    case INDEX_EXPR: {
        IndexExpr *indexExpr = (IndexExpr *)expr;
        return findStructName(indexExpr->variable);
    }
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        Variable *var = findVariableByName(varExpr->name);
        while (var->type == ARRAY_VAR) {
            ArrayVariable *arrayVar = (ArrayVariable *)var;
            var = arrayVar->items;
        }
        if (var->type != STRUCT_VAR) {
            printf("wasn't struct?\n");
            debugVariable(var);
            printf("\n");
            exit(1);
        }
        StructVariable *struktVar = (StructVariable *)var;
        return struktVar->structName;
    }
    default: {
        printf("Don't know how to find struct name for: ");
        debugExpression(expr);
        exit(1);
    }
    }
}

static void assignToDotExpr(AssignStmt *assignStmt) {
    DotExpr *dotExpr = (DotExpr *)assignStmt->variable;
    std::string structName = findStructName(dotExpr);
    llvm::Value *structPtr = nullptr;

    if (dotExpr->name->type == INDEX_EXPR) {
        Variable *var = findVariableByName(structName);
        structPtr = builder->CreateLoad(builder->getPtrTy(), getPointerToArrayIndex((IndexExpr *)dotExpr->name, var));
    } else {
        structPtr = compileExpression(dotExpr->name);
    }

    llvm::Value *value = compileExpression(assignStmt->value);

    if (LLVMStruct *strukt = llvmCompiler->structs[structName]) {
        llvm::StructType *structType = strukt->structType;
        for (int j = 0; j < strukt->fields.size(); j++) {
            if (strukt->fields[j] == dotExpr->field) {
                storeStructField(structType, structPtr, value, j);
                return;
            }
        }
    }
}

llvm::Value *compileExpression(Expr *expr) {
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;

        llvm::Value *left = compileExpression(binaryExpr->left);
        llvm::Value *right = compileExpression(binaryExpr->right);

        if (isStringTy(left) && isStringTy(right)) {
            return concatStrings(left, right);
        }
        return binaryOp(left, right, binaryExpr->op, binaryExpr->line);
    }
    case GROUPING_EXPR: {
        GroupingExpr *groupingExpr = (GroupingExpr *)expr;
        return compileExpression(groupingExpr->expression);
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;

        llvm::Value *left = loadAllocaInst(compileExpression(logicalExpr->left));
        llvm::Value *right = loadAllocaInst(compileExpression(logicalExpr->right));

        if (left->getType() == builder->getInt1Ty() && right->getType() == left->getType()) {
            switch (logicalExpr->op) {
            case OR_LOGICAL: {
                return builder->CreateLogicalOr(left, right);
            }
            case AND_LOGICAL: {
                return builder->CreateLogicalAnd(left, right);
            }
            }
        }
    }
    case LITERAL_EXPR: {
        return compileLiteral((LiteralExpr *)expr);
    }
    case DOT_EXPR: {
        DotExpr *dotExpr = (DotExpr *)expr;
        llvm::Value *value = lookupStruct(dotExpr);
        if (LLVMStruct *strukt = llvmCompiler->structs[value->getType()->getStructName().str()]) {
            for (int i = 0; i < strukt->fields.size(); i++) {
                if (strukt->fields[i] == dotExpr->field) {
                    return builder->CreateExtractValue(value, i);
                }
            }
        }
        errorAt(0, "Unable to lookup struct for dotExpr?");
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;

        llvm::Value *left = loadAllocaInst(compileExpression(comparisonExpr->left));
        llvm::Value *right = loadAllocaInst(compileExpression(comparisonExpr->right));

        // Need to check fp as well, string equality, array equality,
        // map equality
        if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) {

            switch (comparisonExpr->op) {
            case LESS_EQUAL_COMPARISON: {
                return builder->CreateICmpULE(left, right);
            }
            case LESS_COMPARISON: {
                return builder->CreateICmpULT(left, right);
            }
            case GREATER_COMPARISON: {
                return builder->CreateICmpUGT(left, right);
            }
            case GREATER_EQUAL_COMPARISON: {
                return builder->CreateICmpUGE(left, right);
            }
            case EQUAL_EQUAL_COMPARISON: {
                return builder->CreateICmpEQ(left, right);
            }
            }
        }
        castIntDouble(left, right);
        if (left->getType()->isDoubleTy() && right->getType()->isDoubleTy()) {
            switch (comparisonExpr->op) {
            case LESS_EQUAL_COMPARISON: {
                return builder->CreateFCmpULE(left, right);
            }
            case LESS_COMPARISON: {
                return builder->CreateFCmpULT(left, right);
            }
            case GREATER_COMPARISON: {
                return builder->CreateFCmpUGT(left, right);
            }
            case GREATER_EQUAL_COMPARISON: {
                return builder->CreateFCmpUGE(left, right);
            }
            case EQUAL_EQUAL_COMPARISON: {
                return builder->CreateFCmpOEQ(left, right);
            }
            }
        }
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        llvm::Value *value = loadAllocaInst(compileExpression(unaryExpr->right));
        if (unaryExpr->op == NEG_UNARY && (value->getType()->isIntegerTy() || value->getType()->isDoubleTy())) {
            return builder->CreateMul(value, builder->getInt32(-1));
        } else {
            // Check value type?
            return builder->CreateXor(value, 1);
        }
    }
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        return lookupValue(varExpr->name, varExpr->line);
    }
    case INDEX_EXPR: {
        // ToDo if string -> create new one
        IndexExpr *indexExpr = (IndexExpr *)expr;
        Variable *var = new Variable();
        return loadIndex(indexExpr, var);
    }
    case INC_EXPR: {
        IncExpr *incExpr = (IncExpr *)expr;
        llvm::Value *value = compileExpression(incExpr->expr);
        if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            llvm::Value *loadedValue = builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
            llvm::Value *valueOp = nullptr;
            if (incExpr->op == INC) {
                valueOp = builder->CreateAdd(loadedValue, builder->getInt32(1));
            } else {
                valueOp = builder->CreateSub(loadedValue, builder->getInt32(1));
            }
            return builder->CreateStore(valueOp, value);
        }
    }
    case ARRAY_EXPR: {
        ArrayExpr *arrayExpr = (ArrayExpr *)expr;

        llvm::Type *elementType = getTypeFromVariable(arrayExpr->itemType);
        std::vector<llvm::Value *> arrayItems(arrayExpr->items.size());

        for (int i = 0; i < arrayItems.size(); ++i) {
            llvm::Value *item = compileExpression(arrayExpr->items[i]);
            llvm::Type *itemType = item->getType();

            if (elementType == nullptr) {
                elementType = itemType;
            }

            arrayItems[i] = item;
        }

        llvm::AllocaInst *arrayInstance =
            builder->CreateAlloca(llvmCompiler->internalStructs["array"], nullptr, "array");
        storeArray(elementType, arrayInstance, arrayItems);

        return arrayInstance;
    }
    case MAP_EXPR: {
        MapExpr *mapExpr = (MapExpr *)expr;

        std::vector<llvm::Value *> keys = std::vector<llvm::Value *>(mapExpr->keys.size());
        std::vector<llvm::Value *> values = std::vector<llvm::Value *>(mapExpr->values.size());

        // ToDo type check this
        for (int i = 0; i < keys.size(); ++i) {
            keys[i] = compileExpression(mapExpr->keys[i]);
            values[i] = compileExpression(mapExpr->values[i]);
        }

        MapVariable *var = (MapVariable *)mapExpr->mapVar;
        llvm::Type *valueType = nullptr;
        llvm::Type *keyType = nullptr;

        if (var != nullptr) {
            valueType = getTypeFromVariable(var->values);
            keyType = getTypeFromVariable(var->keys);
        }

        for (int i = 0; i < keys.size(); ++i) {
            if (var == nullptr) {
                valueType = values[i]->getType();
                keyType = keys[i]->getType();
            }
        }

        llvm::AllocaInst *mapInstance = builder->CreateAlloca(llvmCompiler->internalStructs["map"], nullptr, "map");

        storeStructField(llvmCompiler->internalStructs["map"], mapInstance, createAndStoreArray(keyType, keys), 0);
        storeStructField(llvmCompiler->internalStructs["map"], mapInstance, createAndStoreArray(valueType, values), 1);

        return mapInstance;
    }
    case CALL_EXPR: {
        CallExpr *callExpr = (CallExpr *)expr;
        int argSize = callExpr->arguments.size();
        std::string name = callExpr->callee;

        if (llvmCompiler->structs.count(name)) {
            return createStruct(callExpr);
        }

        std::vector<llvm::Value *> params = std::vector<llvm::Value *>(argSize);
        for (int i = 0; i < argSize; ++i) {
            params[i] = compileExpression(callExpr->arguments[i]);
        }
        if (llvmCompiler->internalFuncs.count(name)) {
            return builder->CreateCall(llvmCompiler->internalFuncs[name], params);
        }

        if (llvmCompiler->libraryFuncs.count(name)) {
            for (int i = 0; i < argSize; ++i) {
                if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(params[i])) {
                    if (allocaInst->getAllocatedType()->isStructTy()) {
                        params[i] = loadArrayFromArrayStruct(allocaInst);
                    } else {
                        params[i] = builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                    }
                }
            }
            return builder->CreateCall(llvmCompiler->libraryFuncs[name], params);
        }

        llvm::Function *func = lookupFunction(name);
        checkCallParamCorrectness(func, params, name);
        return builder->CreateCall(func, params);
    }
    }
}

void compileStatement(Stmt *stmt) {
    switch (stmt->type) {
    case EXPR_STMT: {
        ExprStmt *exprStmt = (ExprStmt *)stmt;
        compileExpression(exprStmt->expression);
        break;
    }
    case COMP_ASSIGN_STMT: {
        CompAssignStmt *compStmt = (CompAssignStmt *)stmt;
        // ToDo Check this?
        llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(lookupValue(compStmt->name, compStmt->line));

        llvm::Value *variable = builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
        builder->CreateStore(binaryOp(compileExpression(compStmt->right), variable, compStmt->op, compStmt->line),
                             allocaInst);
        break;
    }
    case BREAK_STMT: {
        errorAt(stmt->line, "How did you get here?\n");
    }
    case ASSIGN_STMT: {
        AssignStmt *assignStmt = (AssignStmt *)stmt;
        switch (assignStmt->variable->type) {
        case VAR_EXPR: {
            assignToVarExpr(assignStmt);
            return;
        }
        case INDEX_EXPR: {
            assignToIndexExpr(assignStmt);
            return;
        }
        case DOT_EXPR: {
            assignToDotExpr(assignStmt);
            return;
        }
        default: {
            errorAt(assignStmt->line, "Don't know how to assign to this expr?");
        }
        }
    }
    case RETURN_STMT: {
        ReturnStmt *returnStmt = (ReturnStmt *)stmt;
        // Need to check type is correct;
        if (!llvmFunction->enclosing) {
            errorAt(returnStmt->line, "Can't return outside of a function");
        }

        llvm::Value *returnValue = loadAllocaInst(compileExpression(returnStmt->value));
        // ToDo  better check for llvmCompiler
        // Check here if it's an allocaInst and then load it before sending
        // it back
        llvm::Type *expectedReturnType = llvmFunction->functionType->getReturnType();
        builder->CreateRet(returnValue);
        break;
    }
    case VAR_STMT: {
        VarStmt *varStmt = (VarStmt *)stmt;
        Variable *var = varStmt->var;
        std::string varName = var->name;

        llvm::Value *value = compileExpression(varStmt->initializer);

        // if (!checkVariableValueMatch(var, value)) {
        //     printf("Invalid type mismatch in var declaration\nexpected: ");
        //     debugVariable(var);
        //     printf("\nbut got: ");
        //     debugValueType(value->getType(), llvmCompiler->ctx);
        //     printf("\n");
        //     exit(1);
        // }

        llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value);

        if (allocaInst != nullptr) {
            if (varStmt->initializer->type == VAR_EXPR) {
                llvm::AllocaInst *allocaVar = builder->CreateAlloca(allocaInst->getAllocatedType(), nullptr, varName);
                copyAllocation(allocaVar, allocaInst, var);
                allocaInst = allocaVar;
            }
            allocaInst->setName(varName);
        } else {
            allocaInst = builder->CreateAlloca(value->getType(), nullptr, varName);

            if (value->getType() == llvmCompiler->internalStructs["array"]) {
                copyArray(allocaInst, value, var);
            } else {
                builder->CreateStore(value, allocaInst);
            }
        }

        llvmFunction->scopedVariables.back().push_back(allocaInst);

        break;
    }
    case WHILE_STMT: {
        WhileStmt *whileStmt = (WhileStmt *)stmt;

        llvm::BasicBlock *headerBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "header", llvmFunction->function);
        llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "body", llvmFunction->function);
        llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "exit", llvmFunction->function);

        compileLoopHeader(headerBlock, exitBlock, bodyBlock, whileStmt->condition);
        compileLoopBody(headerBlock, exitBlock, whileStmt->body);
        compileLoopExit(headerBlock, exitBlock);

        break;
    }
    case FOR_STMT: {
        ForStmt *forStmt = (ForStmt *)stmt;

        llvm::BasicBlock *headerBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "header", llvmFunction->function);
        llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "body", llvmFunction->function);
        llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "exit", llvmFunction->function);

        compileStatement(forStmt->initializer);
        compileLoopHeader(headerBlock, exitBlock, bodyBlock, forStmt->condition);
        compileLoopBody(headerBlock, exitBlock, forStmt->body);
        compileLoopExit(headerBlock, exitBlock, forStmt->increment);

        break;
    }
    case STRUCT_STMT: {
        StructStmt *structStmt = (StructStmt *)stmt;
        std::string structName = structStmt->name;

        std::vector<llvm::Type *> fieldTypes = std::vector<llvm::Type *>(structStmt->fields.size());
        std::vector<std::string> fieldNames = std::vector<std::string>(structStmt->fields.size());

        for (int i = 0; i < fieldTypes.size(); ++i) {
            fieldTypes[i] = getTypeFromVariable(structStmt->fields[i]);
            fieldNames[i] = structStmt->fields[i]->name;
        }

        llvmCompiler->structs[structName] = new LLVMStruct(fieldTypes, structName, fieldNames, llvmCompiler->ctx);
        break;
    }
    case IF_STMT: {
        IfStmt *ifStmt = (IfStmt *)stmt;
        llvm::Value *condition = compileExpression(ifStmt->condition);

        llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "then", llvmFunction->function);
        llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "else", llvmFunction->function);
        llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*llvmCompiler->ctx, "merge", llvmFunction->function);

        // Enter then block
        builder->CreateCondBr(condition, thenBlock, elseBlock);
        builder->SetInsertPoint(thenBlock);
        llvmFunction->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());

        // Enter else block
        bool returned = compileIfBranch(ifStmt->thenBranch);
        llvmFunction->scopedVariables.pop_back();
        if (!returned && !llvmFunction->broke) {
            builder->CreateBr(mergeBlock);
        }
        builder->SetInsertPoint(elseBlock);
        llvmFunction->broke = false;
        llvmFunction->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());

        enterMergeBlock(compileIfBranch(ifStmt->elseBranch), mergeBlock);
        break;
    }
    case FUNC_STMT: {
        FuncStmt *funcStmt = (FuncStmt *)stmt;
        if (llvmFunction->enclosing) {
            errorAt(funcStmt->line, "Can't declare a function in a function");
        }
        llvm::IRBuilder<> *prevBuilder = enterFuncScope(funcStmt);

        bool returned = false;
        for (auto &stmt : funcStmt->body) {
            compileStatement(stmt);
            if (stmt->type == RETURN_STMT) {
                returned = true;
                break;
            }
        }

        if (!returned) {
            if (!llvmFunction->functionType->getReturnType()->isVoidTy()) {
                errorAt(funcStmt->line, ("Non-void function does not return a value in " + funcStmt->name).c_str());
            }
            builder->CreateRetVoid();
        }

        llvmCompiler->callableFunctions.push_back(llvmFunction->function);
        llvmFunction = llvmFunction->enclosing;
        builder = prevBuilder;
        break;
    }
    }
}

void compile(std::vector<Stmt *> stmts) {
    for (auto &stmt : stmts) {
        compileStatement(stmt);
    }
    endCompiler();
}
