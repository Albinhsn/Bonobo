#include "llvm.h"

LLVMCompiler *llvmCompiler;
llvm::IRBuilder<> *builder;
LLVMFunction *llvmFunction;

static void errorAt(int line, const char *message, ...) {
    va_list args;
    fprintf(stderr, "[%d] Error", line);
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
    default: {
        printf("not implemented?\n");
    }
    }
    printf("Can't lookup llvmCompiler var type ");
    debugVariable(var);
    printf("\n");
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
        }
        case NIL_VAR: {
        }
        }
    }
    printf("trying to lookup unknown var \n");
    exit(1);
}

static void addLibraryFuncs() {
    std::map<std::string, llvm::FunctionCallee> libraryFuncs = std::map<std::string, llvm::FunctionCallee>();

    std::vector<llvm::Type *> args = {builder->getPtrTy()};
    llvm::FunctionType *type = llvm::FunctionType::get(builder->getVoidTy(), args, true);
    llvm::FunctionCallee func = llvmCompiler->module->getOrInsertFunction("printf", type);
    libraryFuncs["printf"] = func;

    args = {builder->getPtrTy(), builder->getPtrTy()};
    type = llvm::FunctionType::get(builder->getPtrTy(), args, true);
    func = llvmCompiler->module->getOrInsertFunction("strcat", type);
    libraryFuncs["strcat"] = func;

    args = {builder->getInt32Ty()};
    type = llvm::FunctionType::get(builder->getPtrTy(), args, true);
    func = llvmCompiler->module->getOrInsertFunction("malloc", type);

    libraryFuncs["malloc"] = func;
    llvmCompiler->libraryFuncs = libraryFuncs;
}
static void addInternalStructs() {
    std::map<std::string, llvm::StructType *> strukts = {};

    std::vector<llvm::Type *> fieldTypes = {builder->getPtrTy(), builder->getInt32Ty()};
    strukts["array"] = llvm::StructType::create(fieldTypes, "array");

    fieldTypes = {strukts["array"], strukts["array"]};
    strukts["map"] = llvm::StructType::create(*llvmCompiler->ctx, fieldTypes, "map");

    llvmCompiler->internalStructs = strukts;
}

void initCompiler(std::vector<Variable *> variables) {
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
    addLibraryFuncs();
    addInternalStructs();
}

static void endCompiler() {
    builder->CreateRet(builder->getInt32(0));

    std::error_code errorCode;
    llvm::raw_fd_ostream outLL("./out.ll", errorCode);
    llvmCompiler->module->print(outLL, nullptr);
}

static bool nameIsAlreadyDeclared(std::string name) {
    // Check variables
    std::vector<llvm::AllocaInst *> lastScope = llvmFunction->scopedVariables.back();
    for (int i = 0; i < lastScope.size(); i++) {
        if (lastScope[i]->getName().str() == name) {
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
    std::vector<llvm::Function *> callableFunctions = llvmCompiler->callableFunctions;
    for (int i = 0; i < callableFunctions.size(); ++i) {
        if (callableFunctions[i]->getName().str() == name) {
            return true;
        }
    }

    return false;
}

// Returns true if you return inside of the branch
static bool compileIfBranch(std::vector<Stmt *> branch) {
    for (int i = 0; i < branch.size(); ++i) {
        if (branch[i]->type == BREAK_STMT) {
            llvmFunction->broke = true;
            builder->CreateBr(llvmFunction->exitBlock->exitBlock);
            return false;
        }
        compileStatement(branch[i]);
        if (branch[i]->type == RETURN_STMT) {
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

static void checkValidFuncDeclaration(FuncStmt *funcStmt) {
    if (nameIsAlreadyDeclared(funcStmt->name)) {
        errorAt(funcStmt->line,
                "Can't declare function '%s', name is already "
                "declared\n",
                funcStmt->name.c_str());
    }
    if (llvmFunction->enclosing) {
        errorAt(funcStmt->line, "Can't declare a function in a function\n");
    }
}

static llvm::MaybeAlign getAlignment(llvm::Type *type) { return llvm::MaybeAlign(type->getPrimitiveSizeInBits() / 8); }

static llvm::Function *lookupFunction(std::string name) {
    for (int i = 0; i < llvmCompiler->callableFunctions.size(); ++i) {
        if (llvmCompiler->callableFunctions[i]->getName() == name) {
            return llvmCompiler->callableFunctions[i];
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
    errorAt(line, ("Unknown variable" + name).c_str());
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
    for (int i = 0; i < body.size(); ++i) {
        if (body[i]->type == BREAK_STMT) {
            llvmFunction->broke = true;
            builder->CreateBr(exitBlock);
            break;
        }
        compileStatement(body[i]);
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

static void storeArraySize(llvm::Value *size, llvm::Value *arrayInstance) {
    storeStructField(llvmCompiler->internalStructs["array"], arrayInstance, size, 1);
}

static void storeArray(llvm::Value *array, llvm::Value *arrayInstance) {
    storeStructField(llvmCompiler->internalStructs["array"], arrayInstance, array, 0);
}

static llvm::Value *compileLiteral(LiteralExpr *expr) {
    std::string stringLiteral = expr->literal;
    switch (expr->literalType) {
    case STR_LITERAL: {

        llvm::AllocaInst *stringInstance =
            builder->CreateAlloca(llvmCompiler->internalStructs["array"], nullptr, "string");
        llvmCompiler->strings.push_back(stringInstance);

        storeArray(builder->CreateGlobalString(stringLiteral), stringInstance);
        storeArraySize(builder->getInt32(stringLiteral.size() + 1), stringInstance);

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

static llvm::Value *loadArray(llvm::Value *arrayPtr) {
    llvm::Value *ptr = builder->CreateStructGEP(llvmCompiler->internalStructs["array"], arrayPtr, 0);
    return builder->CreateLoad(builder->getPtrTy(), ptr);
}

static llvm::Value *loadArraySize(llvm::Value *arrayPtr) {
    llvm::Value *ptr = builder->CreateStructGEP(llvmCompiler->internalStructs["array"], arrayPtr, 1);
    return builder->CreateLoad(builder->getInt32Ty(), ptr);
}

static llvm::Value *loadAllocaInst(llvm::Value *value) {
    if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
        return builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
    }
    return value;
}

static void storePtrArrayItems(ArrayExpr *arrayExpr, llvm::AllocaInst *arrayInstance) {
    // ToDo document why llvmCompiler behavesllvmCompiler way
    for (int i = 0; i < arrayExpr->items.size(); ++i) {
        llvm::Value *arrValue = compileExpression(arrayExpr->items[i]);
        llvm::Value *arrayInstanceGEP = builder->CreateInBoundsGEP(
            llvmCompiler->internalStructs["array"], arrayInstance, {builder->getInt32(0), builder->getInt32(0)});
        llvm::Value *arrayPtr = builder->CreateLoad(builder->getPtrTy(), arrayInstanceGEP);
        llvm::Value *arrayInboundPtr = builder->CreateInBoundsGEP(
            builder->getPtrTy(), arrayPtr, builder->CreateSExt(builder->getInt32(i), builder->getInt64Ty()));
        builder->CreateStore(arrValue, arrayInboundPtr);
    }
}

static llvm::GlobalVariable *createGlobalArray(ArrayExpr *arrayExpr, llvm::ArrayType *arrayType) {
    std::vector<llvm::Constant *> arrayItems = std::vector<llvm::Constant *>(arrayExpr->items.size());
    for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
        arrayItems[i] = llvm::dyn_cast<llvm::Constant>(compileExpression(arrayExpr->items[i]));
    }

    return new llvm::GlobalVariable(*llvmCompiler->module, arrayType, false, llvm::GlobalValue::PrivateLinkage,
                                    llvm::ConstantArray::get(arrayType, arrayItems));
}

static bool isStringTy(llvm::Value *value) {
    for (int i = 0; i < llvmCompiler->strings.size(); ++i) {
        if (llvmCompiler->strings[i] == value) {
            return true;
        }
    }
    return false;
}

static llvm::Value *concatStrings(llvm::Value *left, llvm::Value *right) {
    llvm::StructType *stringStruct = llvmCompiler->internalStructs["array"];

    llvm::Value *leftSize = loadArraySize(left);
    llvm::Value *newSize = builder->CreateAdd(leftSize, loadArraySize(right));

    llvm::AllocaInst *concStringInstance = builder->CreateAlloca(stringStruct, nullptr, "string");
    llvm::Value *mallocResult = builder->CreateCall(llvmCompiler->libraryFuncs["malloc"], {newSize});

    storeArraySize(newSize, concStringInstance);
    storeArray(mallocResult, concStringInstance);

    builder->CreateMemCpy(mallocResult, llvm::MaybeAlign(1), loadArray(left), llvm::MaybeAlign(1), leftSize);
    builder->CreateCall(llvmCompiler->libraryFuncs["strcat"], {mallocResult, loadArray(right)});

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

static Variable *lookupVariableByName(std::string varName) {
    for (int i = 0; i < llvmCompiler->variables.size(); ++i) {
        if (llvmCompiler->variables[i]->name == varName) {
            return llvmCompiler->variables[i];
        }
    }
    return nullptr;
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

static llvm::Value *getArrayIndex(llvm::Type *type, llvm::Value *loadedArray, llvm::Value *index) {
    if (type == llvmCompiler->internalStructs["array"] || type->isStructTy()) {
        type = builder->getPtrTy();
    }
    return builder->CreateInBoundsGEP(type, loadedArray, index);
}

static llvm::Value *getPointerToArrayIndex(IndexExpr *indexExpr, Variable *&var) {
    llvm::Value *indexValue = nullptr;
    if (indexExpr->variable->type == INDEX_EXPR) {
        IndexExpr *expr = (IndexExpr *)indexExpr->variable;
        indexValue = loadIndex(expr, var);
    } else if (indexExpr->variable->type == VAR_EXPR) {
        indexValue = compileExpression(indexExpr->variable);
        VarExpr *varExpr = (VarExpr *)indexExpr->variable;
        var = lookupVariableByName(varExpr->name);
    }

    if (indexValue == nullptr) {
        printf("can't index non var?\n");
        debugExpression(indexExpr->variable);
        exit(1);
    }

    llvm::Value *index = compileExpression(indexExpr->index);
    if (llvm::AllocaInst *castedVar = llvm::dyn_cast<llvm::AllocaInst>(indexValue)) {
        var = lookupVariableByName(castedVar->getName().str());
        return getArrayIndex(lookupArrayItemType(var), loadArray(castedVar), index);
    } else if (indexValue->getType() == llvmCompiler->internalStructs["array"]) {
        ArrayVariable *arrayVar = (ArrayVariable *)var;
        var = arrayVar->items;
        llvm::Value *loadedArray = builder->CreateExtractValue(indexValue, 0);
        return getArrayIndex(lookupArrayItemType(var), loadedArray, index);
    }

    printf("couldn't cast index variable, was type: ");
    debugValueType(indexValue->getType(), llvmCompiler->ctx);
    printf("\n");
    exit(1);
}

llvm::Value *loadIndex(IndexExpr *indexExpr, Variable *&var) {
    llvm::Value *idxPtr = getPointerToArrayIndex(indexExpr, var);
    if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(idxPtr)) {
        printf("was allocated\n");
    }
    llvm::Type *arrayItemType = lookupArrayItemType(var);
    if (var->type == ARRAY_VAR) {
        ArrayVariable *arrayVar = (ArrayVariable *)var;
        if (arrayVar->items->type == ARRAY_VAR || arrayVar->items->type == STR_VAR) {
            llvm::Value *loadedPtr = builder->CreateLoad(builder->getPtrTy(), idxPtr);
            llvm::Value *loadedArrayPtr =
                builder->CreateInBoundsGEP(arrayItemType, loadedPtr, {builder->getInt32(0), builder->getInt32(0)});
            return builder->CreateLoad(arrayItemType, loadedArrayPtr);
        }
        if (arrayVar->items->type == STRUCT_VAR) {
            llvm::Value *loadedStructPtr = builder->CreateLoad(builder->getPtrTy(), idxPtr);
            return builder->CreateLoad(arrayItemType, loadedStructPtr);
        }
    }
    return builder->CreateLoad(arrayItemType, idxPtr);
}

static llvm::Type *getTypeFromNestedIndexExpr(Expr *expr) {
    if (expr->type == VAR_EXPR) {
        VarExpr *varExpr = (VarExpr *)expr;
        ArrayVariable *var = (ArrayVariable *)lookupVariableByName(varExpr->name);
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

static llvm::Value *loadIndexedArray(IndexExpr *indexExpr) {
    if (indexExpr->variable->type == VAR_EXPR) {
        llvm::Value *loadedTarget = loadArray(compileExpression(indexExpr->variable));
        VarExpr *varExpr = (VarExpr *)indexExpr->variable;
        return builder->CreateInBoundsGEP(lookupArrayItemType(lookupVariableByName(varExpr->name)), loadedTarget,
                                          compileExpression(indexExpr->index));
    } else if (indexExpr->variable->type != INDEX_EXPR) {
        printf("can't index non var?\n");
        debugExpression(indexExpr->variable);
        exit(1);
    }

    IndexExpr *expr = (IndexExpr *)indexExpr->variable;
    llvm::Value *loadedTarget = loadArray(builder->CreateLoad(builder->getPtrTy(), loadIndexedArray(expr)));
    return builder->CreateInBoundsGEP(getTypeFromNestedIndexExpr(expr->variable), loadedTarget,
                                      compileExpression(indexExpr->index));
}

static void assignToIndexExpr(AssignStmt *assignStmt) {
    IndexExpr *indexExpr = (IndexExpr *)assignStmt->variable;
    Variable *var = new Variable();
    builder->CreateStore(compileExpression(assignStmt->value), getPointerToArrayIndex(indexExpr, var));
}

static void assignToVarExpr(AssignStmt *assignStmt) {
    llvm::Value *value = compileExpression(assignStmt->value);
    VarExpr *varExpr = (VarExpr *)assignStmt->variable;
    llvm::Value *variable = lookupValue(varExpr->name, varExpr->line);
    // Check type is correct?
    builder->CreateStore(value, variable);
}

static llvm::Value *lookupStruct(DotExpr *dotExpr) {
    llvm::Value *value = compileExpression(dotExpr->name);
    if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
        return builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
    }
    if (!value->getType()->isStructTy()) {
        printf("variable isn't struct\n");
        exit(1);
    }

    return value;
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
        Variable *var = lookupVariableByName(varExpr->name);
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
    llvm::Value *struktPtr = nullptr;

    if (dotExpr->name->type == INDEX_EXPR) {
        Variable *var = lookupVariableByName(structName);
        struktPtr = builder->CreateLoad(builder->getPtrTy(), getPointerToArrayIndex((IndexExpr *)dotExpr->name, var));
    } else {
        struktPtr = compileExpression(dotExpr->name);
    }
    llvm::Value *value = compileExpression(assignStmt->value);

    if (LLVMStruct *strukt = llvmCompiler->structs[structName]) {
        for (int j = 0; j < strukt->fields.size(); j++) {
            if (strukt->fields[j] == dotExpr->field) {
                llvm::Value *ptr = builder->CreateStructGEP(strukt->structType, struktPtr, j);
                builder->CreateStore(value, ptr);
                return;
            }
        }
        printf("unknown struct field '%s' for '%s'\n", dotExpr->field.c_str(), structName.c_str());
        exit(1);
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
        errorAt(logicalExpr->line, "Can't do logical expr with these types\n");
    }
    case LITERAL_EXPR: {
        return compileLiteral((LiteralExpr *)expr);
    }
    case DOT_EXPR: {
        // ToDo clean llvmCompiler mess up
        DotExpr *dotExpr = (DotExpr *)expr;

        // Check llvmCompiler?
        llvm::Value *value = lookupStruct(dotExpr);
        if (LLVMStruct *strukt = llvmCompiler->structs[value->getType()->getStructName().str()]) {
            for (int j = 0; j < strukt->fields.size(); j++) {
                if (strukt->fields[j] == dotExpr->field) {
                    return builder->CreateExtractValue(value, j);
                }
            }
            printf("unknown struct field '%s' for '%s'\n", dotExpr->field.c_str(),
                   value->getType()->getStructName().str().c_str());
            exit(1);
        }
        errorAt(dotExpr->line, "Can't do property lookup on non struct\n");
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
        errorAt(comparisonExpr->line, "Can't do addition with llvmCompiler?\n");
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        llvm::Value *value = loadAllocaInst(compileExpression(unaryExpr->right));
        if (unaryExpr->op == NEG_UNARY) {
            if (value->getType()->isIntegerTy() || value->getType()->isDoubleTy()) {
                return builder->CreateMul(value, builder->getInt32(-1));
            }
            errorAt(unaryExpr->line, "Can't do '-' with llvmCompiler type\n");
        } else if (unaryExpr->op == BANG_UNARY) {
            // Check value type?
            return builder->CreateXor(value, 1);
        } else {
            errorAt(unaryExpr->line, "unknown unary expr?\n");
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
        printf("can't increment non allocation?\n");
        exit(1);
    }
    case ARRAY_EXPR: {
        ArrayExpr *arrayExpr = (ArrayExpr *)expr;

        // Figure ut which type array has
        llvm::Type *elementType = getTypeFromVariable(arrayExpr->itemType);

        llvm::AllocaInst *arrayInstance =
            builder->CreateAlloca(llvmCompiler->internalStructs["array"], nullptr, "array");
        // Is ptr to objects
        if (elementType == llvmCompiler->internalStructs["array"] || elementType->isStructTy()) {
            llvm::Value *arrGep = builder->CreateCall(llvmCompiler->libraryFuncs["malloc"],
                                                      {builder->getInt32(arrayExpr->items.size() * 8)});
            storeArray(arrGep, arrayInstance);
            storePtrArrayItems(arrayExpr, arrayInstance);
        } else {
            llvm::ArrayType *arrayType = llvm::ArrayType::get(elementType, arrayExpr->items.size());
            llvm::GlobalVariable *globalArray = createGlobalArray(arrayExpr, arrayType);
            llvm::Value *arrGep =
                builder->CreateGEP(arrayType, globalArray, {builder->getInt32(0), builder->getInt32(0)});
            storeArray(arrGep, arrayInstance);
        }
        storeArraySize(builder->getInt32(arrayExpr->items.size()), arrayInstance);

        return arrayInstance;
    }
    case MAP_EXPR: {
        MapExpr *mapExpr = (MapExpr *)expr;
        std::vector<llvm::Value *> keys = std::vector<llvm::Value *>(mapExpr->keys.size());
        std::vector<llvm::Value *> values = std::vector<llvm::Value *>(mapExpr->values.size());
        for (int i = 0; i < keys.size(); ++i) {
            keys[i] = compileExpression(mapExpr->keys[i]);
            values[i] = compileExpression(mapExpr->values[i]);
        }
        // llvm::Type *elementType = ;
    }
    case CALL_EXPR: {
        CallExpr *callExpr = (CallExpr *)expr;
        std::string name = callExpr->callee;

        if (llvmCompiler->structs.count(name)) {
            return createStruct(callExpr);
        }

        std::vector<llvm::Value *> params = std::vector<llvm::Value *>(callExpr->arguments.size());
        for (int i = 0; i < callExpr->arguments.size(); ++i) {
            params[i] = compileExpression(callExpr->arguments[i]);
        }

        llvm::Function *func = lookupFunction(name);
        if (llvmCompiler->libraryFuncs.count(name)) {
            for (int i = 0; i < callExpr->arguments.size(); ++i) {
                if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(params[i])) {
                    if (allocaInst->getAllocatedType()->isStructTy()) {
                        params[i] = loadArray(allocaInst);
                    } else {
                        params[i] = builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                    }
                }
            }
            return builder->CreateCall(llvmCompiler->libraryFuncs[name], params);
        }

        checkCallParamCorrectness(func, params, name);
        return builder->CreateCall(func, params);
    }
    default: {
        printf("unknown expr\n");
        exit(1);
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
        llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(lookupValue(compStmt->name, compStmt->line));
        llvm::Value *variable = builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
        builder->CreateStore(binaryOp(compileExpression(compStmt->right), variable, compStmt->op, compStmt->line),
                             allocaInst);
        break;
    }
    case BREAK_STMT: {
        printf("how did you get here?\n");
        exit(1);
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
            printf("how did llvmCompiler happen to me\n");
            exit(1);
        }
        }
    }
    case RETURN_STMT: {
        ReturnStmt *returnStmt = (ReturnStmt *)stmt;
        // Need to check type is correct;
        if (!llvmFunction->enclosing) {
            printf("Can't return outside of a function\n");
            exit(1);
        }

        llvm::Value *returnValue = loadAllocaInst(compileExpression(returnStmt->value));
        // ToDo  better check for llvmCompiler
        // Check here if it's an allocaInst and then load it before sending
        // it back

        if (llvmFunction->functionType->getReturnType() != returnValue->getType()) {
            printf("funcType -> ");
            debugValueType(llvmFunction->functionType->getReturnType(), llvmCompiler->ctx);
            printf("\n");
            printf("return -> ");
            debugValueType(returnValue->getType(), llvmCompiler->ctx);
            printf("\n");
            printf("Mismatching return in '%s'\n", llvmFunction->function->getName().str().c_str());
            exit(1);
        }
        builder->CreateRet(returnValue);
        break;
    }
    case VAR_STMT: {
        VarStmt *varStmt = (VarStmt *)stmt;
        std::string varName = varStmt->var->name;
        if (nameIsAlreadyDeclared(varName)) {
            std::string errorMessage = "Can't declare variable '" + varName + "', name is already declared";
            errorAt(varStmt->line, errorMessage.c_str());
        }
        llvm::Value *value = compileExpression(varStmt->initializer);
        // if (!checkVariableValueMatch(varStmt->var, value)) {
        //     printf("Invalid type mismatch in var declaration\nexpected: ");
        //     debugVariable(varStmt->var);
        //     printf("\nbut got: ");
        //     debugValueType(value->getType(), llvmCompiler->ctx);
        //     printf("\n");
        //     exit(1);
        // }

        if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            if (varStmt->initializer->type == VAR_EXPR) {
                // If var is array, retrieve field types, copy the size and array
                llvm::AllocaInst *allocaVar = builder->CreateAlloca(allocaInst->getAllocatedType(), nullptr, varName);
                builder->CreateMemCpy(allocaVar, llvm::MaybeAlign(8), allocaInst, llvm::MaybeAlign(8), 16);
                llvmFunction->scopedVariables.back().push_back(allocaVar);
            } else {
                allocaInst->setName(varName);
                llvmFunction->scopedVariables.back().push_back(allocaInst);
            }
            break;
        }
        llvm::AllocaInst *allocaInst = builder->CreateAlloca(value->getType(), nullptr, varName);
        builder->CreateStore(value, allocaInst);
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
        if (nameIsAlreadyDeclared(structStmt->name)) {

            errorAt(structStmt->line,
                    ("Can't declare struct '" + structName + "', name is already declared\n").c_str());
        }

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
        checkValidFuncDeclaration(funcStmt);
        llvm::IRBuilder<> *prevBuilder = enterFuncScope(funcStmt);

        bool returned = false;
        for (int i = 0; i < funcStmt->body.size(); ++i) {
            compileStatement(funcStmt->body[i]);
            if (funcStmt->body[i]->type == RETURN_STMT) {
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
    for (int i = 0; i < stmts.size(); i++) {
        compileStatement(stmts[i]);
    }
    endCompiler();
}
