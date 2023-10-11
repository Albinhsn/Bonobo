#include "common.h"
#include "debug.h"
#include "expr.h"
#include "stmt.h"
#include "variables.h"
#include <algorithm>
#include <llvm/Support/Casting.h>
#include <memory>

class ExitBlock {
  public:
    ExitBlock *prev;
    llvm::BasicBlock *exitBlock;
    ExitBlock(ExitBlock *prev, llvm::BasicBlock *exitBlock) {
        this->prev = prev;
        this->exitBlock = exitBlock;
    }
};

class LLVMStruct {
  private:
  public:
    llvm::StructType *structType;
    std::vector<std::string> fields;
    LLVMStruct(std::vector<llvm::Type *> fieldTypes, std::string structName, std::vector<std::string> fields,
               llvm::LLVMContext *ctx) {
        this->fields = fields;
        this->structType = llvm::StructType::create(*ctx, fieldTypes, structName);
    }
};

class LLVMFunction {
  private:
  public:
    bool broke;
    LLVMFunction *enclosing;
    std::vector<std::vector<llvm::AllocaInst *>> scopedVariables;
    std::map<std::string, int> functionArguments;
    std::map<llvm::Value *, llvm::Type *> arrayElements;
    llvm::BasicBlock *entryBlock;
    ExitBlock *exitBlock;
    llvm::Function *function;
    llvm::FunctionType *functionType;

    LLVMFunction(LLVMFunction *enclosing, llvm::FunctionType *funcType, std::string name,
                 std::map<std::string, int> funcArgs, llvm::LLVMContext *ctx, llvm::Module *module) {
        this->broke = false;
        this->exitBlock = nullptr;
        this->enclosing = enclosing;
        this->functionType = funcType;
        this->function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name, *module);
        this->scopedVariables = std::vector<std::vector<llvm::AllocaInst *>>(1);
        this->functionArguments = funcArgs;
        this->entryBlock = llvm::BasicBlock::Create(*ctx, "entry", this->function);
    }
};

class LLVMCompiler {
  private:
    llvm::IRBuilder<> *builder;
    std::vector<llvm::Function *> callableFunctions;
    llvm::LLVMContext *ctx;
    LLVMFunction *llvmFunction;
    std::map<std::string, llvm::StructType *> internalStructs;
    std::map<std::string, llvm::FunctionCallee> libraryFuncs;
    llvm::Module *module;
    std::vector<Variable *> variables;
    std::vector<Stmt *> stmts;
    std::vector<llvm::AllocaInst *> strings;
    std::map<std::string, LLVMStruct *> structs;

    void endCompiler() {
        this->builder->CreateRet(this->builder->getInt32(0));

        std::error_code errorCode;
        llvm::raw_fd_ostream outLL("./out.ll", errorCode);
        this->module->print(outLL, nullptr);
    }

    bool nameIsAlreadyDeclared(std::string name) {
        // Check variables
        for (int i = 0; i < this->llvmFunction->scopedVariables.back().size(); i++) {
            if (this->llvmFunction->scopedVariables.back()[i]->getName().str() == name) {
                return true;
            }
        }
        // Check structs
        if (this->structs.count(name)) {
            return true;
        }
        // Check library functions
        for (const auto &[key, value] : this->libraryFuncs) {
            if (key == name) {
                return true;
            }
        }
        // Check user declared functions
        for (int i = 0; i < this->callableFunctions.size(); ++i) {
            if (this->callableFunctions[i]->getName().str() == name) {
                return true;
            }
        }

        return false;
    }

    bool compileBranch(std::vector<Stmt *> branch) {
        for (int i = 0; i < branch.size(); ++i) {
            if (branch[i]->type == BREAK_STMT) {
                this->llvmFunction->broke = true;
                this->builder->CreateBr(this->llvmFunction->exitBlock->exitBlock);
                return false;
            }
            compileStatement(branch[i]);
            if (branch[i]->type == RETURN_STMT) {
                return true;
            }
        }
        return false;
    }
    void enterThenBlock(llvm::Value *condition, llvm::BasicBlock *thenBlock, llvm::BasicBlock *elseBlock) {
        this->builder->CreateCondBr(condition, thenBlock, elseBlock);
        this->builder->SetInsertPoint(thenBlock);
        this->llvmFunction->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());
    }

    void enterMergeBlock(bool returned, llvm::BasicBlock *mergeBlock) {
        this->llvmFunction->scopedVariables.pop_back();
        if (!returned && !this->llvmFunction->broke) {
            this->builder->CreateBr(mergeBlock);
        }
        this->builder->SetInsertPoint(mergeBlock);
    }

    void enterElseBlock(bool returned, llvm::BasicBlock *elseBlock, llvm::BasicBlock *mergeBlock) {
        this->llvmFunction->scopedVariables.pop_back();
        if (!returned && !this->llvmFunction->broke) {
            this->builder->CreateBr(mergeBlock);
        }
        this->builder->SetInsertPoint(elseBlock);
        this->llvmFunction->broke = false;
        this->llvmFunction->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());
    }

    llvm::IRBuilder<> *enterFuncScope(FuncStmt *funcStmt) {
        // Fix params
        std::vector<llvm::Type *> params = std::vector<llvm::Type *>(funcStmt->params.size());
        std::map<std::string, int> funcArgs;
        for (int i = 0; i < funcStmt->params.size(); ++i) {
            params[i] = getLLVMTypeFromVariable(funcStmt->params[i]);
            funcArgs[funcStmt->params[i]->name] = i;
        }

        // Ret type
        llvm::Type *returnType = getLLVMTypeFromVariable(funcStmt->returnType);

        // Fix func type
        llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, params, false);
        this->llvmFunction =
            new LLVMFunction(this->llvmFunction, funcType, funcStmt->name, funcArgs, this->ctx, this->module);
        llvm::IRBuilder<> *prevBuilder = this->builder;
        this->builder = new llvm::IRBuilder<>(this->llvmFunction->entryBlock);
        return prevBuilder;
    }

    void checkValidFuncDeclaration(FuncStmt *funcStmt) {
        if (nameIsAlreadyDeclared(funcStmt->name)) {
            printf("Can't declare function '%s', name is already "
                   "declared\n",
                   funcStmt->name.c_str());
            exit(1);
        }
        if (this->llvmFunction->enclosing) {
            printf("Can't declare a function in a function\n");
            exit(1);
        }
    }

    void exitFuncScope(llvm::IRBuilder<> *prevBuilder) {
        this->callableFunctions.push_back(this->llvmFunction->function);
        this->llvmFunction = this->llvmFunction->enclosing;
        this->builder = prevBuilder;
    }

    llvm::Type *getLLVMTypeFromVariable(Variable *itemType) {
        if (itemType != nullptr) {
            switch (itemType->type) {
            case INT_VAR: {
                return this->builder->getInt32Ty();
            }
            case DOUBLE_VAR: {
                return this->builder->getDoubleTy();
            }
            case BOOL_VAR: {
                return this->builder->getInt1Ty();
            }
            case ARRAY_VAR: {
                return this->internalStructs["array"];
            }
            case STR_VAR: {
                return this->internalStructs["array"];
            }
            case STRUCT_VAR: {
                StructVariable *structVar = (StructVariable *)itemType;
                if (this->structs.count(structVar->structName.lexeme)) {
                    return this->structs[structVar->structName.lexeme]->structType;
                } else {
                    printf("trying to lookup unknown struct '%s'\n", structVar->structName.lexeme.c_str());
                    exit(1);
                }
            }
            default: {
            }
            }
        }
        printf("trying to lookup unknown var \n");
        exit(1);
    }

    llvm::MaybeAlign getAlignment(llvm::Type *type) { return llvm::MaybeAlign(type->getPrimitiveSizeInBits() / 8); }

    llvm::Function *lookupFunction(std::string name) {
        for (int i = 0; i < this->callableFunctions.size(); ++i) {
            if (this->callableFunctions[i]->getName() == name) {
                return this->callableFunctions[i];
            }
        }
        return this->llvmFunction->function->getName() == name ? this->llvmFunction->function : nullptr;
    }

    llvm::Value *lookupValue(std::string name) {
        if (this->llvmFunction->enclosing && this->llvmFunction->functionArguments.count(name)) {
            int i = 0;
            for (llvm::Function::arg_iterator arg = this->llvmFunction->function->arg_begin();
                 arg != this->llvmFunction->function->arg_end(); ++arg) {
                if (i == this->llvmFunction->functionArguments[name]) {
                    return arg;
                }
                ++i;
            }
        }
        for (int i = this->llvmFunction->scopedVariables.size() - 1; i >= 0; i--) {
            std::vector<llvm::AllocaInst *> scopeVars = this->llvmFunction->scopedVariables[i];

            for (int j = 0; j < scopeVars.size(); ++j) {
                if (scopeVars[j]->getName().str() == name) {
                    return scopeVars[j];
                }
            }
        }
        printf("Unknown variable %s\n", name.c_str());
        exit(1);
    }

    void compileLoopExit(llvm::BasicBlock *headerBlock, llvm::BasicBlock *exitBlock, Stmt *stmt = nullptr) {
        if (!this->llvmFunction->broke) {
            if (stmt != nullptr) {
                compileStatement(stmt);
            }
            this->llvmFunction->broke = false;
            this->builder->CreateBr(headerBlock);
        }

        this->llvmFunction->exitBlock = this->llvmFunction->exitBlock->prev;
        this->builder->SetInsertPoint(exitBlock);
    }

    void compileLoopBody(llvm::BasicBlock *headerBlock, llvm::BasicBlock *exitBlock, std::vector<Stmt *> body) {
        for (int i = 0; i < body.size(); ++i) {
            if (body[i]->type == BREAK_STMT) {
                this->llvmFunction->broke = true;
                this->builder->CreateBr(exitBlock);
                break;
            }
            compileStatement(body[i]);
        }
    }

    void compileLoopHeader(llvm::BasicBlock *headerBlock, llvm::BasicBlock *exitBlock, llvm::BasicBlock *bodyBlock,
                           Expr *condition) {
        this->builder->CreateBr(headerBlock);
        this->builder->SetInsertPoint(headerBlock);

        this->llvmFunction->exitBlock = new ExitBlock(this->llvmFunction->exitBlock, exitBlock);
        this->builder->CreateCondBr(compileExpression(condition), bodyBlock, exitBlock);
        this->builder->SetInsertPoint(bodyBlock);
    }

    void checkCallParamCorrectness(llvm::Function *func, std::vector<llvm::Value *> params, std::string funcName) {
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

    bool checkVariableValueMatch(Variable *var, llvm::Value *&value) {
        llvm::Type *type = value->getType();
        if (type == this->builder->getInt32Ty()) {
            if (var->type == DOUBLE_VAR) {
                value = this->builder->CreateUIToFP(value, this->builder->getDoubleTy());
                return true;
            }
            return var->type == INT_VAR;
        } else if (type == this->builder->getInt1Ty()) {
            return var->type == BOOL_VAR;
        } else if (type == this->builder->getDoubleTy()) {
            return var->type == DOUBLE_VAR;
        } else if (type->isStructTy()) {
            // ToDo this needs to check underlying type as well
            return var->type == STR_VAR || var->type == ARRAY_VAR || var->type == MAP_VAR || var->type == STRUCT_VAR;
        }
        return false;
    }

    llvm::Value *compileLiteral(LiteralExpr *expr) {
        std::string stringLiteral = expr->literal;
        switch (expr->literalType) {
        case STR_LITERAL: {

            llvm::AllocaInst *stringInstance =
                this->builder->CreateAlloca(this->internalStructs["array"], nullptr, "string");
            this->strings.push_back(stringInstance);

            storeArray(this->builder->CreateGlobalString(stringLiteral), stringInstance);
            storeArraySize(this->builder->getInt32(stringLiteral.size() + 1), stringInstance);

            return stringInstance;
        }
        case INT_LITERAL: {
            return this->builder->getInt32(stoi(stringLiteral));
        }
        case BOOL_LITERAL: {
            return stringLiteral == "true" ? this->builder->getInt1(1) : this->builder->getInt1(0);
        }
        case DOUBLE_LITERAL: {
            return llvm::ConstantFP::get(this->builder->getDoubleTy(), stod(stringLiteral));
        }
        }
    }

    llvm::Value *loadArray(llvm::Value *arrayPtr) {
        llvm::Value *ptr = this->builder->CreateStructGEP(this->internalStructs["array"], arrayPtr, 0);
        return this->builder->CreateLoad(this->builder->getPtrTy(), ptr);
    }

    llvm::Value *loadArraySize(llvm::Value *arrayPtr) {
        llvm::Value *ptr = this->builder->CreateStructGEP(this->internalStructs["array"], arrayPtr, 1);
        return this->builder->CreateLoad(this->builder->getInt32Ty(), ptr);
    }

    llvm::Value *loadAllocaInst(llvm::Value *value) {
        if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            return this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
        }
        return value;
    }

    void storeStructField(llvm::StructType *structType, llvm::Value *structInstance, llvm::Value *toStore, uint field) {
        llvm::Value *gep = this->builder->CreateStructGEP(structType, structInstance, field);
        this->builder->CreateStore(toStore, gep);
    }

    void storeArraySize(llvm::Value *size, llvm::Value *arrayInstance) {
        storeStructField(this->internalStructs["array"], arrayInstance, size, 1);
    }

    void storeArray(llvm::Value *array, llvm::Value *arrayInstance) {
        storeStructField(this->internalStructs["array"], arrayInstance, array, 0);
    }

    void storePtrArrayItems(ArrayExpr *arrayExpr, llvm::AllocaInst *arrayInstance) {
        // ToDo document why this behaves this way
        for (int i = 0; i < arrayExpr->items.size(); ++i) {
            llvm::Value *arrValue = compileExpression(arrayExpr->items[i]);
            llvm::Value *arrayInstanceGEP =
                this->builder->CreateInBoundsGEP(this->internalStructs["array"], arrayInstance,
                                                 {this->builder->getInt32(0), this->builder->getInt32(0)});
            llvm::Value *arrayPtr = this->builder->CreateLoad(this->builder->getPtrTy(), arrayInstanceGEP);
            llvm::Value *arrayInboundPtr = this->builder->CreateInBoundsGEP(
                this->builder->getPtrTy(), arrayPtr,
                this->builder->CreateSExt(this->builder->getInt32(i), this->builder->getInt64Ty()));
            this->builder->CreateStore(arrValue, arrayInboundPtr);
        }
    }

    llvm::GlobalVariable *createGlobalArray(ArrayExpr *arrayExpr, llvm::ArrayType *arrayType) {
        std::vector<llvm::Constant *> arrayItems = std::vector<llvm::Constant *>(arrayExpr->items.size());
        for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
            arrayItems[i] = llvm::dyn_cast<llvm::Constant>(compileExpression(arrayExpr->items[i]));
        }

        return createGlobalVariable(arrayType, llvm::ConstantArray::get(arrayType, arrayItems));
    }

    llvm::GlobalVariable *createGlobalVariable(llvm::Type *type, llvm::Constant *value) {
        return new llvm::GlobalVariable(*this->module, type, false, llvm::GlobalValue::PrivateLinkage, value);
    }

    bool isStringTy(llvm::Value *value) {
        return std::find(this->strings.begin(), this->strings.end(), value) != this->strings.end();
    }

    llvm::Value *concatStrings(llvm::Value *left, llvm::Value *right) {
        llvm::StructType *stringStruct = this->internalStructs["array"];

        llvm::Value *leftSize = loadArraySize(left);
        llvm::Value *newSize = this->builder->CreateAdd(leftSize, loadArraySize(right));

        llvm::AllocaInst *concStringInstance = this->builder->CreateAlloca(stringStruct, nullptr, "string");
        llvm::Value *mallocResult = this->builder->CreateCall(this->libraryFuncs["malloc"], {newSize});

        storeArraySize(newSize, concStringInstance);
        storeArray(mallocResult, concStringInstance);

        this->builder->CreateMemCpy(mallocResult, llvm::MaybeAlign(1), loadArray(left), llvm::MaybeAlign(1), leftSize);
        this->builder->CreateCall(this->libraryFuncs["strcat"], {mallocResult, loadArray(right)});

        return concStringInstance;
    }

    llvm::Value *createStruct(CallExpr *callExpr) {
        std::string name = callExpr->callee;
        LLVMStruct *strukt = this->structs[name];
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

    Variable *lookupVariable(std::string varName) {
        for (int i = 0; i < this->variables.size(); ++i) {
            if (this->variables[i]->name == varName) {
                return this->variables[i];
            }
        }
        return nullptr;
    }

    llvm::Type *lookupArrayItemType(Variable *var) {
        switch (var->type) {
        case INT_VAR: {
            return this->builder->getInt32Ty();
        }
        case BOOL_VAR: {
            return this->builder->getInt1Ty();
        }
        case STR_VAR: {
            return this->builder->getInt8Ty();
        }
        case DOUBLE_VAR: {
            return this->builder->getDoubleTy();
        }
        case ARRAY_VAR: {
            ArrayVariable *arrayVariable = (ArrayVariable *)var;
            if (arrayVariable->items->type == ARRAY_VAR || arrayVariable->items->type == STR_VAR) {
                return this->internalStructs["array"];
            }
            return lookupArrayItemType(arrayVariable->items);
        }
        case STRUCT_VAR: {
            StructVariable *structVar = (StructVariable *)var;
            if (this->structs.count(structVar->structName.lexeme)) {
                return this->structs[structVar->structName.lexeme]->structType;
            }
            break;
        }
        default: {
            printf("not implemented?\n");
        }
        }
        printf("Can't lookup this var type ");
        debugVariable(var);
        printf("\n");
        exit(1);
    }

    void castIntDouble(llvm::Value *&left, llvm::Value *&right) {
        if (left->getType()->isIntegerTy()) {
            left = this->builder->CreateUIToFP(left, this->builder->getDoubleTy());
        } else if (right->getType()->isIntegerTy()) {
            right = this->builder->CreateUIToFP(right, this->builder->getDoubleTy());
        }
    }

    llvm::Value *binaryOp(llvm::Value *left, llvm::Value *right, BinaryOp op) {
        left = loadAllocaInst(left);
        right = loadAllocaInst(right);

        // Check that both are literal
        //    Create new LLVMValue after op
        if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) {
            switch (op) {
            case ADD: {
                return this->builder->CreateAdd(left, right);
            }
            case SUB: {
                return this->builder->CreateSub(left, right);
            }
            case MUL: {
                return this->builder->CreateMul(left, right);
            }
            case DIV: {
                return this->builder->CreateUDiv(left, right);
            }
            }
        }

        castIntDouble(left, right);
        if (left->getType()->isDoubleTy() && right->getType()->isDoubleTy()) {
            switch (op) {
            case ADD: {
                return this->builder->CreateFAdd(left, right);
            }
            case SUB: {
                return this->builder->CreateFSub(left, right);
            }
            case MUL: {
                return this->builder->CreateFMul(left, right);
            }
            case DIV: {
                return this->builder->CreateFDiv(left, right);
            }
            }
        }
        printf("Can't do this binary op\n");
        exit(1);
    }

    llvm::Value *getPointerToArrayIndex(IndexExpr *indexExpr, Variable *&var) {
        llvm::Value *indexValue = nullptr;
        if (indexExpr->variable->type == INDEX_EXPR) {
            IndexExpr *expr = (IndexExpr *)indexExpr->variable;
            indexValue = loadIndex(expr, var);
        } else if (indexExpr->variable->type == VAR_EXPR) {
            indexValue = compileExpression(indexExpr->variable);
            VarExpr *varExpr = (VarExpr *)indexExpr->variable;
            var = lookupVariable(varExpr->name);
        }

        if (indexValue == nullptr) {
            printf("can't index non var?\n");
            debugExpression(indexExpr->variable);
            exit(1);
        }

        llvm::Value *index = compileExpression(indexExpr->index);
        if (llvm::AllocaInst *castedVar = llvm::dyn_cast<llvm::AllocaInst>(indexValue)) {
            var = lookupVariable(castedVar->getName().str());
            return getArrayIndex(lookupArrayItemType(var), loadArray(castedVar), index);
        } else if (indexValue->getType() == this->internalStructs["array"]) {
            ArrayVariable *arrayVar = (ArrayVariable *)var;
            var = arrayVar->items;
            llvm::Value *loadedArray = this->builder->CreateExtractValue(indexValue, 0);
            return getArrayIndex(lookupArrayItemType(var), loadedArray, index);
        }

        printf("couldn't cast index variable, was type: ");
        debugValueType(indexValue->getType(), this->ctx);
        printf("\n");
        exit(1);
    }

    llvm::Value *getArrayIndex(llvm::Type *type, llvm::Value *loadedArray, llvm::Value *index) {
        if (type == this->internalStructs["array"] || type->isStructTy()) {
            type = this->builder->getPtrTy();
        }
        return this->builder->CreateInBoundsGEP(type, loadedArray, index);
    }

    llvm::Value *loadIndex(IndexExpr *indexExpr, Variable *&var) {
        llvm::Value *idxPtr = getPointerToArrayIndex(indexExpr, var);
        llvm::Type *arrayItemType = lookupArrayItemType(var);
        if (var->type == ARRAY_VAR) {
            ArrayVariable *arrayVar = (ArrayVariable *)var;
            if (arrayVar->items->type == ARRAY_VAR || arrayVar->items->type == STR_VAR) {
                llvm::Value *loadedPtr = this->builder->CreateLoad(this->builder->getPtrTy(), idxPtr);
                llvm::Value *loadedArrayPtr = this->builder->CreateInBoundsGEP(
                    arrayItemType, loadedPtr, {this->builder->getInt32(0), this->builder->getInt32(0)});
                return this->builder->CreateLoad(arrayItemType, loadedArrayPtr);
            }
            if (arrayVar->items->type == STRUCT_VAR) {
                llvm::Value *loadedStructPtr = this->builder->CreateLoad(this->builder->getPtrTy(), idxPtr);
                return this->builder->CreateLoad(arrayItemType, loadedStructPtr);
            }
        }
        return this->builder->CreateLoad(arrayItemType, idxPtr);
    }

    llvm::Type *getTypeFromNestedIndexExpr(Expr *expr) {
        if (expr->type == VAR_EXPR) {
            VarExpr *varExpr = (VarExpr *)expr;
            ArrayVariable *var = (ArrayVariable *)lookupVariable(varExpr->name);
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

    llvm::Value *loadIndexedArray(IndexExpr *indexExpr) {
        if (indexExpr->variable->type == VAR_EXPR) {
            llvm::Value *loadedTarget = loadArray(compileExpression(indexExpr->variable));
            VarExpr *varExpr = (VarExpr *)indexExpr->variable;
            return this->builder->CreateInBoundsGEP(lookupArrayItemType(lookupVariable(varExpr->name)), loadedTarget,
                                                    compileExpression(indexExpr->index));
        } else if (indexExpr->variable->type != INDEX_EXPR) {
            printf("can't index non var?\n");
            debugExpression(indexExpr->variable);
            exit(1);
        }

        IndexExpr *expr = (IndexExpr *)indexExpr->variable;
        llvm::Value *loadedTarget =
            loadArray(this->builder->CreateLoad(this->builder->getPtrTy(), loadIndexedArray(expr)));
        return this->builder->CreateInBoundsGEP(getTypeFromNestedIndexExpr(expr->variable), loadedTarget,
                                                compileExpression(indexExpr->index));
    }

    void assignToIndexExpr(AssignStmt *assignStmt) {
        IndexExpr *indexExpr = (IndexExpr *)assignStmt->variable;
        Variable *var = new Variable();
        this->builder->CreateStore(compileExpression(assignStmt->value), getPointerToArrayIndex(indexExpr, var));
        // this->builder->CreateStore(compileExpression(assignStmt->value), loadIndexedArray(indexExpr));
    }

    void assignToVarExpr(AssignStmt *assignStmt) {
        llvm::Value *value = compileExpression(assignStmt->value);
        VarExpr *varExpr = (VarExpr *)assignStmt->variable;
        llvm::Value *variable = lookupValue(varExpr->name);
        // Check type is correct?
        this->builder->CreateStore(value, variable);
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
            return binaryOp(left, right, binaryExpr->op);
        }
        case GROUPING_EXPR: {
            GroupingExpr *groupingExpr = (GroupingExpr *)expr;
            return compileExpression(groupingExpr->expression);
        }
        case LOGICAL_EXPR: {
            LogicalExpr *logicalExpr = (LogicalExpr *)expr;

            llvm::Value *left = loadAllocaInst(compileExpression(logicalExpr->left));
            llvm::Value *right = loadAllocaInst(compileExpression(logicalExpr->right));

            if (left->getType() == this->builder->getInt1Ty() && right->getType() == left->getType()) {
                switch (logicalExpr->op) {
                case OR_LOGICAL: {
                    return this->builder->CreateLogicalOr(left, right);
                }
                case AND_LOGICAL: {
                    return this->builder->CreateLogicalAnd(left, right);
                }
                }
            }
            printf("Can't do 'and' or 'or' with these types");
            exit(1);
        }
        case LITERAL_EXPR: {
            return compileLiteral((LiteralExpr *)expr);
        }
        case DOT_EXPR: {
            // ToDo clean this mess up
            DotExpr *dotExpr = (DotExpr *)expr;

            // Check this?
            llvm::Value *value = compileExpression(dotExpr->name);
            std::string allocatedName = "";
            if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
                value = this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                allocatedName = allocaInst->getAllocatedType()->getStructName().str();

            } else if (value->getType()->isStructTy()) {
                allocatedName = value->getType()->getStructName().str();
            } else {
                printf("Didn't find property '%s' for struct \n", dotExpr->field.c_str());
                exit(1);
            }
            if (value->getType()->isStructTy() && this->structs.count(allocatedName)) {
                LLVMStruct *strukt = this->structs[allocatedName];
                for (int j = 0; j < strukt->fields.size(); j++) {
                    if (strukt->fields[j] == dotExpr->field) {
                        return this->builder->CreateExtractValue(value, j);
                    }
                }
            }
            debugValueType(value->getType(), this->ctx);
            printf("\nCan't do property lookup on non struct \n");
            exit(1);
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
                    return this->builder->CreateICmpULE(left, right);
                }
                case LESS_COMPARISON: {
                    return this->builder->CreateICmpULT(left, right);
                }
                case GREATER_COMPARISON: {
                    return this->builder->CreateICmpUGT(left, right);
                }
                case GREATER_EQUAL_COMPARISON: {
                    return this->builder->CreateICmpUGE(left, right);
                }
                case EQUAL_EQUAL_COMPARISON: {
                    return this->builder->CreateICmpEQ(left, right);
                }
                }
            }
            castIntDouble(left, right);
            if (left->getType()->isDoubleTy() && right->getType()->isDoubleTy()) {
                switch (comparisonExpr->op) {
                case LESS_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpULE(left, right);
                }
                case LESS_COMPARISON: {
                    return this->builder->CreateFCmpULT(left, right);
                }
                case GREATER_COMPARISON: {
                    return this->builder->CreateFCmpUGT(left, right);
                }
                case GREATER_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpUGE(left, right);
                }
                case EQUAL_EQUAL_COMPARISON: {
                    return this->builder->CreateFCmpOEQ(left, right);
                }
                }
            }
            printf("Can't do addition with this?\n");
            exit(1);
        }
        case UNARY_EXPR: {
            UnaryExpr *unaryExpr = (UnaryExpr *)expr;
            llvm::Value *value = loadAllocaInst(compileExpression(unaryExpr->right));
            if (unaryExpr->op == NEG_UNARY) {
                if (value->getType()->isIntegerTy() || value->getType()->isDoubleTy()) {
                    return this->builder->CreateMul(value, this->builder->getInt32(-1));
                }
                printf("Can't do '-' with this type\n");
                exit(1);
            } else if (unaryExpr->op == BANG_UNARY) {
                // Check value type?
                return this->builder->CreateXor(value, 1);
            } else {
                printf("unknown unary expr?\n");
                exit(1);
            }
        }
        case VAR_EXPR: {
            VarExpr *varExpr = (VarExpr *)expr;
            return lookupValue(varExpr->name);
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
                llvm::Value *loadedValue = this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                llvm::Value *valueOp = nullptr;
                if (incExpr->op == INC) {
                    valueOp = this->builder->CreateAdd(loadedValue, this->builder->getInt32(1));
                } else {
                    valueOp = this->builder->CreateSub(loadedValue, this->builder->getInt32(1));
                }
                return this->builder->CreateStore(valueOp, value);
            }
            printf("can't increment non allocation?\n");
            exit(1);
        }
        case ARRAY_EXPR: {
            ArrayExpr *arrayExpr = (ArrayExpr *)expr;

            // Figure ut which type array has
            llvm::Type *elementType = getLLVMTypeFromVariable(arrayExpr->itemType);

            llvm::AllocaInst *arrayInstance =
                this->builder->CreateAlloca(this->internalStructs["array"], nullptr, "array");
            // Is ptr to objects
            if (elementType == this->internalStructs["array"] || elementType->isStructTy()) {
                llvm::Value *arrGep = this->builder->CreateCall(this->libraryFuncs["malloc"],
                                                                {this->builder->getInt32(arrayExpr->items.size() * 8)});
                storeArray(arrGep, arrayInstance);
                storePtrArrayItems(arrayExpr, arrayInstance);
            } else {
                llvm::ArrayType *arrayType = llvm::ArrayType::get(elementType, arrayExpr->items.size());
                llvm::GlobalVariable *globalArray = createGlobalArray(arrayExpr, arrayType);
                llvm::Value *arrGep = this->builder->CreateGEP(
                    arrayType, globalArray, {this->builder->getInt32(0), this->builder->getInt32(0)});
                storeArray(arrGep, arrayInstance);
            }
            storeArraySize(this->builder->getInt32(arrayExpr->items.size()), arrayInstance);

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

            if (this->structs.count(name)) {
                return createStruct(callExpr);
            }

            std::vector<llvm::Value *> params = std::vector<llvm::Value *>(callExpr->arguments.size());
            for (int i = 0; i < callExpr->arguments.size(); ++i) {
                params[i] = compileExpression(callExpr->arguments[i]);
            }

            llvm::Function *func = lookupFunction(name);
            if (this->libraryFuncs.count(name)) {
                for (int i = 0; i < callExpr->arguments.size(); ++i) {
                    if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(params[i])) {
                        if (allocaInst->getAllocatedType()->isStructTy()) {
                            params[i] = loadArray(allocaInst);
                        } else {
                            params[i] = this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                        }
                    }
                }
                return this->builder->CreateCall(this->libraryFuncs[name], params);
            }

            checkCallParamCorrectness(func, params, name);
            return this->builder->CreateCall(func, params);
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
            llvm::Value *right = compileExpression(compStmt->right);
            llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(lookupValue(compStmt->name));
            llvm::Value *variable = this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
            this->builder->CreateStore(binaryOp(right, variable, compStmt->op), allocaInst);
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
            default: {
                printf("how did this happen to me\n");
                exit(1);
            }
            }
        }
        case RETURN_STMT: {
            ReturnStmt *returnStmt = (ReturnStmt *)stmt;
            // Need to check type is correct;
            if (!this->llvmFunction->enclosing) {
                printf("Can't return outside of a function\n");
                exit(1);
            }

            llvm::Value *returnValue = loadAllocaInst(compileExpression(returnStmt->value));
            // ToDo  better check for this
            // Check here if it's an allocaInst and then load it before sending
            // it back

            if (this->llvmFunction->functionType->getReturnType() != returnValue->getType()) {
                printf("funcType -> ");
                debugValueType(this->llvmFunction->functionType->getReturnType(), this->ctx);
                printf("\n");
                printf("return -> ");
                debugValueType(returnValue->getType(), this->ctx);
                printf("\n");
                printf("Mismatching return in '%s'\n", this->llvmFunction->function->getName().str().c_str());
                exit(1);
            }
            this->builder->CreateRet(returnValue);
            break;
        }
        case VAR_STMT: {
            VarStmt *varStmt = (VarStmt *)stmt;
            std::string varName = varStmt->var->name;
            if (nameIsAlreadyDeclared(varName)) {
                printf("Can't declare variable '%s', name is already "
                       "declared\n",
                       varName.c_str());
                exit(1);
            }
            llvm::Value *value = compileExpression(varStmt->initializer);
            // if (!checkVariableValueMatch(varStmt->var, value)) {
            //     printf("Invalid type mismatch in var declaration\nexpected: ");
            //     debugVariable(varStmt->var);
            //     printf("\nbut got: ");
            //     debugValueType(value->getType(), this->ctx);
            //     printf("\n");
            //     exit(1);
            // }

            // Already allocated

            if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
                allocaInst->setName(varName);
                this->llvmFunction->scopedVariables.back().push_back(allocaInst);
                break;
            }
            // ToDo, figure out when to memcpy
            if (value->getType() == this->internalStructs["array"]) {
            }
            llvm::AllocaInst *allocaInst = this->builder->CreateAlloca(value->getType(), nullptr, varName);
            this->builder->CreateStore(value, allocaInst);
            this->llvmFunction->scopedVariables.back().push_back(allocaInst);
            break;
        }
        case WHILE_STMT: {
            WhileStmt *whileStmt = (WhileStmt *)stmt;

            llvm::BasicBlock *headerBlock =
                llvm::BasicBlock::Create(*this->ctx, "header", this->llvmFunction->function);
            llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*this->ctx, "body", this->llvmFunction->function);
            llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*this->ctx, "exit", this->llvmFunction->function);

            compileLoopHeader(headerBlock, exitBlock, bodyBlock, whileStmt->condition);
            compileLoopBody(headerBlock, exitBlock, whileStmt->body);
            compileLoopExit(headerBlock, exitBlock);

            break;
        }
        case FOR_STMT: {
            ForStmt *forStmt = (ForStmt *)stmt;

            llvm::BasicBlock *headerBlock =
                llvm::BasicBlock::Create(*this->ctx, "header", this->llvmFunction->function);
            llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*this->ctx, "body", this->llvmFunction->function);
            llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*this->ctx, "exit", this->llvmFunction->function);

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
                printf("Can't declare struct '%s', name is already declared\n", structName.c_str());
                exit(1);
            }

            std::vector<llvm::Type *> fieldTypes = std::vector<llvm::Type *>(structStmt->fields.size());
            std::vector<std::string> fieldNames = std::vector<std::string>(structStmt->fields.size());

            for (int i = 0; i < fieldTypes.size(); ++i) {
                fieldTypes[i] = getLLVMTypeFromVariable(structStmt->fields[i]);
                fieldNames[i] = structStmt->fields[i]->name;
            }

            this->structs[structName] = new LLVMStruct(fieldTypes, structName, fieldNames, this->ctx);
            break;
        }
        case IF_STMT: {
            IfStmt *ifStmt = (IfStmt *)stmt;
            llvm::Value *condition = compileExpression(ifStmt->condition);

            llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*ctx, "then", this->llvmFunction->function);
            llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(*ctx, "else", this->llvmFunction->function);
            llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*ctx, "merge", this->llvmFunction->function);

            enterThenBlock(condition, thenBlock, elseBlock);
            enterElseBlock(compileBranch(ifStmt->thenBranch), elseBlock, mergeBlock);
            enterMergeBlock(compileBranch(ifStmt->elseBranch), mergeBlock);
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
                if (!this->llvmFunction->functionType->getReturnType()->isVoidTy()) {
                    printf("Non-void function does not return a value in "
                           "'%s'\n",
                           funcStmt->name.c_str());
                    exit(1);
                }
                this->builder->CreateRetVoid();
            }

            exitFuncScope(prevBuilder);
            break;
        }
        }
    }
    void addLibraryFuncs() {
        std::map<std::string, llvm::FunctionCallee> libraryFuncs = std::map<std::string, llvm::FunctionCallee>();

        std::vector<llvm::Type *> printfArgs = {llvm::Type::getInt8PtrTy(*this->ctx)};
        llvm::FunctionType *printfType = llvm::FunctionType::get(llvm::Type::getVoidTy(*this->ctx), printfArgs, true);
        llvm::FunctionCallee printfFunc = this->module->getOrInsertFunction("printf", printfType);

        libraryFuncs["printf"] = printfFunc;

        std::vector<llvm::Type *> strcatArgs = {llvm::Type::getInt8PtrTy(*this->ctx),
                                                llvm::Type::getInt8PtrTy(*this->ctx)};
        llvm::FunctionType *strcatType = llvm::FunctionType::get(this->builder->getPtrTy(), strcatArgs, true);
        llvm::FunctionCallee strcatFunc = this->module->getOrInsertFunction("strcat", strcatType);

        libraryFuncs["strcat"] = strcatFunc;

        std::vector<llvm::Type *> mallocArgs = {this->builder->getInt32Ty()};
        llvm::FunctionType *mallocType = llvm::FunctionType::get(this->builder->getPtrTy(), mallocArgs, true);
        llvm::FunctionCallee mallocFunc = this->module->getOrInsertFunction("malloc", mallocType);

        libraryFuncs["malloc"] = mallocFunc;
        this->libraryFuncs = libraryFuncs;
    }
    void addInternalStructs() {
        std::map<std::string, llvm::StructType *> strukts = {};

        // Array
        //    Int, Double, Bool, Ptr, Strng is just this
        //  Pointer to first element
        //  Size
        //  Allocated capacity
        std::vector<llvm::Type *> arrayFieldTypes = {this->builder->getPtrTy(), this->builder->getInt32Ty()};
        strukts["array"] = llvm::StructType::create(arrayFieldTypes, "array");

        std::vector<llvm::Type *> mapFieldTypes = {strukts["array"], strukts["array"]};
        strukts["map"] = llvm::StructType::create(*this->ctx, mapFieldTypes, "map");

        this->internalStructs = strukts;
    }

  public:
    LLVMCompiler(std::vector<Stmt *> stmts, std::vector<Variable *> variables) {
        this->stmts = stmts;
        this->variables = variables;
        this->ctx = new llvm::LLVMContext();
        this->module = new llvm::Module("Bonobo", *ctx);
        this->callableFunctions = std::vector<llvm::Function *>();
        llvm::FunctionType *funcType = llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
        this->llvmFunction = new LLVMFunction(nullptr, funcType, "main", {}, this->ctx, this->module);
        this->structs = std::map<std::string, LLVMStruct *>();
        this->builder = new llvm::IRBuilder<>(this->llvmFunction->entryBlock);
        this->strings = {};
        addLibraryFuncs();
        addInternalStructs();
    }
    void compile() {
        for (int i = 0; i < stmts.size(); i++) {
            compileStatement(stmts[i]);
        }
        endCompiler();
    }
};
