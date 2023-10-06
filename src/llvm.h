#include "common.h"
#include "debug.h"
#include "expr.h"
#include "stmt.h"
#include "variables.h"
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
    std::map<std::string, int> funcArgs;
    std::map<llvm::Value *, llvm::Type *> arrayElements;
    llvm::BasicBlock *entryBlock;
    ExitBlock *exitBlock;
    llvm::Function *function;
    llvm::FunctionType *funcType;

    LLVMFunction(LLVMFunction *enclosing, llvm::FunctionType *funcType, std::string name,
                 std::map<std::string, int> funcArgs, llvm::LLVMContext *ctx, llvm::Module *module) {
        this->broke = false;
        this->exitBlock = nullptr;
        this->enclosing = enclosing;
        this->funcType = funcType;
        this->function = llvm::Function::Create(funcType, llvm::Function::ExternalLinkage, name, *module);
        this->scopedVariables = std::vector<std::vector<llvm::AllocaInst *>>(1);
        this->funcArgs = funcArgs;
        this->entryBlock = llvm::BasicBlock::Create(*ctx, "entry", this->function);
    }
};

class LLVMCompiler {
  private:
    llvm::IRBuilder<> *builder;
    std::vector<llvm::Function *> callableFunctions;
    llvm::LLVMContext *ctx;
    LLVMFunction *function;
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
        for (int i = 0; i < this->function->scopedVariables.back().size(); i++) {
            if (this->function->scopedVariables.back()[i]->getName().str() == name) {
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
    bool compileFuncBody(std::vector<Stmt *> body) {
        for (int i = 0; i < body.size(); ++i) {
            compileStatement(body[i]);
            if (body[i]->type == RETURN_STMT) {
                return true;
            }
        }
        return false;
    }

    bool compileBranch(std::vector<Stmt *> branch) {
        for (int i = 0; i < branch.size(); ++i) {
            if (branch[i]->type == BREAK_STMT) {
                this->function->broke = true;
                this->builder->CreateBr(this->function->exitBlock->exitBlock);
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
        this->function->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());
    }

    void enterMergeBlock(bool returned, llvm::BasicBlock *mergeBlock) {
        this->function->scopedVariables.pop_back();
        if (!returned && !this->function->broke) {
            this->builder->CreateBr(mergeBlock);
        }
        this->builder->SetInsertPoint(mergeBlock);
    }

    void enterElseBlock(bool returned, llvm::BasicBlock *elseBlock, llvm::BasicBlock *mergeBlock) {
        this->function->scopedVariables.pop_back();
        if (!returned && !this->function->broke) {
            this->builder->CreateBr(mergeBlock);
        }
        this->builder->SetInsertPoint(elseBlock);
        this->function->broke = false;
        this->function->scopedVariables.push_back(std::vector<llvm::AllocaInst *>());
    }

    llvm::IRBuilder<> *enterFuncScope(FuncStmt *funcStmt) {
        // Fix params
        std::vector<llvm::Type *> params = std::vector<llvm::Type *>(funcStmt->params.size());
        std::map<std::string, int> funcArgs;
        for (int i = 0; i < funcStmt->params.size(); ++i) {
            params[i] = getVariableLLVMType(funcStmt->params[i]);
            funcArgs[funcStmt->params[i]->name.lexeme] = i;
        }

        // Ret type
        llvm::Type *returnType = getVariableLLVMType(funcStmt->returnType);

        // Fix func type
        llvm::FunctionType *funcType = llvm::FunctionType::get(returnType, params, false);
        this->function =
            new LLVMFunction(this->function, funcType, funcStmt->name.lexeme, funcArgs, this->ctx, this->module);
        llvm::IRBuilder<> *prevBuilder = this->builder;
        this->builder = new llvm::IRBuilder<>(this->function->entryBlock);
        return prevBuilder;
    }

    void checkValidFuncDeclaration(FuncStmt *funcStmt) {
        if (nameIsAlreadyDeclared(funcStmt->name.lexeme)) {
            printf("Can't declare function '%s', name is already "
                   "declared\n",
                   funcStmt->name.lexeme.c_str());
            exit(1);
        }
        if (this->function->enclosing) {
            printf("Can't declare a function in a function\n");
            exit(1);
        }
    }

    void checkReturnType(bool returned, FuncStmt *funcStmt) {
        if (!returned) {
            if (!this->function->funcType->getReturnType()->isVoidTy()) {
                printf("Non-void function does not return a value in "
                       "'%s'\n",
                       funcStmt->name.lexeme.c_str());
                exit(1);
            } else {
                this->builder->CreateRetVoid();
            }
        }
    }

    void exitFuncScope(llvm::IRBuilder<> *prevBuilder) {
        this->callableFunctions.push_back(this->function->function);
        this->function = this->function->enclosing;
        this->builder = prevBuilder;
    }

    llvm::Type *getArrayElementType(Variable *itemType) {
        if (itemType != nullptr) {
            if (itemType->type == INT_VAR) {
                return this->builder->getInt32Ty();
            } else if (itemType->type == DOUBLE_VAR) {
                return this->builder->getDoubleTy();
            } else if (itemType->type == BOOL_VAR) {
                return this->builder->getInt1Ty();
            } else if (itemType->type == ARRAY_VAR) {
                return this->internalStructs["array"];
            } else if (itemType->type == STRUCT_VAR) {
                StructVariable *structVar = (StructVariable *)itemType;
                if (this->structs.count(structVar->structName.lexeme)) {
                    return this->structs[structVar->structName.lexeme]->structType;
                } else {
                    printf("trying to lookup unknown struct '%s'\n", structVar->structName.lexeme.c_str());
                    exit(1);
                }
            } else if (itemType->type == STR_VAR) {
                return this->internalStructs["array"];
            }
        }
        printf("trying to lookup unknown var \n");
        exit(1);
    }

    llvm::Type *getVariableLLVMType(Variable *var) {
        if (var == nullptr) {
            printf("Variable is none in getVariableLLVMType\n");
            exit(1);
        }
        switch (var->type) {
        case INT_VAR: {
            return this->builder->getInt32Ty();
        }
        case STR_VAR: {
            return this->internalStructs["array"];
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
        case STRUCT_VAR: {
            StructVariable *structVar = (StructVariable *)var;
            if (this->structs.count(structVar->name.lexeme)) {
                return this->structs[structVar->name.lexeme]->structType;
            }
            printf("trying to return unknown struct '%s'\n", structVar->structName.lexeme.c_str());
            exit(1);
        }
        case NIL_VAR: {
            return this->builder->getVoidTy();
        }
        default: {
            printf("unknown llvmType\n");
            exit(1);
        }
        }
    }
    llvm::MaybeAlign getAlignment(llvm::Type *type) { return llvm::MaybeAlign(type->getPrimitiveSizeInBits() / 8); }

    llvm::Function *lookupFunction(std::string name) {
        for (int i = 0; i < this->callableFunctions.size(); ++i) {
            if (this->callableFunctions[i]->getName() == name) {
                return this->callableFunctions[i];
            }
        }
        return this->function->function->getName() == name ? this->function->function : nullptr;
    }

    llvm::Value *lookupVariable(std::string name) {
        if (this->function->enclosing) {
            if (this->function->funcArgs.count(name)) {
                int i = 0, value = this->function->funcArgs[name];
                for (llvm::Function::arg_iterator arg = this->function->function->arg_begin();
                     arg != this->function->function->arg_end(); ++arg) {
                    if (i == value) {
                        return arg;
                    }
                    ++i;
                }
            }
        }
        for (int i = this->function->scopedVariables.size() - 1; i >= 0; i--) {
            std::vector<llvm::AllocaInst *> scopeVars = this->function->scopedVariables[i];

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
        if (!this->function->broke) {
            if (stmt != nullptr) {
                compileStatement(stmt);
            }
            this->function->broke = false;
            this->builder->CreateBr(headerBlock);
        }

        this->function->exitBlock = this->function->exitBlock->prev;
        this->builder->SetInsertPoint(exitBlock);
    }

    void compileLoopBody(llvm::BasicBlock *headerBlock, llvm::BasicBlock *exitBlock, std::vector<Stmt *> body) {
        for (int i = 0; i < body.size(); ++i) {
            if (body[i]->type == BREAK_STMT) {
                this->function->broke = true;
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

        this->function->exitBlock = new ExitBlock(this->function->exitBlock, exitBlock);
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

    void checkIndexCorrectness(llvm::Type *type, llvm::Value *index, llvm::Value *loadedVar) {
        if (type == nullptr) {
            printf("couldn't find var type\n");
            exit(1);
        }
        if (!index->getType()->isIntegerTy()) {
            printf("Can't index array with something other then int\n");
            exit(1);
        }

        if (!loadedVar->getType()->isStructTy()) {
            printf("can't index non struct - ");
            debugValueType(loadedVar->getType(), this->ctx);
            printf("\n");
            exit(1);
        }
    }

    bool checkVariableValueMatch(Variable *var, llvm::Value *&value) {
        if (value->getType() == llvm::Type::getInt32Ty(*this->ctx)) {
            if (var->type == DOUBLE_VAR) {
                value = this->builder->CreateUIToFP(value, this->builder->getDoubleTy());
                return true;
            }
            return var->type == INT_VAR;

        } else if (value->getType() == llvm::Type::getInt1Ty(*this->ctx)) {
            return var->type == BOOL_VAR;

        } else if (value->getType() == llvm::Type::getDoubleTy(*this->ctx)) {
            if (var->type == INT_VAR) {
                value = this->builder->CreateFPToUI(value, this->builder->getInt32Ty());
                return true;
            }
            return var->type == DOUBLE_VAR;
        } else if (value->getType()->isStructTy()) {
            // ToDo this needs to check underlying type as well
            return var->type == STR_VAR || var->type == ARRAY_VAR || var->type == MAP_VAR || var->type == STRUCT_VAR;
        }
        if (value->getType()->isPointerTy()) {
            printf("was pointer\n");
        } else if (value->getType() == llvm::Type::getInt8Ty(*this->ctx)) {
            printf("was int8\n");
        } else {
            printf("unknown value\n");
        }

        return false;
    }

    llvm::Value *compileLiteral(LiteralExpr *expr) {
        std::string stringLiteral = expr->literal.lexeme;
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
            if (stringLiteral == "true") {
                return this->builder->getInt1(1);
            } else {
                return this->builder->getInt1(0);
            }
        }
        case DOUBLE_LITERAL: {
            return llvm::ConstantFP::get(this->builder->getDoubleTy(), stod(stringLiteral));
        }
        }
        printf("how did we get here?\n");
        exit(1);
    }

    llvm::Value *loadArray(llvm::Value *value) {
        llvm::AllocaInst *valueAlloca = llvm::dyn_cast<llvm::AllocaInst>(value);
        llvm::Value *ptr = this->builder->CreateStructGEP(this->internalStructs["array"], value, 0);
        return this->builder->CreateLoad(this->builder->getPtrTy(), ptr);
    }

    llvm::Value *loadArraySize(llvm::Value *value) {
        llvm::AllocaInst *valueAlloca = llvm::dyn_cast<llvm::AllocaInst>(value);
        llvm::Value *ptr = this->builder->CreateStructGEP(this->internalStructs["array"], value, 1);
        return this->builder->CreateLoad(this->builder->getInt32Ty(), ptr);
    }

    llvm::Value *loadAllocaInst(llvm::Value *value) {
        if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(value)) {
            return this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
        }
        return value;
    }

    llvm::AllocaInst *createAllocation(llvm::Value *value, std::string name) {
        llvm::AllocaInst *allocaInst = this->builder->CreateAlloca(value->getType(), nullptr, name);
        this->builder->CreateStore(value, allocaInst);
        return allocaInst;
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
        for (int i = 0; i < arrayExpr->items.size(); ++i) {
            llvm::Value *arrValue = compileExpression(arrayExpr->items[i]);
            llvm::Value *gep =
                this->builder->CreateInBoundsGEP(this->internalStructs["array"], arrayInstance,
                                                 {this->builder->getInt32(0), this->builder->getInt32(0)});
            llvm::Value *arrPtr = this->builder->CreateLoad(this->builder->getPtrTy(), gep);
            llvm::Value *arrInboundPtr = this->builder->CreateInBoundsGEP(
                this->builder->getPtrTy(), arrPtr,
                this->builder->CreateSExt(this->builder->getInt32(i), this->builder->getInt64Ty()));
            this->builder->CreateStore(arrValue, arrInboundPtr);
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
        for (int i = 0; i < this->strings.size(); ++i) {
            if (value == this->strings[i]) {
                return true;
            }
        }
        return false;
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
        std::string name = callExpr->callee.lexeme;
        LLVMStruct *strukt = this->structs[name];
        llvm::AllocaInst *structInstance = builder->CreateAlloca(strukt->structType, nullptr, name);

        if (callExpr->arguments.size() != strukt->fields.size()) {
            printf("Strukt has different amount of args, expected: "
                   "%d but got %d",
                   (int)strukt->fields.size(), (int)callExpr->arguments.size());
            exit(1);
        }

        for (int arg = 0; arg < callExpr->arguments.size(); ++arg) {
            llvm::Value *paramValue = compileExpression(callExpr->arguments[arg]);
            if (strukt->structType->getContainedType(arg) != paramValue->getType()) {
                printf("Param %d does match it's type\n", arg);
                exit(1);
            }
            storeStructField(strukt->structType, structInstance, paramValue, arg);
        }
        return structInstance;
    }

    llvm::Type *getIndexVarType(std::string varName) {
        for (int i = 0; i < this->variables.size(); ++i) {
            if (this->variables[i]->name.lexeme == varName) {
                return lookupArrayItemType(this->variables[i]);
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
        }
        }
        printf("Can't lookup this var type ");
        debugVariable(var);
        printf("\n");
        exit(1);
    }

    void castIntDouble(llvm::Value *&left, llvm::Value *&right) {
        llvm::Type *leftType = left->getType();
        llvm::Type *rightType = right->getType();
        if ((leftType->isIntegerTy() && rightType->isDoubleTy()) ||
            (leftType->isDoubleTy() && rightType->isIntegerTy())) {
            if (leftType->isIntegerTy()) {
                left = this->builder->CreateUIToFP(left, this->builder->getDoubleTy());
            } else {
                right = this->builder->CreateUIToFP(right, this->builder->getDoubleTy());
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
            left = loadAllocaInst(left);
            right = loadAllocaInst(right);

            // Check that both are literal
            //    Create new LLVMValue after op
            if (left->getType()->isIntegerTy() && right->getType()->isIntegerTy()) {
                switch (binaryExpr->op) {
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
                switch (binaryExpr->op) {
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
            if (llvm::AllocaInst *allocaInst = llvm::dyn_cast<llvm::AllocaInst>(compileExpression(dotExpr->name))) {
                llvm::Value *loadedValue = this->builder->CreateLoad(allocaInst->getAllocatedType(), allocaInst);
                std::string allocatedName = allocaInst->getAllocatedType()->getStructName().str();

                if (loadedValue->getType()->isStructTy() && this->structs.count(allocatedName)) {
                    LLVMStruct *strukt = this->structs[allocatedName];
                    for (int j = 0; j < strukt->fields.size(); j++) {
                        if (strukt->fields[j] == dotExpr->field.lexeme) {
                            return this->builder->CreateExtractValue(loadedValue, j);
                        }
                    }
                }
                printf("Didn't find property '%s' for struct \n", dotExpr->field.lexeme.c_str());
                exit(1);
            }
            printf("Can't do property lookup on non struct \n");
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
            return lookupVariable(varExpr->name.lexeme);
        }
        case INDEX_EXPR: {
            // ToDo if string -> create new one
            IndexExpr *indexExpr = (IndexExpr *)expr;
            llvm::Value *variable = compileExpression(indexExpr->variable);
            llvm::Value *index = compileExpression(indexExpr->index);

            if (llvm::AllocaInst *castedVar = llvm::dyn_cast<llvm::AllocaInst>(variable)) {
                llvm::Type *type = getIndexVarType(castedVar->getName().str());

                llvm::Value *loadedVar = this->builder->CreateLoad(castedVar->getAllocatedType(), castedVar);
                checkIndexCorrectness(type, index, loadedVar);

                llvm::Value *loadedArray = loadArray(castedVar);
                if (type == this->internalStructs["array"]) {
                    llvm::Value *idxGEP =
                        this->builder->CreateInBoundsGEP(this->builder->getPtrTy(), loadedArray,
                                                         this->builder->CreateSExt(index, this->builder->getInt64Ty()));
                    llvm::Value *loadedPtr = this->builder->CreateLoad(this->builder->getPtrTy(), idxGEP);

                    llvm::Value *idxGEP2 = this->builder->CreateInBoundsGEP(
                        type, loadedPtr, {this->builder->getInt32(0), this->builder->getInt32(0)});
                    return this->builder->CreateLoad(type, idxGEP2);
                } else if (type->isStructTy()) {
                    llvm::Value *idxGEP =
                        this->builder->CreateInBoundsGEP(this->builder->getPtrTy(), loadedArray,
                                                         this->builder->CreateSExt(index, this->builder->getInt64Ty()));
                    llvm::Value *ptr = this->builder->CreateLoad(this->builder->getPtrTy(), idxGEP);
                    return this->builder->CreateLoad(type, ptr);

                } else {
                    llvm::Value *idxGEP = this->builder->CreateInBoundsGEP(type, loadedArray, index);
                    return this->builder->CreateLoad(type, idxGEP);
                }
            } else if (variable->getType() == this->internalStructs["array"]) {
                llvm::Value *loadedArray = loadArray(variable);
                if (this->function->arrayElements.count(loadedArray) == 0) {
                    printf("couldn't find array\n");
                }
                printf("could find array\n");
                llvm::Type *type = this->function->arrayElements[loadedArray];
                exit(1);
            }
            printf("couldn't cast index variable, was type: ");
            debugValueType(variable->getType(), this->ctx);
            printf("\n");
            exit(1);
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
            llvm::Type *elementType = getArrayElementType(arrayExpr->itemType);

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
            std::string name = callExpr->callee.lexeme;

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
        case BREAK_STMT: {
            break;
        }
        case ASSIGN_STMT: {
            AssignStmt *assignStmt = (AssignStmt *)stmt;
            llvm::Value *value = compileExpression(assignStmt->value);
            llvm::Value *variable = lookupVariable(assignStmt->name.lexeme);
            // Check type is correct?
            this->builder->CreateStore(value, variable);
            break;
        }
        case RETURN_STMT: {
            ReturnStmt *returnStmt = (ReturnStmt *)stmt;
            // Need to check type is correct;
            if (!this->function->enclosing) {
                printf("Can't return outside of a function\n");
                exit(1);
            }

            llvm::Value *returnValue = loadAllocaInst(compileExpression(returnStmt->value));
            // ToDo  better check for this
            // Check here if it's an allocaInst and then load it before sending
            // it back

            if (this->function->funcType->getReturnType() != returnValue->getType()) {
                printf("funcType -> ");
                debugValueType(this->function->funcType->getReturnType(), this->ctx);
                printf("\n");
                printf("return -> ");
                debugValueType(returnValue->getType(), this->ctx);
                printf("\n");
                printf("Mismatching return in '%s'\n", this->function->function->getName().str().c_str());
                exit(1);
            }
            this->builder->CreateRet(returnValue);
            break;
        }
        case VAR_STMT: {
            VarStmt *varStmt = (VarStmt *)stmt;
            std::string varName = varStmt->var->name.lexeme;
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
                this->function->scopedVariables.back().push_back(allocaInst);
                break;
            }

            this->function->scopedVariables.back().push_back(createAllocation(value, varName));
            break;
        }
        case WHILE_STMT: {
            WhileStmt *whileStmt = (WhileStmt *)stmt;

            llvm::BasicBlock *headerBlock = llvm::BasicBlock::Create(*this->ctx, "header", this->function->function);
            llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*this->ctx, "body", this->function->function);
            llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*this->ctx, "exit", this->function->function);

            compileLoopHeader(headerBlock, exitBlock, bodyBlock, whileStmt->condition);
            compileLoopBody(headerBlock, exitBlock, whileStmt->body);
            compileLoopExit(headerBlock, exitBlock);

            break;
        }
        case FOR_STMT: {
            ForStmt *forStmt = (ForStmt *)stmt;

            llvm::BasicBlock *headerBlock = llvm::BasicBlock::Create(*this->ctx, "header", this->function->function);
            llvm::BasicBlock *bodyBlock = llvm::BasicBlock::Create(*this->ctx, "body", this->function->function);
            llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(*this->ctx, "exit", this->function->function);

            compileStatement(forStmt->initializer);
            compileLoopHeader(headerBlock, exitBlock, bodyBlock, forStmt->condition);
            compileLoopBody(headerBlock, exitBlock, forStmt->body);
            compileLoopExit(headerBlock, exitBlock, forStmt->increment);

            break;
        }
        case STRUCT_STMT: {
            StructStmt *structStmt = (StructStmt *)stmt;
            std::string structName = structStmt->name.lexeme;
            if (nameIsAlreadyDeclared(structStmt->name.lexeme)) {
                printf("Can't declare struct '%s', name is already declared\n", structName.c_str());
                exit(1);
            }

            std::vector<llvm::Type *> fieldTypes = std::vector<llvm::Type *>(structStmt->fields.size());
            std::vector<std::string> fieldNames = std::vector<std::string>(structStmt->fields.size());

            for (int i = 0; i < fieldTypes.size(); ++i) {
                fieldTypes[i] = getVariableLLVMType(structStmt->fields[i]);
                fieldNames[i] = structStmt->fields[i]->name.lexeme;
            }

            this->structs[structName] = new LLVMStruct(fieldTypes, structName, fieldNames, this->ctx);
            break;
        }
        case IF_STMT: {
            IfStmt *ifStmt = (IfStmt *)stmt;
            llvm::Value *condition = compileExpression(ifStmt->condition);

            llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(*ctx, "then", this->function->function);
            llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(*ctx, "else", this->function->function);
            llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(*ctx, "merge", this->function->function);

            enterThenBlock(condition, thenBlock, elseBlock);
            enterElseBlock(compileBranch(ifStmt->thenBranch), elseBlock, mergeBlock);
            enterMergeBlock(compileBranch(ifStmt->elseBranch), mergeBlock);
            break;
        }
        case FUNC_STMT: {
            FuncStmt *funcStmt = (FuncStmt *)stmt;
            checkValidFuncDeclaration(funcStmt);
            llvm::IRBuilder<> *prevBuilder = enterFuncScope(funcStmt);

            checkReturnType(compileFuncBody(funcStmt->body), funcStmt);

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
        this->function = new LLVMFunction(nullptr, funcType, "main", {}, this->ctx, this->module);
        this->structs = std::map<std::string, LLVMStruct *>();
        this->builder = new llvm::IRBuilder<>(this->function->entryBlock);
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
