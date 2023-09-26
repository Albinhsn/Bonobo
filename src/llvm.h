#include "common.h"
#include "debug.h"
#include "expr.h"
#include "stmt.h"

enum LLVMType { LLVM_LITERAL, LLVM_ARRAY, LLVM_STRING, LLVM_MAP, LLVM_STRUCT };

class LLVMValue {
  private:
  public:
    LLVMType type;
};

class LLVMLiteral : public LLVMValue {
  private:
  public:
    llvm::Value *literal;
    LiteralType literalType;
    LLVMLiteral() {
        this->type = LLVM_LITERAL;
        this->literal = nullptr;
    }
};

class LLVMArray : public LLVMValue {
  private:
  public:
    llvm::Value *ptr;
    uint64_t size;
    llvm::ArrayType *arrayType;
    llvm::GlobalVariable *variable;
    LLVMArray() { this->type = LLVM_ARRAY; }
};

class LLVMString : public LLVMValue {
  private:
  public:
    llvm::Value *ptr;
    uint64_t size;
    llvm::GlobalVariable *variable;
    LLVMString() { this->type = LLVM_STRING; }
};

class LLVMMap : public LLVMValue {
  private:
    LLVMArray *keys;
    LLVMArray *values;

  public:
    LLVMMap() { this->type = LLVM_MAP; }
};

class LLVMStruct : LLVMValue {
  private:
    StructStmt *strukt;
    std::map<std::string, LLVMValue *> fields;

  public:
    LLVMStruct(StructStmt *strukt) {
        this->type = LLVM_STRUCT;
        this->strukt = strukt;
        this->fields = std::map<std::string, LLVMValue *>();
    }
};

class LLVMFunction {
  private:
  public:
    LLVMFunction *enclosing;
    std::vector<std::vector<LLVMValue *>> scopedVariables;
    std::map<std::string, int> funcArgs;
    llvm::BasicBlock *entryBlock;
    llvm::Function *function;
    llvm::FunctionType *funcType;

    LLVMFunction(LLVMFunction *enclosing, llvm::FunctionType *funcType,
                 std::string name, std::map<std::string, int> funcArgs,
                 llvm::LLVMContext *ctx, llvm::Module *module) {
        this->enclosing = enclosing;
        this->funcType = funcType;
        this->function = llvm::Function::Create(
            funcType, llvm::Function::ExternalLinkage, name, *module);
        this->scopedVariables = std::vector<std::vector<LLVMValue *>>(1);
        this->funcArgs = funcArgs;
        this->entryBlock =
            llvm::BasicBlock::Create(*ctx, "entry", this->function);
    }
};

class LLVMCompiler {
  private:
    llvm::LLVMContext *ctx;
    llvm::Module *module;
    std::vector<Stmt *> stmts;
    llvm::IRBuilder<> *builder;
    std::map<std::string, llvm::FunctionCallee> libraryFuncs;
    std::vector<llvm::Function *> callableFunctions;
    LLVMFunction *function;

    void endCompiler() {
        this->builder->CreateRet(this->builder->getInt32(0));

        std::error_code errorCode;
        llvm::raw_fd_ostream outLL("./out.ll", errorCode);
        this->module->print(outLL, nullptr);
    }

    std::string getLLVMValueName(LLVMValue *value) {
        llvm::AllocaInst *allocaInst = nullptr;
        switch (value->type) {
        case LLVM_LITERAL: {
            LLVMLiteral *literal = (LLVMLiteral *)value;
            allocaInst = llvm::dyn_cast<llvm::AllocaInst>(literal->literal);
            break;
        }
        case LLVM_STRING: {
            LLVMString *str = (LLVMString *)value;
            allocaInst = llvm::dyn_cast<llvm::AllocaInst>(str->ptr);
            break;
        }
        case LLVM_ARRAY: {
            LLVMArray *str = (LLVMArray *)value;
            allocaInst = llvm::dyn_cast<llvm::AllocaInst>(str->ptr);
            break;
        }
        }
        if (allocaInst == nullptr) {
            printf("Unable to get llvm value name\n");
            exit(1);
        }
        return allocaInst->getName().str();
    }

    llvm::Type *getLLVMValueType(LLVMValue *value) {
        if (value->type == LLVM_LITERAL) {
            LLVMLiteral *literal = (LLVMLiteral *)value;
            return literal->literal->getType();
        }
        return this->builder->getPtrTy();
    }

    LLVMValue *createLLVMValue(llvm::Value *value) {
        llvm::Type *type = value->getType();
        LLVMLiteral *literal = new LLVMLiteral();
        literal->literal = value;
        if (type == this->builder->getInt1Ty()) {
            literal->literalType = BOOL_LITERAL;
        } else if (type == this->builder->getInt32Ty()) {
            literal->literalType = INT_LITERAL;
        } else if (type->isDoubleTy()) {
            literal->literalType = DOUBLE_LITERAL;
        } else {
            LLVMString *str = new LLVMString();
            str->ptr = value;
            // printf("Can't create this value?\n");
            // exit(1);
            return str;
        }
        return literal;
    }

    bool nameIsAlreadyDeclared(std::string name) {
        for (int i = 0; i < this->function->scopedVariables.back().size();
             i++) {
            if (getLLVMValueName(this->function->scopedVariables.back()[i]) ==
                name) {
                printf("Found scopedVar\n");
                return true;
            }
        }
        for (const auto &[key, value] : this->libraryFuncs) {
            if (key == name) {
                return true;
            }
        }
        for (int i = 0; i < this->callableFunctions.size(); ++i) {
            if (this->callableFunctions[i]->getName().str() == name) {
                return true;
            }
        }

        return false;
    }
    llvm::Value *getValue(LLVMValue *value) {
        switch (value->type) {
        case LLVM_LITERAL: {
            LLVMLiteral *literal = (LLVMLiteral *)value;
            return literal->literal;
        }
        case LLVM_STRING: {
            LLVMString *string = (LLVMString *)value;
            return string->ptr;
        }
        case LLVM_ARRAY: {
            LLVMArray *array = (LLVMArray *)value;
            return array->ptr;
        }
        default: {
            printf("Don't know how to get this value\n");
            exit(1);
        }
        }
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
            return this->builder->getPtrTy();
        }
        case DOUBLE_VAR: {
            return this->builder->getDoubleTy();
        }
        case BOOL_VAR: {
            return this->builder->getInt1Ty();
        }
        case ARRAY_VAR: {
            return this->builder->getPtrTy();
        }
        case STRUCT_VAR: {
            return this->builder->getPtrTy();
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
    LLVMValue *lookupVariable(std::string name) {
        if (this->function->enclosing) {
            for (const auto &[key, value] : this->function->funcArgs) {
                if (key == name) {
                    int i = 0;
                    for (llvm::Function::arg_iterator arg =
                             this->function->function->arg_begin();
                         arg != this->function->function->arg_end(); ++arg) {
                        if (i == value) {
                            return createLLVMValue(arg);
                        }
                        ++i;
                    }
                }
            }
        }
        for (int i = this->function->scopedVariables.size() - 1; i >= 0; i--) {
            std::vector<LLVMValue *> scope = this->function->scopedVariables[i];
            for (int j = 0; j < scope.size(); ++j) {
                if (getLLVMValueName(scope[j]) == name) {
                    return scope[j];
                }
            }
        }
        printf("Unknown variable %s\n", name.c_str());
        exit(1);
    }

    bool checkVariableValueMatch(Variable *var, llvm::Value *&value) {
        if (value->getType() == llvm::Type::getInt32Ty(*this->ctx)) {
            if (var->type == DOUBLE_VAR) {
                value = this->builder->CreateUIToFP(
                    value, this->builder->getDoubleTy());
                return true;
            }
            return var->type == INT_VAR;

        } else if (value->getType() == llvm::Type::getInt1Ty(*this->ctx)) {
            return var->type == BOOL_VAR;

        } else if (value->getType() == llvm::Type::getDoubleTy(*this->ctx)) {
            if (var->type == INT_VAR) {
                value = this->builder->CreateFPToUI(
                    value, this->builder->getInt32Ty());
                return true;
            }
            return var->type == DOUBLE_VAR;
        } else if (value->getType()->isPointerTy()) {
            // ToDo this needs to check underlying type as well
            return var->type == STR_VAR || var->type == ARRAY_VAR ||
                   var->type == MAP_VAR || var->type == STRUCT_VAR;
        }

        printf("unknown value\n");
        return false;
    }
    LLVMValue *compileLiteral(LiteralExpr *expr) {
        std::string stringLiteral = expr->literal.lexeme;
        llvm::Value *literal = nullptr;
        switch (expr->literalType) {
        case STR_LITERAL: {
            literal =
                this->builder->CreateGlobalStringPtr(expr->literal.lexeme);
            break;
        }
        case INT_LITERAL: {
            literal = this->builder->getInt32(stoi(stringLiteral));
            break;
        }
        case BOOL_LITERAL: {
            if (stringLiteral == "true") {
                literal = this->builder->getInt1(1);
            } else {
                literal = this->builder->getInt1(0);
            }
            break;
        }
        case DOUBLE_LITERAL: {
            literal = llvm::ConstantFP::get(this->builder->getDoubleTy(),
                                            stod(stringLiteral));
            break;
        }
        }
        return createLLVMValue(literal);
    }
    LLVMValue *compileExpression(Expr *expr) {
        switch (expr->type) {
        case BINARY_EXPR: {
            BinaryExpr *binaryExpr = (BinaryExpr *)expr;

            LLVMValue *leftValue = compileExpression(binaryExpr->left);
            LLVMValue *rightValue = compileExpression(binaryExpr->right);
            // String concat
            if (leftValue->type == LLVM_STRING &&
                rightValue->type == LLVM_STRING) {

                return nullptr;
            }

            llvm::Value *left = getValue(leftValue);
            llvm::Value *right = getValue(rightValue);
            llvm::Type *leftType = left->getType();
            llvm::Type *rightType = right->getType();

            // Check that both are literal
            //    Create new LLVMValue after op
            llvm::Value *result = nullptr;
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
                switch (binaryExpr->op) {
                case ADD: {
                    result = this->builder->CreateAdd(left, right);
                    break;
                }
                case SUB: {
                    result = this->builder->CreateSub(left, right);
                    break;
                }
                case MUL: {
                    result = this->builder->CreateMul(left, right);
                    break;
                }
                case DIV: {
                    result = this->builder->CreateUDiv(left, right);
                    break;
                }
                }
            } else if (leftType->isDoubleTy() && rightType->isDoubleTy()) {
                switch (binaryExpr->op) {
                case ADD: {
                    result = this->builder->CreateFAdd(left, right);
                    break;
                }
                case SUB: {
                    result = this->builder->CreateFSub(left, right);
                    break;
                }
                case MUL: {
                    result = this->builder->CreateFMul(left, right);
                    break;
                }
                case DIV: {
                    result = this->builder->CreateFDiv(left, right);
                    break;
                }
                }

            } else if ((leftType->isIntegerTy() && rightType->isDoubleTy()) ||
                       (leftType->isDoubleTy() && rightType->isIntegerTy())) {
                // Cast the integer
                if (leftType->isIntegerTy()) {
                    left = this->builder->CreateUIToFP(
                        left, this->builder->getDoubleTy());
                } else {
                    right = this->builder->CreateUIToFP(
                        right, this->builder->getDoubleTy());
                }

                switch (binaryExpr->op) {
                case ADD: {
                    result = this->builder->CreateFAdd(left, right);
                    break;
                }
                case SUB: {
                    result = this->builder->CreateFSub(left, right);
                    break;
                }
                case MUL: {
                    result = this->builder->CreateFMul(left, right);
                    break;
                }
                case DIV: {
                    result = this->builder->CreateFDiv(left, right);
                    break;
                }
                }
            } else {
                printf("Can't do this addition\n");
                exit(1);
            }
            return createLLVMValue(result);
        }
        case GROUPING_EXPR: {
            GroupingExpr *groupingExpr = (GroupingExpr *)expr;
            return compileExpression(groupingExpr->expression);
        }
        case LOGICAL_EXPR: {
            LogicalExpr *logicalExpr = (LogicalExpr *)expr;
            LLVMValue *leftValue = compileExpression(logicalExpr->left);
            LLVMValue *rightValue = compileExpression(logicalExpr->right);

            if (getLLVMValueType(leftValue) == this->builder->getInt1Ty() &&
                getLLVMValueType(rightValue) == this->builder->getInt1Ty()) {
                llvm::Value *result = nullptr;
                llvm::Value *left = getValue(leftValue);
                llvm::Value *right = getValue(rightValue);

                switch (logicalExpr->op) {
                case OR_LOGICAL: {
                    result = this->builder->CreateLogicalOr(left, right);
                    break;
                }
                case AND_LOGICAL: {
                    result = this->builder->CreateLogicalAnd(left, right);
                    break;
                }
                }
                return createLLVMValue(result);
            }
            printf("Can't do 'and' or 'or' with these types");
            exit(1);
        }
        case LITERAL_EXPR: {
            return compileLiteral((LiteralExpr *)expr);
            break;
        }
        case COMPARISON_EXPR: {
            ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
            LLVMValue *leftValue = compileExpression(comparisonExpr->left);
            LLVMValue *rightValue = compileExpression(comparisonExpr->right);
            llvm::Type *leftType = getLLVMValueType(leftValue);
            llvm::Type *rightType = getLLVMValueType(rightValue);
            // Need to check fp as well, string equality, array equality,
            // map equality
            llvm::Value *left = getValue(leftValue);
            llvm::Value *right = getValue(rightValue);
            llvm::Value *result = nullptr;
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {

                switch (comparisonExpr->op) {
                case LESS_EQUAL_COMPARISON: {
                    result = this->builder->CreateICmpULE(left, right);
                    break;
                }
                case LESS_COMPARISON: {
                    result = this->builder->CreateICmpULT(left, right);
                    break;
                }
                case GREATER_COMPARISON: {
                    result = this->builder->CreateICmpUGT(left, right);
                    break;
                }
                case GREATER_EQUAL_COMPARISON: {
                    result = this->builder->CreateICmpUGE(left, right);
                    break;
                }
                case EQUAL_EQUAL_COMPARISON: {
                    result = this->builder->CreateICmpEQ(left, right);
                    break;
                }
                }
                return createLLVMValue(result);
            } else if (leftType->isDoubleTy() && rightType->isDoubleTy()) {
                switch (comparisonExpr->op) {
                case LESS_EQUAL_COMPARISON: {
                    result = this->builder->CreateFCmpULE(left, right);
                    break;
                }
                case LESS_COMPARISON: {
                    result = this->builder->CreateFCmpULT(left, right);
                    break;
                }
                case GREATER_COMPARISON: {
                    result = this->builder->CreateFCmpUGT(left, right);
                    break;
                }
                case GREATER_EQUAL_COMPARISON: {
                    result = this->builder->CreateFCmpUGE(left, right);
                    break;
                }
                case EQUAL_EQUAL_COMPARISON: {
                    result = this->builder->CreateFCmpOEQ(left, right);
                    break;
                }
                }
                return createLLVMValue(result);
            } else if ((leftType->isIntegerTy() && rightType->isDoubleTy()) ||
                       (leftType->isDoubleTy() && rightType->isIntegerTy())) {
                // Cast the integer
                if (leftType->isIntegerTy()) {
                    left = this->builder->CreateUIToFP(
                        left, this->builder->getDoubleTy());
                } else {
                    right = this->builder->CreateUIToFP(
                        right, this->builder->getDoubleTy());
                }

                switch (comparisonExpr->op) {
                case LESS_EQUAL_COMPARISON: {
                    result = this->builder->CreateFCmpULE(left, right);
                    break;
                }
                case LESS_COMPARISON: {
                    result = this->builder->CreateFCmpULT(left, right);
                    break;
                }
                case GREATER_COMPARISON: {
                    result = this->builder->CreateFCmpUGT(left, right);
                    break;
                }
                case GREATER_EQUAL_COMPARISON: {
                    result = this->builder->CreateFCmpUGE(left, right);
                    break;
                }
                case EQUAL_EQUAL_COMPARISON: {
                    result = this->builder->CreateFCmpOEQ(left, right);
                    break;
                }
                }
                return createLLVMValue(result);
            }
            printf("Can't do addition with this?\n");
            exit(1);
        }
        case UNARY_EXPR: {
            UnaryExpr *unaryExpr = (UnaryExpr *)expr;
            LLVMValue *llvmValue = compileExpression(unaryExpr->right);
            llvm::Value *value = getValue(llvmValue);
            llvm::Value *result = nullptr;
            if (unaryExpr->op == NEG_UNARY) {
                if (value->getType()->isIntegerTy() ||
                    value->getType()->isDoubleTy()) {
                    result = this->builder->CreateMul(
                        value, this->builder->getInt32(-1));
                }
                printf("Can't do '-' with this type\n");
                exit(1);
            } else if (unaryExpr->op == BANG_UNARY) {
                // Check value type?
                result = this->builder->CreateXor(value, 1);
            } else {
                printf("unknown unary expr?\n");
                exit(1);
            }
            return createLLVMValue(result);
        }
        case VAR_EXPR: {
            VarExpr *varExpr = (VarExpr *)expr;
            LLVMValue *var = lookupVariable(varExpr->name.lexeme);
            llvm::Type *varType = getLLVMValueType(var);
            if (varType->isPointerTy()) {
                if (llvm::AllocaInst *allocaInst =
                        llvm::dyn_cast<llvm::AllocaInst>(getValue(var))) {
                    if (allocaInst->getType()->isPointerTy()) {
                        llvm::Value *loadedValue = this->builder->CreateLoad(
                            allocaInst->getAllocatedType(), allocaInst);
                        return createLLVMValue(loadedValue);
                    }
                    return var;
                } else {
                    printf("failed to cast to allocaInst");
                    exit(1);
                }
            }
            return var;
        }
        case INDEX_EXPR: {
        }
        case ARRAY_EXPR: {
            ArrayExpr *arrayExpr = (ArrayExpr *)expr;

            // Compile array items
            std::vector<LLVMValue *> arrayItems =
                std::vector<LLVMValue *>(arrayExpr->items.size());
            for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
                arrayItems[i] = compileExpression(arrayExpr->items[i]);
            }

            // Figure ut which type array has
            llvm::Type *elementType = nullptr;
            if (arrayExpr->itemType == nullptr) {
            } else {
                elementType = getVariableLLVMType(arrayExpr->itemType);
            }

            uint64_t arraySize = arrayExpr->items.size();
            uint64_t totalSize =
                arraySize * elementType->getPrimitiveSizeInBits();

            llvm::ArrayType *arrayType =
                llvm::ArrayType::get(elementType, arraySize);
            llvm::GlobalVariable *globalArray = new llvm::GlobalVariable(
                *this->module, arrayType, false,
                llvm::GlobalValue::ExternalLinkage,
                llvm::ConstantAggregateZero::get(arrayType));
            for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
                llvm::Value *index = llvm::ConstantInt::get(
                    llvm::Type::getInt32Ty(*this->ctx), i);

                llvm::ArrayRef<llvm::Value *> indices({index});
                llvm::Value *gep =
                    this->builder->CreateGEP(elementType, globalArray, indices);

                this->builder->CreateStore(getValue(arrayItems[i]), gep);
            }
            return createLLVMValue(globalArray);
        }
        case MAP_EXPR: {
        }
        case CALL_EXPR: {
            CallExpr *callExpr = (CallExpr *)expr;
            std::string name = callExpr->callee.lexeme;

            std::vector<llvm::Value *> params =
                std::vector<llvm::Value *>(callExpr->arguments.size());
            for (int i = 0; i < callExpr->arguments.size(); ++i) {
                params[i] = getValue(compileExpression(callExpr->arguments[i]));
            }
            llvm::Function *func = nullptr;
            for (int i = 0; i < this->callableFunctions.size(); ++i) {
                if (this->callableFunctions[i]->getName() == name) {
                    func = this->callableFunctions[i];
                    break;
                }
            }
            if (this->function->function->getName() == name) {
                func = this->function->function;
            }
            if (this->libraryFuncs.count(name)) {
                return createLLVMValue(this->builder->CreateCall(
                    this->libraryFuncs[name], params));
            }
            if (func == nullptr) {
                printf("calling unknown func '%s'\n", name.c_str());
                exit(1);
            }
            if (((int)func->arg_size()) != params.size()) {
                printf("Calling %s requires %d params but got %d\n",
                       name.c_str(), (int)func->arg_size(), (int)params.size());
            }
            int i = 0;
            for (llvm::Argument &arg : func->args()) {
                if (arg.getType() != params[i]->getType()) {
                    printf("Invalid arg type in function %s with arg %d\n",
                           name.c_str(), i + 1);
                }
                ++i;
            }
            return createLLVMValue(this->builder->CreateCall(func, params));
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
        case ASSIGN_STMT: {
            AssignStmt *assignStmt = (AssignStmt *)stmt;
            LLVMValue *value = compileExpression(assignStmt->value);
            std::string name = assignStmt->name.lexeme;

            LLVMValue *variable = lookupVariable(name);
            // Check type is correct?
            createLLVMValue(this->builder->CreateStore(getValue(value),
                                                       getValue(variable)));
            break;
        }
        case RETURN_STMT: {
            ReturnStmt *returnStmt = (ReturnStmt *)stmt;
            // Need to check type is correct;
            if (!this->function->enclosing) {
                printf("Can't return outside of a function\n");
                exit(1);
            }

            LLVMValue *returnValue = compileExpression(returnStmt->value);
            if (this->function->funcType->getReturnType() !=
                getLLVMValueType(returnValue)) {
                printf("Mismatching return in %s\n",
                       this->function->function->getName().str().c_str());
                exit(1);
            }

            this->builder->CreateRet(getValue(returnValue));
            break;
        }
        case VAR_STMT: {
            // ToDo check overwriting existing variable?
            VarStmt *varStmt = (VarStmt *)stmt;

            if (nameIsAlreadyDeclared(varStmt->var->name.lexeme)) {
                printf("Can't declare variable '%s', name is already "
                       "declared\n",
                       varStmt->var->name.lexeme.c_str());
                exit(1);
            }
            LLVMValue *value = compileExpression(varStmt->initializer);
            llvm::Value *val = getValue(value);
            if (!checkVariableValueMatch(varStmt->var, val)) {

                printf("Invalid type mismatch in var declaration\nexpected: ");
                debugVariable(varStmt->var);
                printf("\nbut got: ");
                debugValueType(getLLVMValueType(value), this->ctx);
                exit(1);
            }
            value = createLLVMValue(val);
            llvm::AllocaInst *var =
                this->builder->CreateAlloca(getVariableLLVMType(varStmt->var),
                                            nullptr, varStmt->var->name.lexeme);
            this->builder->CreateStore(getValue(value), var);
            this->function->scopedVariables.back().push_back(
                createLLVMValue(var));
            break;
        }
        case WHILE_STMT: {
            WhileStmt *whileStmt = (WhileStmt *)stmt;

            llvm::BasicBlock *loopHeaderBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.header", this->function->function);

            llvm::BasicBlock *loopBodyBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.body", this->function->function);

            llvm::BasicBlock *loopExitBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.exit", this->function->function);

            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            LLVMValue *condition = compileExpression(whileStmt->condition);

            this->builder->CreateCondBr(getValue(condition), loopBodyBlock,
                                        loopExitBlock);
            this->builder->SetInsertPoint(loopBodyBlock);
            for (int i = 0; i < whileStmt->body.size(); ++i) {
                compileStatement(whileStmt->body[i]);
            }

            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopExitBlock);

            break;
        }
        case FOR_STMT: {
            ForStmt *forStmt = (ForStmt *)stmt;

            llvm::BasicBlock *loopHeaderBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.header", this->function->function);

            llvm::BasicBlock *loopBodyBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.body", this->function->function);

            llvm::BasicBlock *loopExitBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.exit", this->function->function);

            compileStatement(forStmt->initializer);
            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            LLVMValue *condition = compileExpression(forStmt->condition);

            this->builder->CreateCondBr(getValue(condition), loopBodyBlock,
                                        loopExitBlock);
            this->builder->SetInsertPoint(loopBodyBlock);
            for (int i = 0; i < forStmt->body.size(); ++i) {
                compileStatement(forStmt->body[i]);
            }
            compileStatement(forStmt->increment);

            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopExitBlock);
            break;
        }
        case STRUCT_STMT: {
        }
        case IF_STMT: {
            IfStmt *ifStmt = (IfStmt *)stmt;
            LLVMValue *condition = compileExpression(ifStmt->condition);

            llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(
                *ctx, "then", this->function->function);

            llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(
                *ctx, "else", this->function->function);

            llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(
                *ctx, "merge", this->function->function);

            this->builder->CreateCondBr(getValue(condition), thenBlock,
                                        elseBlock);
            this->builder->SetInsertPoint(thenBlock);

            this->function->scopedVariables.push_back(
                std::vector<LLVMValue *>());
            // This is just bad, especially for multiple returns?
            bool returned = false;
            for (int i = 0; i < ifStmt->thenBranch.size(); ++i) {
                compileStatement(ifStmt->thenBranch[i]);
                if (ifStmt->thenBranch[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
            this->function->scopedVariables.pop_back();
            if (!returned) {
                this->builder->CreateBr(mergeBlock);
            }
            this->builder->SetInsertPoint(elseBlock);

            returned = false;
            this->function->scopedVariables.push_back(
                std::vector<LLVMValue *>());
            for (int i = 0; i < ifStmt->elseBranch.size(); ++i) {
                compileStatement(ifStmt->elseBranch[i]);
                if (ifStmt->elseBranch[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
            this->function->scopedVariables.pop_back();
            if (!returned) {
                this->builder->CreateBr(mergeBlock);
            }
            this->builder->SetInsertPoint(mergeBlock);
            break;
        }
        case FUNC_STMT: {
            FuncStmt *funcStmt = (FuncStmt *)stmt;

            if (nameIsAlreadyDeclared(funcStmt->name.lexeme)) {
                printf("Can't declare function '%s', name is already "
                       "declared\n",
                       funcStmt->name.lexeme.c_str());
                exit(1);
            }

            // Fix params
            std::vector<llvm::Type *> params =
                std::vector<llvm::Type *>(funcStmt->params.size());
            std::map<std::string, int> funcArgs;
            for (int i = 0; i < funcStmt->params.size(); ++i) {
                params[i] = getVariableLLVMType(funcStmt->params[i]);
                funcArgs[funcStmt->params[i]->name.lexeme] = i;
            }

            // Ret type
            llvm::Type *returnType = getVariableLLVMType(funcStmt->returnType);

            // Fix func type
            llvm::FunctionType *funcType =
                llvm::FunctionType::get(returnType, params, false);

            if (this->function->enclosing) {
                printf("Can't declare a function in a function\n");
                exit(1);
            }

            this->function = new LLVMFunction(this->function, funcType,
                                              funcStmt->name.lexeme, funcArgs,
                                              this->ctx, this->module);
            llvm::IRBuilder<> *prevBuilder = this->builder;
            this->builder = new llvm::IRBuilder<>(this->function->entryBlock);
            bool returned = false;
            for (int i = 0; i < funcStmt->body.size(); ++i) {
                compileStatement(funcStmt->body[i]);
                if (funcStmt->body[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
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
            this->callableFunctions.push_back(this->function->function);
            this->function = this->function->enclosing;
            this->builder = prevBuilder;
            break;
        }
        }
    }
    std::map<std::string, llvm::FunctionCallee> addLibraryFuncs() {
        std::map<std::string, llvm::FunctionCallee> libraryFuncs =
            std::map<std::string, llvm::FunctionCallee>();
        std::vector<llvm::Type *> printfArgs;
        printfArgs.push_back(llvm::Type::getInt8PtrTy(*this->ctx));
        llvm::FunctionType *printfType = llvm::FunctionType::get(
            llvm::Type::getInt32Ty(*this->ctx), printfArgs, true);
        llvm::FunctionCallee printfFunc =
            this->module->getOrInsertFunction("printf", printfType);

        libraryFuncs["printf"] = printfFunc;

        return libraryFuncs;
    }

  public:
    LLVMCompiler(std::vector<Stmt *> stmts) {
        this->stmts = stmts;
        this->ctx = new llvm::LLVMContext();
        this->module = new llvm::Module("Bonobo", *ctx);
        this->callableFunctions = std::vector<llvm::Function *>();
        llvm::FunctionType *funcType =
            llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
        this->function = new LLVMFunction(nullptr, funcType, "main", {},
                                          this->ctx, this->module);

        this->libraryFuncs = addLibraryFuncs();
        this->builder = new llvm::IRBuilder<>(this->function->entryBlock);
    }
    void compile() {
        for (int i = 0; i < stmts.size(); i++) {
            compileStatement(stmts[i]);
        }
        endCompiler();
    }
};
