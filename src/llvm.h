#include "common.h"
#include "debug.h"
#include "expr.h"
#include "stmt.h"

using namespace llvm;
using namespace std;

class LLVMFunction {
  private:
  public:
    LLVMFunction *enclosing;
    // Scopes
    vector<AllocaInst *> localVariables;
    map<string, int> funcArgs;
    BasicBlock *entryBlock;
    Function *function;
    FunctionType *funcType;

    LLVMFunction(LLVMFunction *enclosing, FunctionType *funcType,
                 string name, map<string, int> funcArgs, LLVMContext *ctx,
                 Module *module) {
        this->enclosing = enclosing;
        this->funcType = funcType;
        this->function = Function::Create(funcType, Function::ExternalLinkage,
                                          name, *module);
        this->funcArgs = funcArgs;
        this->entryBlock = BasicBlock::Create(*ctx, "entry", this->function);
    }
};

class LLVMCompiler {
  private:
    LLVMContext *ctx;
    Module *module;
    vector<AllocaInst *> globalVariables;
    vector<Stmt *> stmts;
    IRBuilder<> *builder;
    map<string, FunctionCallee> libraryFuncs;
    vector<Function *> callableFunctions;
    LLVMFunction *function;

    void endCompiler() {
        this->builder->CreateRet(this->builder->getInt32(0));

        error_code errorCode;
        raw_fd_ostream outLL("./out.ll", errorCode);
        this->module->print(outLL, nullptr);
    }

    bool nameIsAlreadyDeclared(string name) {
        for (int i = 0; i < this->globalVariables.size(); ++i) {
            if (this->globalVariables[i]->getName().str() == name) {
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
    Type *getVariableLLVMType(Variable *var) {
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
    Value *lookupVariable(string name) {
        if (this->function->enclosing) {
            for (const auto &[key, value] : this->function->funcArgs) {
                if (key == name) {
                    int i = 0;
                    for (Function::arg_iterator arg =
                             this->function->function->arg_begin();
                         arg != this->function->function->arg_end(); ++arg) {
                        if (i == value) {
                            return arg;
                        }
                        ++i;
                    }
                }
            }
            for (int i = 0; i < this->function->localVariables.size(); i++) {
                if (this->function->localVariables[i]->getName() == name) {
                    return this->function->localVariables[i];
                }
            }
        }
        for (int i = 0; i < this->globalVariables.size(); i++) {
            if (this->globalVariables[i]->getName() == name) {
                return this->globalVariables[i];
            }
        }
        printf("Unknown variable %s\n", name.c_str());
        exit(1);
    }

    bool checkVariableValueMatch(Variable *var, Value *value) {
        if (value->getType() == Type::getInt32Ty(*this->ctx)) {
            return var->type == INT_VAR;

        } else if (value->getType() == Type::getInt1Ty(*this->ctx)) {
            return var->type == BOOL_VAR;

        } else if (value->getType() == Type::getDoubleTy(*this->ctx)) {
            return var->type == DOUBLE_VAR;
        } else if (value->getType()->isPointerTy()) {
            // ToDo this needs to check underlying type as well
            return var->type == STR_VAR || var->type == ARRAY_VAR ||
                   var->type == MAP_VAR || var->type == STRUCT_VAR;
        }

        printf("unknown value\n");
        return false;
    }
    Value *compileLiteral(LiteralExpr *expr) {
        string stringLiteral = expr->literal.lexeme;
        switch (expr->literalType) {
        case STR_LITERAL: {
            return this->builder->CreateGlobalStringPtr(expr->literal.lexeme);
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
            return ConstantFP::get(this->builder->getDoubleTy(),
                                         stod(stringLiteral));
        }
        }
    }
    Value *compileExpression(Expr *expr) {
        switch (expr->type) {
        case BINARY_EXPR: {
            BinaryExpr *binaryExpr = (BinaryExpr *)expr;

            Value *left = compileExpression(binaryExpr->left);
            Type *leftType = left->getType();

            Value *right = compileExpression(binaryExpr->right);
            Type *rightType = right->getType();

            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
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
            } else if (leftType->isDoubleTy() && rightType->isDoubleTy()) {
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
            // This should be string concat
            else if (leftType->isPointerTy() && rightType->isPointerTy()) {
            }
            printf("Can't do this addition\n");
            exit(1);
        }
        case GROUPING_EXPR: {
            GroupingExpr *groupingExpr = (GroupingExpr *)expr;
            return compileExpression(groupingExpr->expression);
        }
        case LOGICAL_EXPR: {
            LogicalExpr *logicalExpr = (LogicalExpr *)expr;
            Value *left = compileExpression(logicalExpr->left);
            Value *right = compileExpression(logicalExpr->right);
            if (left->getType() == this->builder->getInt1Ty() &&
                right->getType() == this->builder->getInt1Ty()) {

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
            break;
        }
        case COMPARISON_EXPR: {
            ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
            Value *left = compileExpression(comparisonExpr->left);
            Type *leftType = left->getType();
            Value *right = compileExpression(comparisonExpr->right);
            Type *rightType = right->getType();
            // Need to check fp as well, string equality, array equality,
            // map equality
            if (leftType->isIntegerTy() && rightType->isIntegerTy()) {
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
            } else if (leftType->isDoubleTy() && rightType->isDoubleTy()) {
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
        }
        case UNARY_EXPR: {
            UnaryExpr *unaryExpr = (UnaryExpr *)expr;
            Value *value = compileExpression(unaryExpr->right);
            if (unaryExpr->op == NEG_UNARY) {
                if (value->getType()->isIntegerTy() ||
                    value->getType()->isDoubleTy()) {
                    return this->builder->CreateMul(
                        value, this->builder->getInt32(-1));
                }
                printf("Can't do '-' with this type\n");
                exit(1);
            } else if (unaryExpr->op == BANG_UNARY) {
                // Check value type?
                return this->builder->CreateXor(value, 1);
                printf("\nCan't do '!' on smth other then a boolean\n");
                exit(1);
            }

            printf("unknown unary expr?\n");
            exit(1);
        }
        case VAR_EXPR: {
            VarExpr *varExpr = (VarExpr *)expr;
            Value *var = lookupVariable(varExpr->name.lexeme);
            if (var->getType()->isPointerTy()) {
                if (AllocaInst *allocaInst =
                        dyn_cast<AllocaInst>(var)) {
                    if (allocaInst->getType()->isPointerTy()) {
                        Value *loadedValue = this->builder->CreateLoad(
                            allocaInst->getAllocatedType(), allocaInst);
                        return loadedValue;
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
            vector<Value *> arrayItems =
                vector<Value *>(arrayExpr->items.size());
            for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
                arrayItems[i] = compileExpression(arrayExpr->items[i]);
            }

            // Figure ut which type array has
            Type *elementType = nullptr;
            if (arrayExpr->itemType == nullptr) {
                // if (arrayItems[i]->getType() != elementType) {
                // printf("value: ");
                // debugValueType(arrayItems[i]->getType(), this->ctx);
                // printf("elementType: ");
                // debugValueType(elementType, this->ctx);
                // printf("Can't create an array different element then "
                //        "specified\n");
                // exit(1);
                // }
            } else {
                elementType = getVariableLLVMType(arrayExpr->itemType);
            }

            uint64_t arraySize = arrayExpr->items.size();
            uint64_t totalSize =
                arraySize * elementType->getPrimitiveSizeInBits();

            ArrayType *arrayType =
                ArrayType::get(elementType, arraySize);
            GlobalVariable *globalArray = new GlobalVariable(
                *this->module, arrayType, false,
                GlobalValue::ExternalLinkage,
                ConstantAggregateZero::get(arrayType));
            for (uint64_t i = 0; i < arrayExpr->items.size(); ++i) {
                Value *index = ConstantInt::get(
                    Type::getInt32Ty(*this->ctx), i);

                ArrayRef<Value *> indices({index});
                Value *gep =
                    this->builder->CreateGEP(elementType, globalArray, indices);

                this->builder->CreateStore(arrayItems[i], gep);
            }

            return globalArray;
        }
        case MAP_EXPR: {
        }
        case CALL_EXPR: {
            CallExpr *callExpr = (CallExpr *)expr;
            string name = callExpr->callee.lexeme;

            vector<Value *> params =
                vector<Value *>(callExpr->arguments.size());
            for (int i = 0; i < callExpr->arguments.size(); ++i) {
                params[i] = compileExpression(callExpr->arguments[i]);
            }
            Function *func = nullptr;
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
                return this->builder->CreateCall(this->libraryFuncs[name],
                                                 params);
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
            for (Argument &arg : func->args()) {
                if (arg.getType() != params[i]->getType()) {
                    printf("Invalid arg type in function %s with arg %d\n",
                           name.c_str(), i + 1);
                }
                ++i;
            }
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
        case ASSIGN_STMT: {
            AssignStmt *assignStmt = (AssignStmt *)stmt;
            Value *value = compileExpression(assignStmt->value);
            string name = assignStmt->name.lexeme;

            Value *variable = lookupVariable(name);
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

            Value *returnValue = compileExpression(returnStmt->value);
            if (this->function->funcType->getReturnType() !=
                returnValue->getType()) {
                printf("Mismatching return in %s\n",
                       this->function->function->getName().str().c_str());
                exit(1);
            }

            this->builder->CreateRet(returnValue);
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
            Value *value = compileExpression(varStmt->initializer);
            if (!checkVariableValueMatch(varStmt->var, value)) {

                printf("Invalid type mismatch in var declaration\nexpected: ");
                debugVariable(varStmt->var);
                printf("\nbut got: ");
                debugValueType(value->getType(), this->ctx);
                exit(1);
            }
            AllocaInst *var =
                this->builder->CreateAlloca(getVariableLLVMType(varStmt->var),
                                            nullptr, varStmt->var->name.lexeme);
            this->builder->CreateStore(value, var);
            if (this->function->enclosing) {
                this->function->localVariables.push_back(var);
            } else {
                this->globalVariables.push_back(var);
            }
            break;
        }
        case WHILE_STMT: {
            WhileStmt *whileStmt = (WhileStmt *)stmt;

            BasicBlock *loopHeaderBlock = BasicBlock::Create(
                *this->ctx, "loop.header", this->function->function);

            BasicBlock *loopBodyBlock = BasicBlock::Create(
                *this->ctx, "loop.body", this->function->function);

            BasicBlock *loopExitBlock = BasicBlock::Create(
                *this->ctx, "loop.exit", this->function->function);

            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            Value *condition = compileExpression(whileStmt->condition);

            this->builder->CreateCondBr(condition, loopBodyBlock,
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

            BasicBlock *loopHeaderBlock = BasicBlock::Create(
                *this->ctx, "loop.header", this->function->function);

            BasicBlock *loopBodyBlock = BasicBlock::Create(
                *this->ctx, "loop.body", this->function->function);

            BasicBlock *loopExitBlock = BasicBlock::Create(
                *this->ctx, "loop.exit", this->function->function);

            compileStatement(forStmt->initializer);
            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            Value *condition = compileExpression(forStmt->condition);

            this->builder->CreateCondBr(condition, loopBodyBlock,
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
            Value *condition = compileExpression(ifStmt->condition);

            BasicBlock *thenBlock = BasicBlock::Create(
                *ctx, "then", this->function->function);

            BasicBlock *elseBlock = BasicBlock::Create(
                *ctx, "else", this->function->function);

            BasicBlock *mergeBlock = BasicBlock::Create(
                *ctx, "merge", this->function->function);

            this->builder->CreateCondBr(condition, thenBlock, elseBlock);
            this->builder->SetInsertPoint(thenBlock);

            // This is just bad, especially for multiple returns?
            bool returned = false;
            for (int i = 0; i < ifStmt->thenBranch.size(); ++i) {
                compileStatement(ifStmt->thenBranch[i]);
                if (ifStmt->thenBranch[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
            if (!returned) {
                this->builder->CreateBr(mergeBlock);
            }
            this->builder->SetInsertPoint(elseBlock);

            returned = false;
            for (int i = 0; i < ifStmt->elseBranch.size(); ++i) {
                compileStatement(ifStmt->elseBranch[i]);
                if (ifStmt->elseBranch[i]->type == RETURN_STMT) {
                    returned = true;
                    break;
                }
            }
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
            vector<Type *> params =
                vector<Type *>(funcStmt->params.size());
            map<string, int> funcArgs;
            for (int i = 0; i < funcStmt->params.size(); ++i) {
                params[i] = getVariableLLVMType(funcStmt->params[i]);
                funcArgs[funcStmt->params[i]->name.lexeme] = i;
            }

            // Ret type
            Type *returnType = getVariableLLVMType(funcStmt->returnType);

            // Fix func type
            FunctionType *funcType =
                FunctionType::get(returnType, params, false);

            if (this->function->enclosing) {
                printf("Can't declare a function in a function\n");
                exit(1);
            }

            this->function = new LLVMFunction(this->function, funcType,
                                              funcStmt->name.lexeme, funcArgs,
                                              this->ctx, this->module);
            IRBuilder<> *prevBuilder = this->builder;
            this->builder = new IRBuilder<>(this->function->entryBlock);
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
    map<string, FunctionCallee> addLibraryFuncs() {
        map<string, FunctionCallee> libraryFuncs =
            map<string, FunctionCallee>();
        vector<Type *> printfArgs;
        printfArgs.push_back(Type::getInt8PtrTy(*this->ctx));
        FunctionType *printfType = FunctionType::get(
            Type::getInt32Ty(*this->ctx), printfArgs, true);
        FunctionCallee printfFunc =
            this->module->getOrInsertFunction("printf", printfType);

        libraryFuncs["printf"] = printfFunc;

        return libraryFuncs;
    }

  public:
    LLVMCompiler(vector<Stmt *> stmts) {
        this->stmts = stmts;
        this->ctx = new LLVMContext();
        this->module = new Module("Bonobo", *ctx);
        this->callableFunctions = vector<Function *>();
        FunctionType *funcType =
            FunctionType::get(Type::getInt32Ty(*ctx), false);
        this->function = new LLVMFunction(nullptr, funcType, "main", {},
                                          this->ctx, this->module);

        this->libraryFuncs = addLibraryFuncs();
        this->builder = new IRBuilder<>(this->function->entryBlock);
    }
    void compile() {
        for (int i = 0; i < stmts.size(); i++) {
            compileStatement(stmts[i]);
        }
        endCompiler();
    }
};
