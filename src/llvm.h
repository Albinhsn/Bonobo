#include "common.h"
#include "expr.h"
#include "stmt.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <string>

class Function {
  private:
  public:
    Function *enclosing;
    std::vector<llvm::AllocaInst *> localVariables;
    std::map<std::string, int> funcArgs;
    llvm::BasicBlock *entryBlock;
    llvm::Function *function;
    llvm::FunctionType *funcType;

    Function(Function *enclosing, llvm::FunctionType *funcType,
             std::string name, std::map<std::string, int> funcArgs,
             llvm::LLVMContext *ctx, llvm::Module *module) {
        this->enclosing = enclosing;
        this->funcType = funcType;
        this->function = llvm::Function::Create(
            funcType, llvm::Function::ExternalLinkage, name, *module);
        this->funcArgs = funcArgs;
        this->entryBlock =
            llvm::BasicBlock::Create(*ctx, "entry", this->function);
    }
};

class LLVMCompiler {
  private:
    llvm::LLVMContext *ctx;
    llvm::Module *module;
    std::vector<llvm::AllocaInst *> globalVariables;
    std::vector<Stmt *> stmts;
    llvm::IRBuilder<> *builder;
    std::map<std::string, llvm::FunctionCallee> libraryFuncs;
    std::vector<llvm::Function *> callableFunctions;
    Function *function;

    void endCompiler() {
        this->builder->CreateRet(this->builder->getInt32(0));

        std::error_code errorCode;
        llvm::raw_fd_ostream outLL("./out.ll", errorCode);
        this->module->print(outLL, nullptr);
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
        default: {
            printf("unknown llvmType\n");
            exit(1);
        }
        }
    }
    llvm::Value *getVariable(std::string name) {
        if (this->function->enclosing) {
            for (const auto &[key, value] : this->function->funcArgs) {
                if (key == name) {
                    int i = 0;
                    for (llvm::Function::arg_iterator arg =
                             this->function->function->arg_begin();
                         arg != this->function->function->arg_end(); ++arg) {
                        if (i == value) {
                            return arg;
                        }
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

    void debugValueType(llvm::Type *type) {
        if (type == llvm::Type::getInt32Ty(*this->ctx)) {
            printf("int32\n");
        } else if (type == llvm::Type::getInt1Ty(*this->ctx)) {
            printf("int1\n");
        } else if (type == llvm::Type::getDoubleTy(*this->ctx)) {
            printf("double\n");
        } else if (type->isPointerTy()) {
            printf("ptr\n");
        } else if (type->isArrayTy()) {
            printf("array\n");
        }
    }

    bool checkValueMatch(Variable *var, llvm::Value *value) {
        if (value->getType() == llvm::Type::getInt32Ty(*this->ctx)) {
            return var->type == INT_VAR;

        } else if (value->getType() == llvm::Type::getInt1Ty(*this->ctx)) {
            return var->type == BOOL_VAR;

        } else if (value->getType() == llvm::Type::getDoubleTy(*this->ctx)) {
            return var->type == DOUBLE_VAR;
        } else if (value->getType()->isPointerTy()) {
            // ToDo this needs to check underlying type as well
            return var->type == STR_VAR || var->type == ARRAY_VAR ||
                   var->type == MAP_VAR || var->type == STRUCT_VAR;
        }

        printf("unknown value\n");
        return false;
    }
    llvm::Value *compileLiteral(LiteralExpr *expr) {
        std::string stringLiteral = expr->literal.lexeme;
        switch (expr->literalType) {
        case STR_LITERAL: {
            return this->builder->CreateGlobalStringPtr(expr->literal.lexeme);
        }
        case INT_LITERAL: {
            return this->builder->getInt32(std::stoi(stringLiteral));
        }
        case BOOL_LITERAL: {
            if (stringLiteral == "true") {
                return this->builder->getInt1(1);
            } else {
                return this->builder->getInt1(0);
            }
        }
        case DOUBLE_LITERAL: {
            return llvm::ConstantFP::get(this->builder->getDoubleTy(),
                                         std::stod(stringLiteral));
        }
        }
    }
    llvm::Value *compileExpression(Expr *expr) {
        switch (expr->type) {
        case BINARY_EXPR: {
            BinaryExpr *binaryExpr = (BinaryExpr *)expr;
            llvm::Value *left = compileExpression(binaryExpr->left);
            llvm::Value *right = compileExpression(binaryExpr->right);
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
        case GROUPING_EXPR: {
            GroupingExpr *groupingExpr = (GroupingExpr *)expr;
            return compileExpression(groupingExpr->expression);
        }
        case LOGICAL_EXPR: {
            LogicalExpr *logicalExpr = (LogicalExpr *)expr;
            llvm::Value *left = compileExpression(logicalExpr->left);
            llvm::Value *right = compileExpression(logicalExpr->left);
            // Need to type check this?
            switch (logicalExpr->op) {
            case OR_LOGICAL: {
                return this->builder->CreateOr(left, right);
            }
            case AND_LOGICAL: {
                return this->builder->CreateAnd(left, right);
            }
            }
        }
        case LITERAL_EXPR: {
            return compileLiteral((LiteralExpr *)expr);
            break;
        }
        case COMPARISON_EXPR: {
            ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
            llvm::Value *left = compileExpression(comparisonExpr->left);
            llvm::Value *right = compileExpression(comparisonExpr->right);
            // Need to check fp as well, string equality, array equality,
            // map equality
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
        case UNARY_EXPR: {
            UnaryExpr *unaryExpr = (UnaryExpr *)expr;
            llvm::Value *value = compileExpression(unaryExpr->right);
            if (unaryExpr->op == NEG_UNARY) {
                return this->builder->CreateMul(value,
                                                this->builder->getInt32(-1));
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
            llvm::Value *var = getVariable(varExpr->name.lexeme);
            if (var->getType()->isPointerTy()) {
                if (llvm::AllocaInst *allocaInst =
                        llvm::dyn_cast<llvm::AllocaInst>(var)) {
                    if (allocaInst->getType()->isPointerTy()) {
                        llvm::Value *loadedValue = this->builder->CreateLoad(
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

            llvm::Type *elementType = getVariableLLVMType(arrayExpr->itemType);

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
                llvm::Value *value = compileExpression(arrayExpr->items[i]);
                llvm::Value *index = llvm::ConstantInt::get(
                    llvm::Type::getInt32Ty(*this->ctx), i);
                if (value->getType() != elementType) {
                    printf("value: ");
                    debugValueType(value->getType());
                    printf("elementType: ");
                    debugValueType(elementType);
                    printf("Can't create an array different element then "
                           "specified\n");
                    exit(1);
                }

                llvm::ArrayRef<llvm::Value *> indices({index});
                llvm::Value *gep =
                    this->builder->CreateGEP(elementType, globalArray, indices);

                this->builder->CreateStore(value, gep);
            }

            return globalArray;
        }
        case MAP_EXPR: {
        }
        case CALL_EXPR: {
            CallExpr *callExpr = (CallExpr *)expr;
            std::string name = callExpr->callee.lexeme;

            std::vector<llvm::Value *> params =
                std::vector<llvm::Value *>(callExpr->arguments.size());
            for (int i = 0; i < callExpr->arguments.size(); ++i) {
                params[i] = compileExpression(callExpr->arguments[i]);
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
            for (llvm::Argument &arg : func->args()) {
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
            llvm::Value *value = compileExpression(assignStmt->value);
            std::string name = assignStmt->name.lexeme;

            llvm::Value *variable = getVariable(name);
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

            llvm::Value *returnValue = compileExpression(returnStmt->value);
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
            llvm::Value *value = compileExpression(varStmt->initializer);
            if (!checkValueMatch(varStmt->var, value)) {
                printf("Invalid type mismatch in var declaration\n");
                exit(1);
            }
            llvm::AllocaInst *var =
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

            llvm::BasicBlock *loopHeaderBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.header", this->function->function);

            llvm::BasicBlock *loopBodyBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.body", this->function->function);

            llvm::BasicBlock *loopExitBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.exit", this->function->function);

            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            llvm::Value *condition = compileExpression(whileStmt->condition);

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

            llvm::BasicBlock *loopHeaderBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.header", this->function->function);

            llvm::BasicBlock *loopBodyBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.body", this->function->function);

            llvm::BasicBlock *loopExitBlock = llvm::BasicBlock::Create(
                *this->ctx, "loop.exit", this->function->function);

            compileStatement(forStmt->initializer);
            this->builder->CreateBr(loopHeaderBlock);
            this->builder->SetInsertPoint(loopHeaderBlock);

            llvm::Value *condition = compileExpression(forStmt->condition);

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
            llvm::Value *condition = compileExpression(ifStmt->condition);

            llvm::BasicBlock *thenBlock = llvm::BasicBlock::Create(
                *ctx, "then", this->function->function);

            llvm::BasicBlock *elseBlock = llvm::BasicBlock::Create(
                *ctx, "else", this->function->function);

            llvm::BasicBlock *mergeBlock = llvm::BasicBlock::Create(
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

            this->function =
                new Function(this->function, funcType, funcStmt->name.lexeme,
                             funcArgs, this->ctx, this->module);
            llvm::IRBuilder<> *prevBuilder = this->builder;
            this->builder = new llvm::IRBuilder<>(this->function->entryBlock);

            for (int i = 0; i < funcStmt->body.size(); ++i) {
                compileStatement(funcStmt->body[i]);
                if (funcStmt->body[i]->type == RETURN_STMT) {
                    break;
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
        this->function = new Function(nullptr, funcType, "main", {}, this->ctx,
                                      this->module);

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
