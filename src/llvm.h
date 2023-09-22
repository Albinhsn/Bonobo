#include "common.h"
#include "stmt.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <llvm/Support/raw_ostream.h>

class LLVMCompiler {
  private:
    llvm::LLVMContext *ctx;
    llvm::Module *module;
    llvm::IRBuilder<> *builder;
    llvm::BasicBlock *entryBlock;
    llvm::Function *mainFunction;
    std::vector<llvm::AllocaInst *> globalVariables;
    std::vector<Stmt *> stmts;

    void endCompiler() {
        this->builder->CreateRet(this->builder->getInt32(0));

        std::error_code errorCode;
        llvm::raw_fd_ostream outLL("./out.ll", errorCode);
        this->module->print(outLL, nullptr);
    }
    llvm::Type *getLLVMType(Variable *var) {
        switch (var->type) {
        case INT_VAR: {
            return builder->getInt32Ty();
        }
        case STR_VAR: {
            for (int i = 0; i < this->globalVariables.size(); i++) {
                if (this->globalVariables[i]->getName() ==
                    std::string(var->name.lexeme, var->name.length)) {
                    return llvm::ArrayType::get(
                        llvm::IntegerType::get(*this->ctx, 8),
                        this->globalVariables[i]->getNumOperands());
                }
            }
        }
        case DOUBLE_VAR: {
            return builder->getDoubleTy();
        }
        case BOOL_VAR: {
            return builder->getInt32Ty();
        }
        default: {
            printf("unknown llvmType");
            exit(1);
        }
        }
    }
    bool checkValueMatch(Variable *var, llvm::Value *value) {
        printf("checking %.*s ", var->name.length, var->name.lexeme);
        if (value->getType() == llvm::Type::getInt32Ty(*this->ctx)) {
            printf("value is int, returning %d\n",
                   var->type == INT_VAR || var->type == BOOL_VAR);
            return var->type == INT_VAR || var->type == BOOL_VAR;
        } else if (value->getType() == llvm::Type::getDoubleTy(*this->ctx)) {
            printf("value is double, returning %d\n", var->type == DOUBLE_VAR);
            return var->type == DOUBLE_VAR;
        } else if (llvm::isa<llvm::ConstantDataArray>(value)) {
            printf("value is string, returning %d\n", var->type == STR_VAR);
            return var->type == STR_VAR;
        }

        printf("unknown value\n");
        return false;
    }
    llvm::Value *compileLiteral(LiteralExpr *expr) {
        std::string stringLiteral(expr->literal.lexeme, expr->literal.length);
        switch (expr->literalType) {
        case STR_LITERAL: {
            return llvm::ConstantDataArray::getString(*this->ctx,
                                                      stringLiteral);
        }
        case INT_LITERAL: {
            return builder->getInt32(std::stoi(stringLiteral));
        }
        case BOOL_LITERAL: {
            if (stringLiteral == "true") {
                return builder->getInt32(1);
            } else {
                return builder->getInt32(0);
            }
        }
        case DOUBLE_LITERAL: {
            return llvm::ConstantFP::get(builder->getDoubleTy(),
                                         std::stod(stringLiteral));
        }
        }
    }
    llvm::Value *compileExpression(Expr *expr) {
        switch (expr->type) {
        case BINARY_EXPR: {
        }
        case GROUPING_EXPR: {
        }
        case LOGICAL_EXPR: {
        }
        case LITERAL_EXPR: {
            return compileLiteral((LiteralExpr *)expr);
            break;
        }
        case COMPARISON_EXPR: {
        }
        case UNARY_EXPR: {
        }
        case VAR_EXPR: {
        }
        case INDEX_EXPR: {
        }
        case ARRAY_EXPR: {
        }
        case MAP_EXPR: {
        }
        case CALL_EXPR: {
        }
        default: {
            printf("unknown expr\n");
            exit(1);
        }
        }
    }
    void errorInvalidType(Variable *var, llvm::Value *value) {
        printf("invalid type match\n");
        exit(1);
    }
    void compileStatement(Stmt *stmt) {
        switch (stmt->type) {
        case EXPR_STMT: {
            ExprStmt *exprStmt = (ExprStmt *)stmt;
            compileExpression(exprStmt->expression);
            break;
        }
        case ASSIGN_STMT: {
        }
        case RETURN_STMT: {
        }
        case VAR_STMT: {
            VarStmt *varStmt = (VarStmt *)stmt;
            llvm::Value *value = compileExpression(varStmt->initializer);
            if (!checkValueMatch(varStmt->var, value)) {
                errorInvalidType(varStmt->var, value);
            }
            llvm::AllocaInst *var = this->builder->CreateAlloca(
                getLLVMType(varStmt->var), value,
                std::string(varStmt->var->name.lexeme,
                            varStmt->var->name.length));
            this->globalVariables.push_back(var);
            break;
        }
        case WHILE_STMT: {
        }
        case FOR_STMT: {
        }
        case STRUCT_STMT: {
        }
        case IF_STMT: {
        }
        case FUNC_STMT: {
        }
        }
    }

  public:
    LLVMCompiler(std::vector<Stmt *> stmts) {
        this->stmts = stmts;
        this->ctx = new llvm::LLVMContext();
        this->module = new llvm::Module("Bonobo", *ctx);

        llvm::FunctionType *funcType =
            llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx), false);
        this->mainFunction = llvm::Function::Create(
            funcType, llvm::Function::ExternalLinkage, "main", *module);
        this->entryBlock =
            llvm::BasicBlock::Create(*ctx, "entry", this->mainFunction);

        this->builder = new llvm::IRBuilder<>(entryBlock);
    }
    void compile() {
        for (int i = 0; i < stmts.size(); i++) {
            compileStatement(stmts[i]);
        }
        endCompiler();
    }
};
