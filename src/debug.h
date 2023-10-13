
#ifndef DEBUG_HEADER
#define DEBUG_HEADER

#include "compiler.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/LLVMContext.h"

std::string debugValueType(llvm::Type *type, llvm::LLVMContext *ctx);
void debugStatements(std::vector<Stmt *> statements);
void debugStatement(Stmt *statement);
void debugExpression(Expr *expr);
void debugVariable(Variable *var);
void debugToken(Token *token);

#endif
