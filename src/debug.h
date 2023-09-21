
#ifndef DEBUG_HEADER
#define DEBUG_HEADER

#include "compiler.h"

void debugStatements(std::vector<Stmt*> statements);
void debugExpression(Expr *expr);
void debugToken(Token *token);

#endif
