
#ifndef DEBUG_HEADER
#define DEBUG_HEADER

#include "compiler.h"

void debugStatements(std::vector<Stmt*> statements);
void debugStatement(Stmt* statement);
void debugExpression(Expr *expr);
void debugToken(Token *token);

#endif
