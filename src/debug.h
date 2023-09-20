
#ifndef DEBUG_HEADER
#define DEBUG_HEADER

#include "compiler.h"

void debugStatements(Compiler *compiler);
void debugExpression(Expr *expr);
void debugToken(Token * token);

#endif
