#ifndef cpplox_compiler_h
#define cpplox_compiler_h

#include "expr.h"
#include "map"
#include "scanner.h"
#include "stmt.h"

typedef struct Parser {
    Token *current;
    Token *previous;
    Parser() : current(nullptr), previous(nullptr){};
} Parser;

typedef struct Compiler {
    Compiler *enclosing;
    std::vector<Stmt *> statements;
    std::map<std::string, Variable *> variables;
} Compiler;

Compiler *compile(std::string source);

static Expr *mapDeclaration();
static Expr *arrayDeclaration();
static Expr *expression(Expr *expr);
static Stmt *statement();
static Stmt *declaration();

#endif
