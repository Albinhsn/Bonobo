#ifndef cpplox_compiler_h
#define cpplox_compiler_h

#include "common.h"
#include "expr.h"
#include "scanner.h"
#include "stmt.h"

typedef struct Parser {
    Token *current;
    bool hadError;
    Token *previous;
    Parser() : current(nullptr), hadError(false), previous(nullptr){};
} Parser;


typedef struct Compiler {
    Compiler *enclosing;
    std::vector<std::string> s;
    std::vector<Stmt *> statements;
} Compiler;

std::vector<Stmt *> compile(std::string source);

static Expr *mapDeclaration();
static Expr *arrayDeclaration();
static Expr *expression(Expr *expr);
static Stmt*statement();
static Stmt*declaration();

#endif
