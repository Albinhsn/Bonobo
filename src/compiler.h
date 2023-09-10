#ifndef cpplox_compiler_h
#define cpplox_compiler_h

#include "common.h"
#include "object.h"
#include "scanner.h"

typedef struct Parser {
  Token *current;
  bool hadError;
  Token *previous;
  Parser() : current(NULL), hadError(false), previous(NULL){};
} Parser;

typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT, // =
  PREC_OR,         // or
  PREC_AND,        // and
  PREC_EQUALITY,   // == !=
  PREC_COMPARISON, // < > <= >=
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_UNARY,      // ! -
  PREC_CALL,       // . (), []
  PREC_PRIMARY
} Precedence;

typedef struct Local {
  Token name;
  int depth;
} Local;

typedef enum { TYPE_FUNCTION, TYPE_SCRIPT } FunctionType;

typedef struct Compiler {
  struct Compiler *enclosing;
  ObjFunction *function;
  FunctionType type;
  Local *locals;
  int localCap;
  int localLen;
  int scopeDepth;
} Compiler;

Compiler *compile(const char *);

static void statement();
static void declaration();
void markCompilerRoots();
static void prefixRule(TokenType type, bool canAssign);
static void infixRule(TokenType type, bool canAssign);

#endif
