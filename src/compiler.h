#ifndef cpplox_compiler_h
#define cpplox_compiler_h

#include "scanner.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <llvm/Support/raw_ostream.h>
#include <stdio.h>
#include <string.h>
#include <string>

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

typedef enum { TYPE_FUNCTION, TYPE_SCRIPT } FunctionType;

typedef struct Compiler {
  Compiler *enclosing;
  FunctionType type;
  llvm::IRBuilder<> *builder;
  llvm::Module *module;
  llvm::LLVMContext *ctx;
  std::vector<llvm::GlobalVariable> stringConstants;
} Compiler;

void compile(const char *);

static void statement();
static void declaration();
static void prefixRule(TokenType type, bool canAssign);
static void infixRule(TokenType type, bool canAssign);

#endif
