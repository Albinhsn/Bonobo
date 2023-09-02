
// static void emitByte(Compiler *compiler, Parser *parser, uint8_t byte) {
//   writeChunk(currentChunk(compiler), byte, parser->previous->line);
// }
//
// static void emitBytes(Compiler *compiler, Parser *parser, uint8_t byte1,
//                       uint8_t byte2) {
//   emitByte(compiler, parser, byte1);
//   emitByte(compiler, parser, byte2);
// }

#ifndef cpplox_compiler_h
#define cpplox_compiler_h

#include "common.h"
#include "object.h"
#include "scanner.h"
#include "vm.h"

typedef struct Parser {
  Token *current;
  bool hadError;
  bool panicMode;
  Token *previous;
  Parser() : current(NULL), hadError(false), panicMode(false), previous(NULL){};
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

typedef void (*ParseFn)(Parser *parser, Scanner *scanner);

typedef struct ParseRule {
  ParseFn *prefix;
  ParseFn *infix;
  Precedence precedence;
  ParseRule(ParseFn *p, ParseFn *i, Precedence pr)
      : prefix(p), infix(i), precedence(pr){};
} ParseRule;

typedef struct Local {
  Token name;
  int depth;
  Local(Token n, int d) : name(n), depth(d){};
} Local;

typedef enum { TYPE_FUNCTION, TYPE_SCRIPT } FunctionType;

typedef struct Compiler {
  struct Compiler *enclosing;
  ObjFunction *function;
  FunctionType type;
  std::vector<Local> locals;
  int scopeDepth;
  Compiler(Compiler *e, ObjFunction *f, FunctionType t)
      : enclosing(e), function(f), type(t), locals(std::vector<Local>()),
        scopeDepth(0){};
} Compiler;

Compiler *compile(std::string source);

static void statement(Compiler *compiler, Parser *parser, Scanner *scanner);
static void declaration(Compiler *compiler, Parser *parser, Scanner *scanner);

static void prefixRule(Compiler *compiler, Parser *parser, Scanner *scanner,
                       TokenType type, bool canAssign);
static void infixRule(Compiler *compiler, Parser *parser, Scanner *scanner,
                      TokenType type, bool canAssign);

#endif
