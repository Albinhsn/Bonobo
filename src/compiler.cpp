

#include "compiler.h"
#include "chunk.h"
#include "common.h"
#include "memory.h"
#include "object.h"
#include "scanner.h"
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <stdexcept>

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

static void statement(Compiler *compiler, Parser *parser, Scanner *scanner);
static void declaration(Compiler *compiler, Parser *parser, Scanner *scanner);

static void prefixRule(Compiler *compiler, Parser *parser, Scanner *scanner,
                       TokenType type, bool canAssign);
static void infixRule(Compiler *compiler, Parser *parser, Scanner *scanner,
                      TokenType type, bool canAssign);

static Chunk *currentChunk(Compiler *compiler) {
  return compiler->function->chunk;
}

static Compiler *initCompiler(Compiler *current, Parser *parser,
                              FunctionType type) {
  Compiler *compiler = new Compiler(current, newFunction(), type);
  if (type != TYPE_SCRIPT) {
    compiler->function->name =
        new ObjString(createObj(OBJ_STRING), parser->previous->literal);
  }
  return compiler;
}

static void errorAt(Parser *parser, std::string message) {
  if (parser->panicMode) {
    return;
  }
  parser->panicMode = true;

  Token *token = parser->current;
  std::fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    std::fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    //
  } else {
    std::fprintf(stderr, " at '%d'", token->line);
  }

  fprintf(stderr, ": %s\n", message.c_str());
  parser->hadError = true;
}

static void advance(Parser *parser, Scanner *scanner) {
  delete (parser->previous);
  parser->previous = parser->current;
  for (;;) {
    parser->current = scanToken(scanner);
    if (parser->current->type != TOKEN_ERROR) {
      break;
    }
    errorAt(parser, parser->current->literal);
  }
}
static void consume(Parser *parser, Scanner *scanner, TokenType type,
                    std::string message) {
  if (parser->current->type == type) {
    advance(parser, scanner);
    return;
  }

  errorAt(parser, message);
}

static bool match(Parser *parser, Scanner *scanner, TokenType type) {
  if (!(parser->current->type == type)) {
    return false;
  }
  advance(parser, scanner);
  return true;
}

static void emitLoop(Compiler *compiler, Parser *parser, int loopStart) {
  writeChunk(currentChunk(compiler), OP_LOOP, parser->previous->line);

  int offset = currentChunk(compiler)->code.size() - loopStart + 2;
  if (offset > UINT16_MAX) {
    errorAt(parser, "Loop body too large.");
  }

  writeChunks(currentChunk(compiler), (offset >> 8 & 0xff), (offset & 0xff),
              parser->previous->line);
}

static int emitJump(Compiler *compiler, Parser *parser, uint8_t instruction) {
  writeChunk(currentChunk(compiler), instruction, parser->previous->line);
  writeChunks(currentChunk(compiler), 0xff, 0xff, parser->previous->line);

  return currentChunk(compiler)->code.size() - 2;
}

static uint8_t makeConstant(Compiler *compiler, Parser *parser, Value value) {
  int constant = addConstant(currentChunk(compiler), value);

  if (constant > UINT8_MAX) {
    errorAt(parser, "Too many constants in one chunk.");
    return 0;
  }
  return (uint8_t)constant;
}

static ObjFunction *endCompiler(Compiler *compiler, Parser *parser) {
#ifdef DEBUG_PRINT_CODE
  if (!parser->hadError) {
    disassembleChunk(currentChunk(compiler),
                     compiler->function->name != NULL
                         ? compiler->function->name->chars
                         : "<script>");
  }
#endif
  writeChunks(currentChunk(compiler), OP_NIL, OP_RETURN,
              parser->previous->line);
  return compiler->function;
}

static Precedence getPrecedence(TokenType type) {
  switch (type) {
  case TOKEN_LEFT_BRACKET:
    return PREC_CALL;
  case TOKEN_DOT:
    return PREC_CALL;
  case TOKEN_LEFT_PAREN:
    return PREC_CALL;
  case TOKEN_MINUS:
    return PREC_TERM;
  case TOKEN_PLUS:
    return PREC_TERM;
  case TOKEN_SLASH:
    return PREC_FACTOR;
  case TOKEN_STAR:
    return PREC_FACTOR;
  case TOKEN_BANG_EQUAL:
    return PREC_EQUALITY;
  case TOKEN_EQUAL_EQUAL:
    return PREC_EQUALITY;
  case TOKEN_GREATER:
    return PREC_COMPARISON;
  case TOKEN_GREATER_EQUAL:
    return PREC_COMPARISON;
  case TOKEN_LESS:
    return PREC_COMPARISON;
  case TOKEN_LESS_EQUAL:
    return PREC_COMPARISON;
  case TOKEN_AND:
    return PREC_AND;
  case TOKEN_OR:
    return PREC_OR;
  default:
    return PREC_NONE;
  }
}

static void parsePrecedence(Compiler *compiler, Parser *parser,
                            Scanner *scanner, Precedence precedence) {
  advance(parser, scanner);
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(compiler, parser, scanner, parser->previous->type, canAssign);
  while (precedence <= getPrecedence(parser->current->type)) {
    advance(parser, scanner);
    infixRule(compiler, parser, scanner, parser->previous->type, canAssign);
  }
  if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
    errorAt(parser, "Invalid assignment target.");
  }
}

static uint8_t identifierConstant(Compiler *compiler, Parser *parser) {
  return makeConstant(compiler, parser,
                      OBJ_VAL(copyString(parser->previous->literal)));
}

static bool identifiersEqual(Token *a, Token *b) {
  return a->literal == b->literal;
}

static int resolveLocal(Compiler *compiler, Parser *parser) {
  for (int i = compiler->locals.size() - 1; i >= 0; i--) {
    Local local = compiler->locals[i];
    if (identifiersEqual(&local.name, parser->previous)) {
      if (local.depth == -1) {
        errorAt(parser, "Can't read local variable in its own initializer.");
      }
      return i;
    }
  }
  return -1;
}

static void declareVariable(Compiler *compiler, Parser *parser) {
  if (compiler->scopeDepth == 0) {
    return;
  }

  Token *name = parser->previous;
  for (int i = compiler->locals.size() - 1; i >= 0; i--) {
    Local local = compiler->locals[i];
    if (local.depth != -1 && local.depth < compiler->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local.name)) {
      errorAt(parser, "Already a variable with this name in this scope.");
    }
  }
  compiler->locals.push_back(Local(*name, -1));
}

static uint8_t parseVariable(Compiler *compiler, Parser *parser,
                             Scanner *scanner, const char *errorMessage) {
  consume(parser, scanner, TOKEN_IDENTIFIER, errorMessage);

  declareVariable(compiler, parser);
  if (compiler->scopeDepth > 0) {
    return 0;
  }

  return identifierConstant(compiler, parser);
}

static void patchJump(Compiler *compiler, Parser *parser, int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.

  int jump = currentChunk(compiler)->code.size() - offset - 2;
  if (jump > UINT16_MAX) {
    errorAt(parser, "Too much code to jump over.");
  }
  currentChunk(compiler)->code[offset] = (jump >> 8) & 0xff;
  currentChunk(compiler)->code[offset + 1] = jump & 0xff;
}
static void expression(Compiler *compiler, Parser *parser, Scanner *scanner) {
  parsePrecedence(compiler, parser, scanner, PREC_ASSIGNMENT);
}

static void markInitialized(Compiler *compiler) {
  if (compiler->scopeDepth == 0) {
    return;
  }
  compiler->locals[compiler->locals.size() - 1].depth = compiler->scopeDepth;
}

static void defineVariable(Compiler *compiler, Parser *parser, uint8_t global) {
  if (compiler->scopeDepth > 0) {
    markInitialized(compiler);
    return;
  }

  writeChunks(currentChunk(compiler), OP_DEFINE_GLOBAL, global,
              parser->previous->line);
}

static uint8_t argumentList(Compiler *compiler, Parser *parser,
                            Scanner *scanner) {
  uint8_t argCount = 0;
  if (!(parser->current->type == TOKEN_RIGHT_PAREN)) {
    do {
      expression(compiler, parser, scanner);
      if (argCount == 255) {
        errorAt(parser, "Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(parser, scanner, TOKEN_COMMA));
  }
  consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(Compiler *compiler, Parser *parser, Scanner *scanner,
                 bool canAssign) {
  int endJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);

  writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
  parsePrecedence(compiler, parser, scanner, PREC_AND);

  patchJump(compiler, parser, endJump);
}

static void or_(Compiler *compiler, Parser *parser, Scanner *scanner,
                bool canAssign) {
  int elseJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
  int endJump = emitJump(compiler, parser, OP_JUMP);

  patchJump(compiler, parser, elseJump);
  writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);

  parsePrecedence(compiler, parser, scanner, PREC_OR);
  patchJump(compiler, parser, endJump);
}

static void arrayDeclaration(Compiler *compiler, Parser *parser,
                             Scanner *scanner) {
  uint8_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACKET) {
    do {
      expression(compiler, parser, scanner);
      if (items == 255) {
        errorAt(parser, "Can't have more than 255 arguments.");
      }
      items++;
    } while (match(parser, scanner, TOKEN_COMMA));
  }
  consume(parser, scanner, TOKEN_RIGHT_BRACKET, "Expect ')' after arguments.");
  writeChunks(currentChunk(compiler), OP_ARRAY, items, parser->previous->line);
}

static void mapDeclaration(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  uint8_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACE) {

    do {
      consume(parser, scanner, TOKEN_STRING, "Expect strings as keys");
      uint8_t constant = identifierConstant(compiler, parser);
      writeChunks(currentChunk(compiler), OP_CONSTANT, constant,
                  parser->previous->line);

      consume(parser, scanner, TOKEN_COLON,
              "Expect colon between key and value");
      expression(compiler, parser, scanner);

      if (items == 255) {
        errorAt(parser, "Can't have more than 255 arguments.");
      }
      items += 2;

    } while (match(parser, scanner, TOKEN_COMMA));
  }

  consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after map items.");
  writeChunks(currentChunk(compiler), OP_MAP, items, parser->previous->line);
}

static void varDeclaration(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  uint8_t global =
      parseVariable(compiler, parser, scanner, "Expect variable name.");

  if (match(parser, scanner, TOKEN_EQUAL)) {
    if (match(parser, scanner, TOKEN_LEFT_BRACKET)) {
      arrayDeclaration(compiler, parser, scanner);
    } else if (match(parser, scanner, TOKEN_LEFT_BRACE)) {
      mapDeclaration(compiler, parser, scanner);
    } else {
      expression(compiler, parser, scanner);
    }
  } else {
    writeChunk(currentChunk(compiler), OP_NIL, parser->previous->line);
  }
  consume(parser, scanner, TOKEN_SEMICOLON,
          "Expect ';' after variable declaration");

  defineVariable(compiler, parser, global);
}

static void expressionStatement(Compiler *compiler, Parser *parser,
                                Scanner *scanner) {
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after expression.");
  writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
}

static void beginScope(Compiler *compiler) { compiler->scopeDepth++; }
static void endScope(Compiler *compiler, Parser *parser) {
  compiler->scopeDepth--;
  while (compiler->locals.size() > 0 &&
         compiler->locals[compiler->locals.size() - 1].depth >
             compiler->scopeDepth) {
    writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
    compiler->locals.pop_back();
  }
}

static void forStatement(Compiler *compiler, Parser *parser, Scanner *scanner) {
  beginScope(compiler);
  consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(parser, scanner, TOKEN_SEMICOLON)) {
    //
  } else if (match(parser, scanner, TOKEN_VAR)) {
    varDeclaration(compiler, parser, scanner);
  } else {
    expressionStatement(compiler, parser, scanner);
  }

  int loopStart = currentChunk(compiler)->code.size();
  int exitJump = -1;
  if (!match(parser, scanner, TOKEN_SEMICOLON)) {
    expression(compiler, parser, scanner);
    consume(parser, scanner, TOKEN_SEMICOLON,
            "Expect ';' after loop condition");

    exitJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
    writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
  }

  if (!match(parser, scanner, TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(compiler, parser, OP_JUMP);
    int incrementStart = currentChunk(compiler)->code.size();

    expression(compiler, parser, scanner);
    writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
    consume(parser, scanner, TOKEN_RIGHT_PAREN,
            "Expect ')' after for clauses.");

    emitLoop(compiler, parser, loopStart);
    loopStart = incrementStart;
    patchJump(compiler, parser, bodyJump);
  }

  statement(compiler, parser, scanner);
  emitLoop(compiler, parser, loopStart);

  if (exitJump != -1) {
    patchJump(compiler, parser, exitJump);
    writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
  }

  endScope(compiler, parser);
}

static void ifStatement(Compiler *compiler, Parser *parser, Scanner *scanner) {
  consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
  writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
  statement(compiler, parser, scanner);

  int elseJump = emitJump(compiler, parser, OP_JUMP);

  patchJump(compiler, parser, thenJump);
  writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);

  if (match(parser, scanner, TOKEN_ELSE)) {
    statement(compiler, parser, scanner);
  }
  patchJump(compiler, parser, elseJump);
}

static void printStatement(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after value.");
  writeChunk(currentChunk(compiler), OP_PRINT, parser->previous->line);
}

static void returnStatement(Compiler *compiler, Parser *parser,
                            Scanner *scanner) {
  if (compiler->type == TYPE_SCRIPT) {
    errorAt(parser, "Can't return from top-level code");
  }

  if (match(parser, scanner, TOKEN_SEMICOLON)) {
    writeChunks(currentChunk(compiler), OP_NIL, OP_RETURN,
                parser->previous->line);
  } else {
    expression(compiler, parser, scanner);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after return value");
    writeChunk(currentChunk(compiler), OP_RETURN, parser->previous->line);
  }
}

static void whileStatement(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  int loopStart = currentChunk(compiler)->code.size();
  consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
  writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
  statement(compiler, parser, scanner);
  emitLoop(compiler, parser, loopStart);

  patchJump(compiler, parser, exitJump);
  writeChunk(currentChunk(compiler), OP_POP, parser->previous->line);
}

static void synchronize(Parser *parser, Scanner *scanner) {
  parser->panicMode = false;

  while (parser->current->type != TOKEN_EOF) {
    if (parser->previous->type == TOKEN_SEMICOLON) {
      return;
    }
    switch (parser->current->type) {
    case TOKEN_STRUCT:
    case TOKEN_FUN:
    case TOKEN_VAR:
    case TOKEN_FOR:
    case TOKEN_IF:
    case TOKEN_WHILE:
    case TOKEN_PRINT:
    case TOKEN_RETURN:
      return;
    default: {
      // Do nothing.
    }
    }

    advance(parser, scanner);
  }
}

static void binary(Compiler *compiler, Parser *parser, Scanner *scanner) {
  TokenType operatorType = parser->previous->type;

  parsePrecedence(compiler, parser, scanner,
                  (Precedence)(getPrecedence(operatorType) + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL: {
    writeChunks(currentChunk(compiler), OP_EQUAL, OP_NOT,
                parser->previous->line);
    break;
  }
  case TOKEN_EQUAL_EQUAL: {
    writeChunk(currentChunk(compiler), OP_EQUAL, parser->previous->line);
    break;
  }
  case TOKEN_GREATER: {
    writeChunk(currentChunk(compiler), OP_GREATER, parser->previous->line);
    break;
  }
  case TOKEN_GREATER_EQUAL: {
    writeChunks(currentChunk(compiler), OP_LESS, OP_NOT,
                parser->previous->line);
    break;
  }
  case TOKEN_LESS: {
    writeChunk(currentChunk(compiler), OP_LESS, parser->previous->line);
    break;
  }
  case TOKEN_LESS_EQUAL: {
    writeChunks(currentChunk(compiler), OP_GREATER, OP_NOT,
                parser->previous->line);
    break;
  }
  case TOKEN_PLUS: {
    writeChunk(currentChunk(compiler), OP_ADD, parser->previous->line);
    break;
  }
  case TOKEN_MINUS: {
    writeChunk(currentChunk(compiler), OP_SUBTRACT, parser->previous->line);
    break;
  }
  case TOKEN_STAR: {
    writeChunk(currentChunk(compiler), OP_MULTIPLY, parser->previous->line);
    break;
  }
  case TOKEN_SLASH: {
    writeChunk(currentChunk(compiler), OP_DIVIDE, parser->previous->line);
    break;
  }
  default: {
    return;
  }
  }
}

static void index(Compiler *compiler, Parser *parser, Scanner *scanner) {
  expression(compiler, parser, scanner);
  writeChunk(currentChunk(compiler), OP_INDEX, parser->previous->line);
  consume(parser, scanner, TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
}

static void dot(Compiler *compiler, Parser *parser, Scanner *scanner,
                bool canAssign) {
  consume(parser, scanner, TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = identifierConstant(compiler, parser);

  if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
    expression(compiler, parser, scanner);
    writeChunks(currentChunk(compiler), OP_SET_PROPERTY, name,
                parser->previous->line);
  } else {
    writeChunks(currentChunk(compiler), OP_GET_PROPERTY, name,
                parser->previous->line);
  }
}

static void grouping(Compiler *compiler, Parser *parser, Scanner *scanner) {
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary(Compiler *compiler, Parser *parser, Scanner *scanner) {
  TokenType operatorType = parser->previous->type;
  parsePrecedence(compiler, parser, scanner, PREC_UNARY);

  switch (operatorType) {
  case TOKEN_MINUS: {
    writeChunk(currentChunk(compiler), OP_NEGATE, parser->previous->line);
    break;
  }
  case TOKEN_BANG: {
    writeChunk(currentChunk(compiler), OP_NOT, parser->previous->line);
    break;
  }
  default: {
    return;
  }
  }
}

static void number(Compiler *compiler, Parser *parser, Scanner *scanner) {
  double value = std::stod(parser->previous->literal);
  writeChunks(currentChunk(compiler), OP_CONSTANT,
              makeConstant(compiler, parser, NUMBER_VAL(value)),
              parser->previous->line);
}

static void namedVariable(Compiler *compiler, Parser *parser, Scanner *scanner,
                          bool canAssign) {

  uint8_t getOp, setOp;
  int arg = resolveLocal(compiler, parser);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = identifierConstant(compiler, parser);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
    expression(compiler, parser, scanner);
    writeChunks(currentChunk(compiler), setOp, (uint8_t)arg,
                parser->previous->line);
  } else {
    writeChunks(currentChunk(compiler), getOp, (uint8_t)arg,
                parser->previous->line);
  }
}

static void literal(Compiler *compiler, Parser *parser, Scanner *scanner) {
  switch (parser->previous->type) {
  case TOKEN_FALSE: {
    writeChunk(currentChunk(compiler), OP_FALSE, parser->previous->line);
    break;
  }
  case TOKEN_NIL: {
    writeChunk(currentChunk(compiler), OP_NIL, parser->previous->line);
    break;
  }
  case TOKEN_TRUE: {
    writeChunk(currentChunk(compiler), OP_TRUE, parser->previous->line);
    break;
  }
  }
}

static void prefixRule(Compiler *compiler, Parser *parser, Scanner *scanner,
                       TokenType type, bool canAssign) {
  switch (type) {
  case TOKEN_LEFT_PAREN: {
    grouping(compiler, parser, scanner);
    break;
  }
  case TOKEN_MINUS: {
    unary(compiler, parser, scanner);
    break;
  }
  case TOKEN_STRING: {
    writeChunks(currentChunk(compiler), OP_CONSTANT,
                makeConstant(compiler, parser,
                             OBJ_VAL(copyString(parser->previous->literal))),
                parser->previous->line);
    break;
  }
  case TOKEN_NUMBER: {
    number(compiler, parser, scanner);
    break;
  }
  case TOKEN_FALSE: {
    literal(compiler, parser, scanner);
    break;
  }
  case TOKEN_TRUE: {
    literal(compiler, parser, scanner);
    break;
  }
  case TOKEN_NIL: {
    literal(compiler, parser, scanner);
    break;
  }
  case TOKEN_BANG: {
    unary(compiler, parser, scanner);
    break;
  }
  case TOKEN_IDENTIFIER: {
    namedVariable(compiler, parser, scanner, canAssign);
    break;
  }
  default: {
    break;
  }
  }
}
static void infixRule(Compiler *compiler, Parser *parser, Scanner *scanner,
                      TokenType type, bool canAssign) {
  switch (type) {
  case TOKEN_LEFT_BRACKET: {
    index(compiler, parser, scanner);
    break;
  }
  case TOKEN_LEFT_PAREN: {
    writeChunks(currentChunk(compiler), OP_CALL,
                argumentList(compiler, parser, scanner),
                parser->previous->line);
    break;
  }
  case TOKEN_DOT: {
    dot(compiler, parser, scanner, canAssign);
    break;
  }
  case TOKEN_MINUS: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_PLUS: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_STAR: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_SLASH: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_BANG_EQUAL: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_EQUAL_EQUAL: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_GREATER: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_GREATER_EQUAL: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_LESS: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_LESS_EQUAL: {
    binary(compiler, parser, scanner);
    break;
  }
  case TOKEN_AND: {
    and_(compiler, parser, scanner, canAssign);
  }
  case TOKEN_OR: {
    or_(compiler, parser, scanner, canAssign);
  }
  default: {
    break;
  }
  }
}

static void block(Compiler *compiler, Parser *parser, Scanner *scanner) {
  while (parser->current->type != TOKEN_RIGHT_BRACE &&
         parser->current->type != TOKEN_EOF) {
    declaration(compiler, parser, scanner);
  }

  consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(Compiler *current, Parser *parser, Scanner *scanner,
                     FunctionType type) {
  Compiler *compiler = initCompiler(current, parser, type);
  beginScope(compiler);
  consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (parser->current->type != TOKEN_RIGHT_PAREN) {
    do {
      compiler->function->arity++;
      if (compiler->function->arity > 255) {
        errorAt(parser, "Can't at have more than 255 parameters.");
      }
      uint8_t constant =
          parseVariable(compiler, parser, scanner, "Expect parameter name.");
      defineVariable(compiler, parser, constant);
    } while (match(parser, scanner, TOKEN_COMMA));
  }
  consume(parser, scanner, TOKEN_RIGHT_PAREN,
          "Expect ')' after last function param.");
  consume(parser, scanner, TOKEN_LEFT_BRACE,
          "Expect '{' after function params.");
  block(compiler, parser, scanner);

  ObjFunction *function = endCompiler(compiler, parser);
  freeCompiler(compiler);
  writeChunks(currentChunk(current), OP_CONSTANT,
              makeConstant(current, parser, OBJ_VAL(function)),
              parser->previous->line);
}

static void structArgs(Compiler *compiler, Parser *parser, Scanner *scanner) {
  while (parser->current->type != TOKEN_RIGHT_BRACE) {
    consume(parser, scanner, TOKEN_IDENTIFIER,
            "Expect field identifier in struct");
    writeChunks(currentChunk(compiler), OP_STRUCT_ARG,
                makeConstant(compiler, parser,
                             OBJ_VAL(copyString(parser->previous->literal))),
                parser->previous->line);
    consume(parser, scanner, TOKEN_SEMICOLON,
            "Expect semicolon after struct field identifier");
  }
}

static void structDeclaration(Compiler *compiler, Parser *parser,
                              Scanner *scanner) {
  consume(parser, scanner, TOKEN_IDENTIFIER, "Expect struct name");
  uint8_t nameConstant = identifierConstant(compiler, parser);
  declareVariable(compiler, parser);

  writeChunks(currentChunk(compiler), OP_STRUCT, nameConstant,
              parser->previous->line);
  defineVariable(compiler, parser, nameConstant);

  consume(parser, scanner, TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  structArgs(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
}

static void funDeclaration(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  uint8_t global =
      parseVariable(compiler, parser, scanner, "Expect function name");
  markInitialized(compiler);
  function(compiler, parser, scanner, TYPE_FUNCTION);
  defineVariable(compiler, parser, global);
}

static void statement(Compiler *compiler, Parser *parser, Scanner *scanner) {
  if (match(parser, scanner, TOKEN_PRINT)) {
    printStatement(compiler, parser, scanner);
  } else if (match(parser, scanner, TOKEN_FOR)) {
    forStatement(compiler, parser, scanner);
  } else if (match(parser, scanner, TOKEN_IF)) {
    ifStatement(compiler, parser, scanner);
  } else if (match(parser, scanner, TOKEN_RETURN)) {
    returnStatement(compiler, parser, scanner);
  } else if (match(parser, scanner, TOKEN_WHILE)) {
    whileStatement(compiler, parser, scanner);
  } else if (match(parser, scanner, TOKEN_LEFT_BRACE)) {
    block(compiler, parser, scanner);
  } else {
    expressionStatement(compiler, parser, scanner);
  }
}

static void declaration(Compiler *compiler, Parser *parser, Scanner *scanner) {
  if (match(parser, scanner, TOKEN_FUN)) {
    funDeclaration(compiler, parser, scanner);
  } else if (match(parser, scanner, TOKEN_VAR)) {
    varDeclaration(compiler, parser, scanner);
  } else if (match(parser, scanner, TOKEN_STRUCT)) {
    structDeclaration(compiler, parser, scanner);
  } else {
    statement(compiler, parser, scanner);
  }

  if (parser->panicMode) {
    synchronize(parser, scanner);
  }
}

Compiler *compile(std::string source) {
  Scanner *scanner = new Scanner(source);
  Parser *parser = new Parser;
  Compiler *compiler = initCompiler(NULL, parser, TYPE_SCRIPT);
  advance(parser, scanner);
  while (!match(parser, scanner, TOKEN_EOF)) {
    declaration(compiler, parser, scanner);
  }
  bool hadError = parser->hadError;
  endCompiler(compiler, parser);

  freeScanner(scanner);
  freeParser(parser);

  if (hadError) {
    freeCompiler(compiler);
  }

  return hadError ? NULL : compiler;
}
