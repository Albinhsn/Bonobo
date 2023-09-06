
#include "compiler.h"
#include "common.h"
#include "memory.h"
#include "object.h"
#include "opcode.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

static Compiler *initCompiler(Compiler *current, Parser *parser,
                              FunctionType type) {
  Compiler *compiler = new Compiler(current, newFunction(), type);
  if (type != TYPE_SCRIPT) {
    compiler->function->name = new ObjString(
        createObj(OBJ_STRING), newString(parser->previous->string.literal,
                                         parser->previous->string.length));
  }
  return compiler;
}

static void errorAt(Parser *parser, const char *message) {
  if (parser->hadError) {
    return;
  }

  Token *token = parser->current;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    //
  } else {
    fprintf(stderr, " at '%d'", token->line);
  }

  fprintf(stderr, ": %s\n", message);
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
    errorAt(parser, "error advancing");
  }
}
static void consume(Parser *parser, Scanner *scanner, TokenType type,
                    const char *message) {
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
  writeChunk(compiler->function, OP_LOOP, parser->previous->line);

  int offset = compiler->function->cp - loopStart + 2;
  if (offset > UINT16_MAX) {
    errorAt(parser, "Loop body too large.");
  }

  writeChunks(compiler->function, (offset >> 8 & 0xff), (offset & 0xff),
              parser->previous->line);
}

static int emitJump(Compiler *compiler, Parser *parser, uint8_t instruction) {
  writeChunk(compiler->function, instruction, parser->previous->line);
  writeChunks(compiler->function, 0xff, 0xff, parser->previous->line);

  return compiler->function->cp - 2;
}

static uint8_t makeConstant(Compiler *compiler, Parser *parser, Value value) {
  int constant = addConstant(compiler->function, value);

  if (constant > UINT8_MAX) {
    errorAt(parser, "Too many constants in one chunk.");
    return 0;
  }
  return (uint8_t)constant;
}

static inline uint8_t stringConstant(Compiler *compiler, Parser *parser) {
  return makeConstant(compiler, parser,
                      OBJ_VAL(copyString(parser->previous->string)));
}

static ObjFunction *endCompiler(Compiler *compiler, Parser *parser) {
#ifdef DEBUG_PRINT_CODE
  std::string funcName;
  char *str;
  if (compiler->function->name != NULL) {
    sprintf(str, "%.*s", compiler->function->name->string.length,
            compiler->function->name->string.literal);
    funcName = str;
  } else {
    funcName = "<script>";
  }
  if (!parser->hadError) {
    disassembleChunk(compiler->function, funcName.c_str());
  }
#endif
  writeChunks(compiler->function, OP_NIL, OP_RETURN, parser->previous->line);
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

static int resolveLocal(Compiler *compiler, Parser *parser) {
  for (int i = compiler->locals.size() - 1; i >= 0; i--) {
    Local local = compiler->locals[i];
    if (cmpString(local.name.string, parser->previous->string)) {
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

  for (int i = compiler->locals.size() - 1; i >= 0; i--) {
    Local local = compiler->locals[i];
    if (local.depth != -1 && local.depth < compiler->scopeDepth) {
      break;
    }

    if (cmpString(parser->previous->string, local.name.string)) {
      errorAt(parser, "Already a variable with this name in this scope.");
    }
  }
  compiler->locals.push_back(Local(*parser->previous, -1));
}

static uint8_t parseVariable(Compiler *compiler, Parser *parser,
                             Scanner *scanner, const char *errorMessage) {
  consume(parser, scanner, TOKEN_IDENTIFIER, errorMessage);

  declareVariable(compiler, parser);
  if (compiler->scopeDepth > 0) {
    return 0;
  }

  return stringConstant(compiler, parser);
}

static void patchJump(Compiler *compiler, Parser *parser, int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = compiler->function->cp - offset - 2;
  if (jump > UINT16_MAX) {
    errorAt(parser, "Too much code to jump over.");
  }
  compiler->function->code[offset] = (jump >> 8) & 0xff;
  compiler->function->code[offset + 1] = jump & 0xff;
}
static inline void expression(Compiler *compiler, Parser *parser,
                              Scanner *scanner) {
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

  writeChunks(compiler->function, OP_DEFINE_GLOBAL, global,
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

  writeChunk(compiler->function, OP_POP, parser->previous->line);
  parsePrecedence(compiler, parser, scanner, PREC_AND);

  patchJump(compiler, parser, endJump);
}

static void or_(Compiler *compiler, Parser *parser, Scanner *scanner,
                bool canAssign) {
  int elseJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
  int endJump = emitJump(compiler, parser, OP_JUMP);

  patchJump(compiler, parser, elseJump);
  writeChunk(compiler->function, OP_POP, parser->previous->line);

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
  writeChunks(compiler->function, OP_ARRAY, items, parser->previous->line);
}

static void number(Compiler *compiler, Parser *parser, Scanner *scanner) {
  double value = std::stod(parser->previous->string.literal);
  writeChunks(compiler->function, OP_CONSTANT,
              makeConstant(compiler, parser, NUMBER_VAL(value)),
              parser->previous->line);
}

static void mapDeclaration(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  uint8_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACE) {

    do {
      if (match(parser, scanner, TOKEN_STRING)) {
        writeChunks(compiler->function, OP_CONSTANT,
                    stringConstant(compiler, parser), parser->previous->line);

      } else if (match(parser, scanner, TOKEN_NUMBER)) {
        number(compiler, parser, scanner);
      } else {
        errorAt(parser, "Expect number or string as key");
      }

      consume(parser, scanner, TOKEN_COLON,
              "Expect colon between key and value");
      expression(compiler, parser, scanner);

      if (items == 255) {
        errorAt(parser, "Can't have more than 255 arguments.");
      }
      items++;

    } while (match(parser, scanner, TOKEN_COMMA));
  }

  consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after map items.");
  writeChunks(compiler->function, OP_MAP, items, parser->previous->line);
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
    writeChunk(compiler->function, OP_NIL, parser->previous->line);
  }
  consume(parser, scanner, TOKEN_SEMICOLON,
          "Expect ';' after variable declaration");

  defineVariable(compiler, parser, global);
}

static void expressionStatement(Compiler *compiler, Parser *parser,
                                Scanner *scanner) {
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after expression.");
  writeChunk(compiler->function, OP_POP, parser->previous->line);
}

static void beginScope(Compiler *compiler) { compiler->scopeDepth++; }
static void endScope(Compiler *compiler, Parser *parser) {
  compiler->scopeDepth--;
  while (compiler->locals.size() > 0 &&
         compiler->locals[compiler->locals.size() - 1].depth >
             compiler->scopeDepth) {
    writeChunk(compiler->function, OP_POP, parser->previous->line);
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

  int loopStart = compiler->function->cp;
  int exitJump = -1;
  if (!match(parser, scanner, TOKEN_SEMICOLON)) {
    expression(compiler, parser, scanner);
    consume(parser, scanner, TOKEN_SEMICOLON,
            "Expect ';' after loop condition");

    exitJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
    writeChunk(compiler->function, OP_POP, parser->previous->line);
  }

  if (!match(parser, scanner, TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(compiler, parser, OP_JUMP);
    int incrementStart = compiler->function->cp;

    expression(compiler, parser, scanner);
    writeChunk(compiler->function, OP_POP, parser->previous->line);
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
    writeChunk(compiler->function, OP_POP, parser->previous->line);
  }

  endScope(compiler, parser);
}

static void ifStatement(Compiler *compiler, Parser *parser, Scanner *scanner) {
  consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
  writeChunk(compiler->function, OP_POP, parser->previous->line);
  statement(compiler, parser, scanner);

  int elseJump = emitJump(compiler, parser, OP_JUMP);

  patchJump(compiler, parser, thenJump);
  writeChunk(compiler->function, OP_POP, parser->previous->line);

  if (match(parser, scanner, TOKEN_ELSE)) {
    statement(compiler, parser, scanner);
  }
  patchJump(compiler, parser, elseJump);
}

static void printStatement(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after value.");
  writeChunk(compiler->function, OP_PRINT, parser->previous->line);
}

static void returnStatement(Compiler *compiler, Parser *parser,
                            Scanner *scanner) {
  if (compiler->type == TYPE_SCRIPT) {
    errorAt(parser, "Can't return from top-level code");
  }

  if (match(parser, scanner, TOKEN_SEMICOLON)) {
    writeChunks(compiler->function, OP_NIL, OP_RETURN, parser->previous->line);
  } else {
    expression(compiler, parser, scanner);
    consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after return value");
    writeChunk(compiler->function, OP_RETURN, parser->previous->line);
  }
}

static void whileStatement(Compiler *compiler, Parser *parser,
                           Scanner *scanner) {
  int loopStart = compiler->function->cp;
  consume(parser, scanner, TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(compiler, parser, OP_JUMP_IF_FALSE);
  writeChunk(compiler->function, OP_POP, parser->previous->line);
  statement(compiler, parser, scanner);
  emitLoop(compiler, parser, loopStart);

  patchJump(compiler, parser, exitJump);
  writeChunk(compiler->function, OP_POP, parser->previous->line);
}

static void binary(Compiler *compiler, Parser *parser, Scanner *scanner) {
  TokenType operatorType = parser->previous->type;

  parsePrecedence(compiler, parser, scanner,
                  (Precedence)(getPrecedence(operatorType) + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL: {
    writeChunks(compiler->function, OP_EQUAL, OP_NOT, parser->previous->line);
    break;
  }
  case TOKEN_EQUAL_EQUAL: {
    writeChunk(compiler->function, OP_EQUAL, parser->previous->line);
    break;
  }
  case TOKEN_GREATER: {
    writeChunk(compiler->function, OP_GREATER, parser->previous->line);
    break;
  }
  case TOKEN_GREATER_EQUAL: {
    writeChunk(compiler->function, OP_GREATER_EQUAL, parser->previous->line);
    break;
  }
  case TOKEN_LESS: {
    writeChunk(compiler->function, OP_LESS, parser->previous->line);
    break;
  }
  case TOKEN_LESS_EQUAL: {
    writeChunk(compiler->function, OP_LESS_EQUAL, parser->previous->line);
    break;
  }
  case TOKEN_PLUS: {
    writeChunk(compiler->function, OP_ADD, parser->previous->line);
    break;
  }
  case TOKEN_MINUS: {
    writeChunk(compiler->function, OP_SUBTRACT, parser->previous->line);
    break;
  }
  case TOKEN_STAR: {
    writeChunk(compiler->function, OP_MULTIPLY, parser->previous->line);
    break;
  }
  case TOKEN_SLASH: {
    writeChunk(compiler->function, OP_DIVIDE, parser->previous->line);
    break;
  }
  default: {
    return;
  }
  }
}

static void index(Compiler *compiler, Parser *parser, Scanner *scanner) {
  expression(compiler, parser, scanner);
  writeChunk(compiler->function, OP_INDEX, parser->previous->line);
  consume(parser, scanner, TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
}

static void dot(Compiler *compiler, Parser *parser, Scanner *scanner,
                bool canAssign) {
  consume(parser, scanner, TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = stringConstant(compiler, parser);
  if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
    expression(compiler, parser, scanner);
    writeChunks(compiler->function, OP_SET_PROPERTY, name,
                parser->previous->line);
  } else {
    writeChunks(compiler->function, OP_GET_PROPERTY, name,
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
    writeChunk(compiler->function, OP_NEGATE, parser->previous->line);
    break;
  }
  case TOKEN_BANG: {
    writeChunk(compiler->function, OP_NOT, parser->previous->line);
    break;
  }
  default: {
    return;
  }
  }
}

static void namedVariable(Compiler *compiler, Parser *parser, Scanner *scanner,
                          bool canAssign) {

  uint8_t getOp, setOp;
  int arg = resolveLocal(compiler, parser);
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = stringConstant(compiler, parser);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(parser, scanner, TOKEN_EQUAL)) {
    expression(compiler, parser, scanner);
    writeChunks(compiler->function, setOp, (uint8_t)arg,
                parser->previous->line);
  } else if (canAssign && match(parser, scanner, TOKEN_LEFT_BRACKET)) {
    writeChunks(compiler->function, getOp, (uint8_t)arg,
                parser->previous->line);
    expression(compiler, parser, scanner);
    consume(parser, scanner, TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
    if (match(parser, scanner, TOKEN_EQUAL)) {
      expression(compiler, parser, scanner);
      writeChunk(compiler->function, OP_SET_INDEX, parser->previous->line);
    } else {
      writeChunk(compiler->function, OP_INDEX, parser->previous->line);
    }
  } else {
    writeChunks(compiler->function, getOp, (uint8_t)arg,
                parser->previous->line);
  }
}

static void literal(Compiler *compiler, Parser *parser, Scanner *scanner) {
  switch (parser->previous->type) {
  case TOKEN_FALSE: {
    writeChunk(compiler->function, OP_FALSE, parser->previous->line);
    break;
  }
  case TOKEN_NIL: {
    writeChunk(compiler->function, OP_NIL, parser->previous->line);
    break;
  }
  case TOKEN_TRUE: {
    writeChunk(compiler->function, OP_TRUE, parser->previous->line);
    break;
  }
  default: {
    printf("Unknown literal token %d\n", (int)parser->previous->type);
    exit(1);
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
    writeChunks(compiler->function, OP_CONSTANT,
                stringConstant(compiler, parser), parser->previous->line);
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
    writeChunks(compiler->function, OP_CALL,
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
  writeChunks(current->function, OP_CONSTANT,
              makeConstant(current, parser, OBJ_VAL(function)),
              parser->previous->line);
}

static void structArgs(Compiler *compiler, Parser *parser, Scanner *scanner) {
  while (parser->current->type != TOKEN_RIGHT_BRACE) {
    consume(parser, scanner, TOKEN_IDENTIFIER,
            "Expect field identifier in struct");
    writeChunks(compiler->function, OP_STRUCT_ARG,
                stringConstant(compiler, parser), parser->previous->line);
    consume(parser, scanner, TOKEN_SEMICOLON,
            "Expect semicolon after struct field identifier");
  }
}

static void structDeclaration(Compiler *compiler, Parser *parser,
                              Scanner *scanner) {
  consume(parser, scanner, TOKEN_IDENTIFIER, "Expect struct name");
  uint8_t nameConstant = stringConstant(compiler, parser);
  declareVariable(compiler, parser);

  writeChunks(compiler->function, OP_STRUCT, nameConstant,
              parser->previous->line);
  consume(parser, scanner, TOKEN_LEFT_BRACE, "Expect '{' before struct body.");
  structArgs(compiler, parser, scanner);
  consume(parser, scanner, TOKEN_RIGHT_BRACE, "Expect '}' after struct body.");
  consume(parser, scanner, TOKEN_SEMICOLON, "Expect ';' after struct end.");
  defineVariable(compiler, parser, nameConstant);
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
}

Compiler *compile(const char *source) {
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
