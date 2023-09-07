
#include "compiler.h"
#include "common.h"
#include "memory.h"
#include "object.h"
#include "opcode.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif
Compiler *current;
Parser *parser;
Scanner *scanner;

static void initCompiler(Compiler *compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = newFunction();
  compiler->type = type;
  compiler->locals = NULL;
  compiler->function->name = NULL;
  compiler->localCap = 0;
  compiler->localLen = 0;
  compiler->scopeDepth = 0;

  if (type != TYPE_SCRIPT) {
    pushStack(OBJ_VAL((Obj *)compiler->function));
    compiler->function->name = copyString(newString(
        parser->previous->string.literal, parser->previous->string.length));
    popStack();
  }
  current = compiler;
}

static void errorAt(const char *message) {
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

static void advance() {
  free(parser->previous);
  parser->previous = parser->current;
  for (;;) {
    parser->current = scanToken(scanner);
    if (parser->current->type != TOKEN_ERROR) {
      break;
    }
    errorAt("error advancing");
  }
}
static void consume(TokenType type, const char *message) {
  if (parser->current->type == type) {
    advance();
    return;
  }

  errorAt(message);
}

static bool match(TokenType type) {
  if (!(parser->current->type == type)) {
    return false;
  }
  advance();
  return true;
}

static void emitLoop(int loopStart) {
  writeChunk(current->function, OP_LOOP, parser->previous->line);

  int offset = current->function->cp - loopStart + 2;
  if (offset > UINT16_MAX) {
    errorAt("Loop body too large.");
  }

  writeChunks(current->function, (offset >> 8 & 0xff), (offset & 0xff),
              parser->previous->line);
}

static int emitJump(uint16_t instruction) {
  writeChunk(current->function, instruction, parser->previous->line);
  writeChunks(current->function, 0xff, 0xff, parser->previous->line);

  return current->function->cp - 2;
}

static uint16_t makeConstant(Value value) {
  int constant = addConstant(current->function, value);

  if (constant > UINT16_MAX) {
    errorAt("Too many constants in one chunk.");
    return 0;
  }
  return (uint16_t)constant;
}

static inline uint16_t stringConstant() {
  return makeConstant(OBJ_VAL(copyString(parser->previous->string)));
}

static ObjFunction *endCompiler() {
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
  writeChunks(current->function, OP_NIL, OP_RETURN, parser->previous->line);
  ObjFunction *function = current->function;
  current = current->enclosing;
  return function;
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

static void parsePrecedence(Precedence precedence) {
  advance();
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(parser->previous->type, canAssign);
  while (precedence <= getPrecedence(parser->current->type)) {
    advance();
    infixRule(parser->previous->type, canAssign);
  }
  if (canAssign && match(TOKEN_EQUAL)) {
    errorAt("Invalid assignment target.");
  }
}

static int resolveLocal() {
  for (int i = current->localLen - 1; i >= 0; i--) {
    Local local = current->locals[i];
    if (cmpString(local.name.string, parser->previous->string)) {
      if (local.depth == -1) {
        errorAt("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }
  return -1;
}

static void declareVariable() {
  if (current->scopeDepth == 0) {
    return;
  }

  for (int i = current->localLen - 1; i >= 0; i--) {
    Local local = current->locals[i];
    if (local.depth != -1 && local.depth < current->scopeDepth) {
      break;
    }

    if (cmpString(parser->previous->string, local.name.string)) {
      errorAt("Already a variable with this name in this scope.");
    }
  }

  if (current->localCap < current->localLen + 1) {
    int oldCapacity = current->localCap;
    current->localCap = GROW_CAPACITY(oldCapacity);
    current->locals =
        GROW_ARRAY(Local, current->locals, oldCapacity, current->localCap);
  }
  Local local;
  local.name = *parser->previous;
  local.depth = -1;
  current->locals[current->localLen++] = local;
}

static uint16_t parseVariable(const char *errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable();
  if (current->scopeDepth > 0) {
    return 0;
  }

  return stringConstant();
}

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = current->function->cp - offset - 2;
  if (jump > UINT16_MAX) {
    errorAt("Too much code to jump over.");
  }
  current->function->code[offset] = (jump >> 8) & 0xff;
  current->function->code[offset + 1] = jump & 0xff;
}
static inline void expression() { parsePrecedence(PREC_ASSIGNMENT); }

static void markInitialized() {
  if (current->scopeDepth == 0) {
    return;
  }
  current->locals[current->localLen - 1].depth = current->scopeDepth;
}

static void defineVariable(uint16_t global) {
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }

  writeChunks(current->function, OP_DEFINE_GLOBAL, global,
              parser->previous->line);
}

static uint16_t argumentList() {
  uint16_t argCount = 0;
  if (!(parser->current->type == TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      if (argCount == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(bool canAssign) {
  int endJump = emitJump(OP_JUMP_IF_FALSE);

  writeChunk(current->function, OP_POP, parser->previous->line);
  parsePrecedence(PREC_AND);

  patchJump(endJump);
}

static void or_(bool canAssign) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  writeChunk(current->function, OP_POP, parser->previous->line);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void arrayDeclaration() {
  uint16_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACKET) {
    do {
      expression();
      if (items == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      items++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_BRACKET, "Expect ')' after arguments.");
  writeChunks(current->function, OP_ARRAY, items, parser->previous->line);
}

static void number() {
  double value = std::stod(parser->previous->string.literal);
  writeChunks(current->function, OP_CONSTANT, makeConstant(NUMBER_VAL(value)),
              parser->previous->line);
}

static void mapDeclaration() {
  uint16_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACE) {

    do {
      if (match(TOKEN_STRING)) {
        writeChunks(current->function, OP_CONSTANT, stringConstant(),
                    parser->previous->line);

      } else if (match(TOKEN_NUMBER)) {
        number();
      } else {
        errorAt("Expect number or string as key");
      }

      consume(TOKEN_COLON, "Expect colon between key and value");
      expression();

      if (items == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      items++;

    } while (match(TOKEN_COMMA));
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after map items.");
  writeChunks(current->function, OP_MAP, items, parser->previous->line);
}

static void varDeclaration() {
  uint16_t global = parseVariable("Expect variable name.");

  if (match(TOKEN_EQUAL)) {
    if (match(TOKEN_LEFT_BRACKET)) {
      arrayDeclaration();
    } else if (match(TOKEN_LEFT_BRACE)) {
      mapDeclaration();
    } else {
      expression();
    }
  } else {
    writeChunk(current->function, OP_NIL, parser->previous->line);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");

  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  writeChunk(current->function, OP_POP, parser->previous->line);
}

static inline void beginScope() { current->scopeDepth++; }
static void endScope() {
  current->scopeDepth--;
  while (current->localLen > 0 &&
         current->locals[current->localLen - 1].depth > current->scopeDepth) {
    writeChunk(current->function, OP_POP, parser->previous->line);
    current->localLen--;
  }
}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TOKEN_SEMICOLON)) {
    //
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }

  int loopStart = current->function->cp;
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition");

    exitJump = emitJump(OP_JUMP_IF_FALSE);
    writeChunk(current->function, OP_POP, parser->previous->line);
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = current->function->cp;

    expression();
    writeChunk(current->function, OP_POP, parser->previous->line);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();
  emitLoop(loopStart);

  if (exitJump != -1) {
    patchJump(exitJump);
    writeChunk(current->function, OP_POP, parser->previous->line);
  }

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  writeChunk(current->function, OP_POP, parser->previous->line);
  statement();

  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);
  writeChunk(current->function, OP_POP, parser->previous->line);

  if (match(TOKEN_ELSE)) {
    statement();
  }
  patchJump(elseJump);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  writeChunk(current->function, OP_PRINT, parser->previous->line);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    errorAt("Can't return from top-level code");
  }

  if (match(TOKEN_SEMICOLON)) {
    writeChunks(current->function, OP_NIL, OP_RETURN, parser->previous->line);
  } else {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value");
    writeChunk(current->function, OP_RETURN, parser->previous->line);
  }
}

static void whileStatement() {
  int loopStart = current->function->cp;
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  writeChunk(current->function, OP_POP, parser->previous->line);
  statement();
  emitLoop(loopStart);

  patchJump(exitJump);
  writeChunk(current->function, OP_POP, parser->previous->line);
}

static void binary() {
  TokenType operatorType = parser->previous->type;

  parsePrecedence((Precedence)(getPrecedence(operatorType) + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL: {
    writeChunks(current->function, OP_EQUAL, OP_NOT, parser->previous->line);
    break;
  }
  case TOKEN_EQUAL_EQUAL: {
    writeChunk(current->function, OP_EQUAL, parser->previous->line);
    break;
  }
  case TOKEN_GREATER: {
    writeChunk(current->function, OP_GREATER, parser->previous->line);
    break;
  }
  case TOKEN_GREATER_EQUAL: {
    writeChunk(current->function, OP_GREATER_EQUAL, parser->previous->line);
    break;
  }
  case TOKEN_LESS: {
    writeChunk(current->function, OP_LESS, parser->previous->line);
    break;
  }
  case TOKEN_LESS_EQUAL: {
    writeChunk(current->function, OP_LESS_EQUAL, parser->previous->line);
    break;
  }
  case TOKEN_PLUS: {
    writeChunk(current->function, OP_ADD, parser->previous->line);
    break;
  }
  case TOKEN_MINUS: {
    writeChunk(current->function, OP_SUBTRACT, parser->previous->line);
    break;
  }
  case TOKEN_STAR: {
    writeChunk(current->function, OP_MULTIPLY, parser->previous->line);
    break;
  }
  case TOKEN_SLASH: {
    writeChunk(current->function, OP_DIVIDE, parser->previous->line);
    break;
  }
  default: {
    return;
  }
  }
}

static void index() {
  expression();
  writeChunk(current->function, OP_INDEX, parser->previous->line);
  consume(TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint16_t name = stringConstant();
  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    writeChunks(current->function, OP_SET_PROPERTY, name,
                parser->previous->line);
  } else {
    writeChunks(current->function, OP_GET_PROPERTY, name,
                parser->previous->line);
  }
}

static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary() {
  TokenType operatorType = parser->previous->type;
  parsePrecedence(PREC_UNARY);

  switch (operatorType) {
  case TOKEN_MINUS: {
    writeChunk(current->function, OP_NEGATE, parser->previous->line);
    break;
  }
  case TOKEN_BANG: {
    writeChunk(current->function, OP_NOT, parser->previous->line);
    break;
  }
  default: {
    return;
  }
  }
}

static void namedVariable(bool canAssign) {

  uint16_t getOp, setOp;
  int arg = resolveLocal();
  if (arg != -1) {
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else {
    arg = stringConstant();
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    writeChunks(current->function, setOp, (uint16_t)arg,
                parser->previous->line);
  } else if (canAssign && match(TOKEN_LEFT_BRACKET)) {
    writeChunks(current->function, getOp, (uint16_t)arg,
                parser->previous->line);
    expression();
    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
    if (match(TOKEN_EQUAL)) {
      expression();
      writeChunk(current->function, OP_SET_INDEX, parser->previous->line);
    } else {
      writeChunk(current->function, OP_INDEX, parser->previous->line);
    }
  } else {
    writeChunks(current->function, getOp, (uint16_t)arg,
                parser->previous->line);
  }
}

static void literal() {
  switch (parser->previous->type) {
  case TOKEN_FALSE: {
    writeChunk(current->function, OP_FALSE, parser->previous->line);
    break;
  }
  case TOKEN_NIL: {
    writeChunk(current->function, OP_NIL, parser->previous->line);
    break;
  }
  case TOKEN_TRUE: {
    writeChunk(current->function, OP_TRUE, parser->previous->line);
    break;
  }
  default: {
    printf("Unknown literal token %d\n", (int)parser->previous->type);
    exit(1);
  }
  }
}

static void prefixRule(TokenType type, bool canAssign) {
  switch (type) {
  case TOKEN_LEFT_PAREN: {
    grouping();
    break;
  }
  case TOKEN_MINUS: {
    unary();
    break;
  }
  case TOKEN_STRING: {
    writeChunks(current->function, OP_CONSTANT, stringConstant(),
                parser->previous->line);
    break;
  }
  case TOKEN_NUMBER: {
    number();
    break;
  }
  case TOKEN_FALSE: {
    literal();
    break;
  }
  case TOKEN_TRUE: {
    literal();
    break;
  }
  case TOKEN_NIL: {
    literal();
    break;
  }
  case TOKEN_BANG: {
    unary();
    break;
  }
  case TOKEN_IDENTIFIER: {
    namedVariable(canAssign);
    break;
  }
  default: {
    break;
  }
  }
}
static void infixRule(TokenType type, bool canAssign) {
  switch (type) {
  case TOKEN_LEFT_BRACKET: {
    index();
    break;
  }
  case TOKEN_LEFT_PAREN: {
    writeChunks(current->function, OP_CALL, argumentList(),
                parser->previous->line);
    break;
  }
  case TOKEN_DOT: {
    dot(canAssign);
    break;
  }
  case TOKEN_MINUS: {
    binary();
    break;
  }
  case TOKEN_PLUS: {
    binary();
    break;
  }
  case TOKEN_STAR: {
    binary();
    break;
  }
  case TOKEN_SLASH: {
    binary();
    break;
  }
  case TOKEN_BANG_EQUAL: {
    binary();
    break;
  }
  case TOKEN_EQUAL_EQUAL: {
    binary();
    break;
  }
  case TOKEN_GREATER: {
    binary();
    break;
  }
  case TOKEN_GREATER_EQUAL: {
    binary();
    break;
  }
  case TOKEN_LESS: {
    binary();
    break;
  }
  case TOKEN_LESS_EQUAL: {
    binary();
    break;
  }
  case TOKEN_AND: {
    and_(canAssign);
  }
  case TOKEN_OR: {
    or_(canAssign);
  }
  default: {
    break;
  }
  }
}

static void block() {
  while (parser->current->type != TOKEN_RIGHT_BRACE &&
         parser->current->type != TOKEN_EOF) {
    declaration();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  Compiler *compiler = (Compiler *)malloc(sizeof(Compiler));
  initCompiler(compiler, type);
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (parser->current->type != TOKEN_RIGHT_PAREN) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAt("Can't at have more than 255 parameters.");
      }
      uint16_t constant = parseVariable("Expect parameter name.");
      defineVariable(constant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after last function param.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' after function params.");
  block();

  ObjFunction *function = endCompiler();
  freeCompiler(compiler);
  writeChunks(current->function, OP_CONSTANT, makeConstant(OBJ_VAL(function)),
              parser->previous->line);
}

static void structArgs() {
  while (parser->current->type != TOKEN_RIGHT_BRACE) {
    consume(TOKEN_IDENTIFIER, "Expect field identifier in struct");
    writeChunks(current->function, OP_STRUCT_ARG, stringConstant(),
                parser->previous->line);
    consume(TOKEN_SEMICOLON, "Expect semicolon after struct field identifier");
  }
}

static void structDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect struct name");
  uint16_t nameConstant = stringConstant();
  declareVariable();

  writeChunks(current->function, OP_STRUCT, nameConstant,
              parser->previous->line);
  consume(TOKEN_LEFT_BRACE, "Expect '{' before struct body.");
  structArgs();
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after struct body.");
  consume(TOKEN_SEMICOLON, "Expect ';' after struct end.");
  defineVariable(nameConstant);
}

static void funDeclaration() {
  uint16_t global = parseVariable("Expect function name");
  markInitialized();
  function(TYPE_FUNCTION);
  defineVariable(global);
}

static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    block();
  } else {
    expressionStatement();
  }
}

static void declaration() {
  if (match(TOKEN_FUN)) {
    funDeclaration();
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else if (match(TOKEN_STRUCT)) {
    structDeclaration();
  } else {
    statement();
  }
}
static void initParser() {
  parser = (Parser *)malloc(sizeof(Parser));
  parser->current = NULL;
  parser->previous = NULL;
  parser->hadError = false;
}

Compiler *compile(const char *source) {
  scanner = (Scanner *)malloc(sizeof(Scanner));
  initScanner(scanner, source);
  initParser();

  Compiler *compiler = (Compiler *)malloc(sizeof(Compiler));
  initCompiler(compiler, TYPE_SCRIPT);

  advance();
  while (!match(TOKEN_EOF)) {
    declaration();
  }
  bool hadError = parser->hadError;
  endCompiler();

  freeScanner(scanner);
  freeParser(parser);

  if (hadError) {
    freeCompiler(compiler);
  }

  return hadError ? NULL : compiler;
}
void markCompilerRoots() {
  Compiler *compiler = current;
  while (compiler != NULL) {
    markObject((Obj *)compiler->function);
    if (compiler->function->name != NULL) {
      markObject((Obj *)compiler->function->name);
    }
    compiler = compiler->enclosing;
  }
}
