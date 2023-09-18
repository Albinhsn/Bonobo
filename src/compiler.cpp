#include "compiler.h"
#include "common.h"
#include "scanner.h"

Compiler *compiler;
Parser *parser;
Scanner *scanner;

static void initCompiler() {
  compiler = (Compiler *)malloc(sizeof(Compiler));
  compiler->ctx = new llvm::LLVMContext();
  compiler->module = new llvm::Module("Bonobo", *compiler->ctx);
  compiler->builder = new llvm::IRBuilder<>(*compiler->ctx);
  compiler->stringConstants = std::vector<llvm::GlobalVariable>();
}

static void endCompiler(Compiler *current) {
  compiler = current->enclosing;
  free(current);
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

static inline void stringConstant() {
  llvm::Constant *strConstant = llvm::ConstantDataArray::getString(
      *compiler->ctx, parser->previous->lexeme);
  llvm::GlobalVariable *strGlobal =
      new llvm::GlobalVariable(*compiler->module, strConstant->getType(), true,
                               llvm::GlobalValue::PrivateLinkage, strConstant);
  compiler->stringConstants.push_back(*strGlobal);
}

static void emitIR() {
  std::error_code errorCode;
  llvm::raw_fd_ostream outLL("./out.ll", errorCode);
  compiler->module->print(outLL, nullptr);
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

static void expression(Precedence precedence) {
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

// static llvm::Value *resolveLocal() {}
static void resolveLocal() {}

static void declareVariable() {}

static void parseVariable(const char *errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  declareVariable();
}

static void defineVariable(uint16_t global) {}

static uint16_t argumentList() {
  uint16_t argCount = 0;
  if (!(parser->current->type == TOKEN_RIGHT_PAREN)) {
    do {
      expression(PREC_ASSIGNMENT);
      if (argCount == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(bool canAssign) { expression(PREC_AND); }

static void or_(bool canAssign) { expression(PREC_OR); }

static void arrayDeclaration() {
  uint16_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACKET) {
    do {
      expression(PREC_ASSIGNMENT);
      if (items == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      items++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_BRACKET, "Expect ')' after arguments.");
}

// Should handle float/int
static void number() { double value = std::stod(parser->previous->lexeme); }

static void mapDeclaration() {
  uint16_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACE) {

    do {
      if (match(TOKEN_STRING)) {
      } else if (match(TOKEN_INT)) {
        number();
      } else {
        errorAt("Expect number or string as key");
      }

      consume(TOKEN_COLON, "Expect colon between key and value");
      expression(PREC_ASSIGNMENT);

      if (items == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      items++;

    } while (match(TOKEN_COMMA));
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after map items.");
}

static void varDeclaration() {
  parseVariable("Expect variable name.");
  consume(TOKEN_COLON, "Expect type declaration after var name");
  if (match(TOKEN_STR)) {

  } else if (match(TOKEN_INT)) {

  } else if (match(TOKEN_DOUBLE)) {

  } else if (match(TOKEN_BOOL)) {

  } else if (match(TOKEN_MAP)) {

  } else if (match(TOKEN_ARRAY)) {
  }

  if (!match(TOKEN_EQUAL)) {
    errorAt("Expected assignment at var declaration");
  }
  if (match(TOKEN_LEFT_BRACKET)) {
    arrayDeclaration();
  } else if (match(TOKEN_LEFT_BRACE)) {
    mapDeclaration();
  } else {
    expression(PREC_ASSIGNMENT);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");

  // defineVariable(global);
}

static void expressionStatement() {
  expression(PREC_ASSIGNMENT);
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
}

static inline void beginScope() {}

static void endScope() {}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TOKEN_SEMICOLON)) {
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }

  if (!match(TOKEN_SEMICOLON)) {
    expression(PREC_ASSIGNMENT);
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition");
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    expression(PREC_ASSIGNMENT);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
  }

  statement();

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression(PREC_ASSIGNMENT);
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  statement();

  if (match(TOKEN_ELSE)) {
    statement();
  }
}

static void printStatement() {
  expression(PREC_ASSIGNMENT);
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
}

static void returnStatement() {
  if (compiler->type == TYPE_SCRIPT) {
    errorAt("Can't return from top-level code");
  }

  if (match(TOKEN_SEMICOLON)) {
  } else {
    expression(PREC_ASSIGNMENT);
    consume(TOKEN_SEMICOLON, "Expect ';' after return value");
  }
}

static void whileStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression(PREC_ASSIGNMENT);
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  statement();
}

static void binary() {
  TokenType operatorType = parser->previous->type;

  expression((Precedence)(getPrecedence(operatorType) + 1));

  switch (operatorType) {
  case TOKEN_BANG_EQUAL: {
    break;
  }
  case TOKEN_EQUAL_EQUAL: {
    break;
  }
  case TOKEN_GREATER: {
    break;
  }
  case TOKEN_GREATER_EQUAL: {
    break;
  }
  case TOKEN_LESS: {
    break;
  }
  case TOKEN_LESS_EQUAL: {
    break;
  }
  case TOKEN_PLUS: {
    break;
  }
  case TOKEN_MINUS: {
    break;
  }
  case TOKEN_STAR: {
    break;
  }
  case TOKEN_SLASH: {
    break;
  }
  default: {
    return;
  }
  }
}

static void index() {
  expression(PREC_ASSIGNMENT);
  consume(TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  if (canAssign && match(TOKEN_EQUAL)) {
    expression(PREC_ASSIGNMENT);
  } else {
  }
}

static void grouping() {
  expression(PREC_ASSIGNMENT);
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary() {
  TokenType operatorType = parser->previous->type;
  expression(PREC_UNARY);

  switch (operatorType) {
  case TOKEN_MINUS: {
    break;
  }
  case TOKEN_BANG: {
    break;
  }
  default: {
    return;
  }
  }
}

static void namedVariable(bool canAssign) {

  resolveLocal();
  int arg = -1;
  if (arg != -1) {
  } else {
    stringConstant();
  }
  if (canAssign && match(TOKEN_EQUAL)) {
    expression(PREC_ASSIGNMENT);
  } else if (canAssign && match(TOKEN_LEFT_BRACKET)) {
    expression(PREC_ASSIGNMENT);
    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
    if (match(TOKEN_EQUAL)) {
      expression(PREC_ASSIGNMENT);
    } else {
    }
  } else {
  }
}

static void literal() {
  switch (parser->previous->type) {
  case TOKEN_FALSE: {
    break;
  }
  case TOKEN_NIL: {
    break;
  }
  case TOKEN_TRUE: {
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
    break;
  }
  case TOKEN_INT: {
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
  Compiler *funcCompiler = (Compiler *)malloc(sizeof(Compiler));
  funcCompiler->builder = new llvm::IRBuilder<>(*compiler->ctx);
  funcCompiler->module = compiler->module;
  funcCompiler->ctx = compiler->ctx;
  funcCompiler->type = TYPE_SCRIPT;

  compiler->enclosing = funcCompiler;
  compiler = funcCompiler;

  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (parser->current->type != TOKEN_RIGHT_PAREN) {
    do {
      parseVariable("Expect parameter name.");
      defineVariable(0);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after last function param.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' after function params.");
  block();

  endCompiler(compiler);
}

static void structArgs() {
  while (parser->current->type != TOKEN_RIGHT_BRACE) {
    consume(TOKEN_IDENTIFIER, "Expect field identifier in struct");
    consume(TOKEN_SEMICOLON, "Expect semicolon after struct field identifier");
  }
}

static void structDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect struct name");
  stringConstant();
  declareVariable();

  consume(TOKEN_LEFT_BRACE, "Expect '{' before struct body.");
  structArgs();
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after struct body.");
  consume(TOKEN_SEMICOLON, "Expect ';' after struct end.");
  defineVariable(0);
}

static void funDeclaration() {
  parseVariable("Expect function name");
  function(TYPE_FUNCTION);
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
    beginScope();
    block();
    endScope();
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

void compile(const char *source) {
  scanner = (Scanner *)malloc(sizeof(Scanner));
  initScanner(scanner, source);
  initParser();

  initCompiler();

  advance();
  while (!match(TOKEN_EOF)) {
    declaration();
  }
  bool hadError = parser->hadError;
  if (!hadError) {
    emitIR();
  }

  free(scanner);
  free(parser);
  free(compiler);
}
