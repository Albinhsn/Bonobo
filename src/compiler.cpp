#include "compiler.h"
#include "common.h"
#include "debug.h"
#include "scanner.h"
#include <cstdio>
#include <map>
#include <string>

Compiler *compiler;
Parser *parser;
Scanner *scanner;

static void initCompiler() {
  compiler = (Compiler *)malloc(sizeof(Compiler));
  compiler->variables = std::map<std::string, LiteralExpr>();
  compiler->statements = std::vector<Stmt *>();
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
  exit(1);
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

static inline void stringConstant() {}

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

static LiteralType getLiteralType() {
  switch (parser->previous->type) {
  case TOKEN_INT: {
    return INT_LITERAL;
  }
  case TOKEN_DOUBLE: {
    return DOUBLE_LITERAL;
  }
  case TOKEN_TRUE: {
    return BOOL_LITERAL;
  }
  case TOKEN_FALSE: {
    return BOOL_LITERAL;
  }
  case TOKEN_STRING: {
    return STRING_LITERAL;
  }
  default: {
    break;
  }
  }
  errorAt("Unable to get literal type");
  exit(1);
}

static void literal(Expr *&expr) {
  if (expr == NULL) {
    LiteralExpr *literalExpr = new LiteralExpr;
    literalExpr->literal = *parser->previous;
    literalExpr->literalType = getLiteralType();
    literalExpr->type = LITERAL_EXPR;

    delete expr;
    expr = literalExpr;
  } else {
    switch (expr->type) {
    case BINARY_EXPR: {
      BinaryExpr *binaryExpr = (BinaryExpr *)expr;
      literal(binaryExpr->right);
      break;
    }
    case GROUPING_EXPR: {
      GroupingExpr *groupingExpr = (GroupingExpr *)expr;
      literal(groupingExpr->expression);
      break;
    }
    case LOGICAL_EXPR: {
      LogicalExpr *logicalExpr = (LogicalExpr *)expr;
      literal(logicalExpr->right);
      break;
    }
    case UNARY_EXPR: {
      UnaryExpr *unaryExpr = (UnaryExpr *)expr;
      literal(unaryExpr->right);
      break;
    }
    case CALL_EXPR: {
      CallExpr *callExpr = (CallExpr *)expr;
      literal(callExpr->arguments.back());
      break;
    }
    default: {
      errorAt("Can't add literal to expr");
      break;
    }
    }
  }
}

static void grouping(Expr *&expr) {
  GroupingExpr *groupingExpr = new GroupingExpr;

  groupingExpr->type = GROUPING_EXPR;
  groupingExpr->expression = expression(expr);

  consume(TOKEN_RIGHT_PAREN, "Grouping wasn't closed");

  delete expr;

  expr = groupingExpr;
}

static BinaryOp getBinaryOp(Token *token) {
  switch (token->type) {
  case TOKEN_PLUS: {
    return ADD;
  }
  case TOKEN_MINUS: {
    return SUB;
  }
  case TOKEN_SLASH: {
    return DIV;
  }
  case TOKEN_STAR: {
    return MUL;
  }
  default: {
    errorAt("Unknown binaryOp type");
    exit(1);
  }
  }
}

static void operation(Expr *&expr) {
  if (expr == NULL) {
    errorAt("What can't op without expr");
  }
  BinaryOp op = getBinaryOp(parser->previous);
  switch (expr->type) {
  case LITERAL_EXPR: {
    BinaryExpr *binaryExpr = new BinaryExpr;
    binaryExpr->op = op;

    binaryExpr->left = expr;
    binaryExpr->type = BINARY_EXPR;

    expr = binaryExpr;
    break;
  }
  }
}

static void logical(Expr *&expr) {}

static Expr *expression(Expr *expr) {
  while (parser->current->type != TOKEN_SEMICOLON &&
         parser->current->type != TOKEN_RIGHT_PAREN) {
    advance();
    switch (parser->previous->type) {
    case TOKEN_INT: {
      literal(expr);
      break;
    }
    case TOKEN_DOUBLE: {
      literal(expr);
      break;
    }
    case TOKEN_STRING: {
      literal(expr);
      break;
    }
    case TOKEN_IDENTIFIER: {
      literal(expr);
      break;
    }
    case TOKEN_TRUE: {
      literal(expr);
      break;
    }
    case TOKEN_FALSE: {
      literal(expr);
      break;
    }
    case TOKEN_PLUS: {
      operation(expr);
      break;
    }
    case TOKEN_MINUS: {
      operation(expr);
      break;
    }
    case TOKEN_STAR: {
      operation(expr);
      break;
    }
    case TOKEN_SLASH: {
      operation(expr);
      break;
    }
    case TOKEN_BANG: {
      logical(expr);
      break;
    }
    case TOKEN_LESS: {
      logical(expr);
      break;
    }
    case TOKEN_LESS_EQUAL: {
      logical(expr);
      break;
    }
    case TOKEN_GREATER: {
      logical(expr);
      break;
    }
    case TOKEN_GREATER_EQUAL: {
      logical(expr);
      break;
    }
    case TOKEN_EQUAL_EQUAL: {
      logical(expr);
      break;
    }
    case TOKEN_LEFT_PAREN: {
      grouping(expr);
      break;
    }
    default: {
      printf("%.*s\n", parser->current->length, parser->current->lexeme);
      errorAt("Can't parse expr with this token");
    }
    }
  }
  return expr;
}

static void resolveLocal() {}

static Token declareVariable() {
  Token varName = *parser->previous;
  std::string varNameString(varName.lexeme, varName.length);
  if (compiler->variables.count(varNameString)) {
    errorAt(("Already declared variable with name " + varNameString).c_str());
  }

  compiler->variables[varNameString] = LiteralExpr();
  return varName;
}

static Token parseVariable(const char *errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);
  return declareVariable();
}

static void defineVariable(uint16_t global) {}

static uint16_t argumentList() {
  uint16_t argCount = 0;
  if (!(parser->current->type == TOKEN_RIGHT_PAREN)) {
    do {
      Expr *expr = new Expr();
      expression(expr);
      if (argCount == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static Expr *arrayDeclaration() {
  uint16_t items = 0;
  if (parser->current->type != TOKEN_RIGHT_BRACKET) {
    do {
      expression(NULL);
      if (items == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      items++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_BRACKET, "Expect ')' after arguments.");
  return NULL;
}

// Should handle float/int
static void number() { double value = std::stod(parser->previous->lexeme); }

static Expr *mapDeclaration() {
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
      expression(NULL);

      if (items == 255) {
        errorAt("Can't have more than 255 arguments.");
      }
      items++;

    } while (match(TOKEN_COMMA));
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after map items.");
  return NULL;
}

static void varDeclaration() {
  VarStmt *varStmt = (VarStmt *)malloc(sizeof(VarStmt));
  varStmt->type = VAR_STMT;
  varStmt->name = parseVariable("Expect variable name.");
  consume(TOKEN_COLON, "Expect type declaration after var name");
  if (match(TOKEN_STR)) {
    varStmt->varType = STRING_VAR;
  } else if (match(TOKEN_INT)) {
    varStmt->varType = INT_VAR;
  } else if (match(TOKEN_DOUBLE)) {
    varStmt->varType = DOUBLE_VAR;
  } else if (match(TOKEN_BOOL)) {
    varStmt->varType = BOOL_VAR;
  } else if (match(TOKEN_MAP)) {
    varStmt->varType = MAP_VAR;
  } else if (match(TOKEN_ARRAY)) {
    varStmt->varType = ARRAY_VAR;
  } else if (match(TOKEN_STRUCT)) {
    varStmt->varType = STRUCT_VAR;
  } else {
    errorAt("Expected type declaration after ':'");
  }

  consume(TOKEN_EQUAL, "Expected assignment at var declaration");

  if (match(TOKEN_LEFT_BRACKET)) {
    varStmt->initializer = arrayDeclaration();
  } else if (match(TOKEN_LEFT_BRACE)) {
    varStmt->initializer = mapDeclaration();
  } else {
    varStmt->initializer = expression(NULL);
  }

  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");
  compiler->statements.push_back((Stmt *)varStmt);
}

static void expressionStatement() {
  Expr *expr = new Expr();
  expression(expr);
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
    Expr *expr = new Expr();
    expression(expr);
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition");
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    Expr *expr = new Expr();
    expression(expr);
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
  }

  statement();

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  Expr *expr = new Expr();
  expression(expr);
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  statement();

  if (match(TOKEN_ELSE)) {
    statement();
  }
}

static void printStatement() {
  Expr *expr = new Expr();
  expression(expr);
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
}

static void returnStatement() {
  if (compiler->type == TYPE_SCRIPT) {
    errorAt("Can't return from top-level code");
  }

  if (match(TOKEN_SEMICOLON)) {
  } else {
    Expr *expr = new Expr();
    expression(expr);
    consume(TOKEN_SEMICOLON, "Expect ';' after return value");
  }
}

static void whileStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  Expr *expr = new Expr();
  expression(expr);
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  statement();
}

static void binary() {
  TokenType operatorType = parser->previous->type;

  Expr *expr = new Expr();
  expression(expr);

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
  Expr *expr = new Expr();
  expression(expr);
  consume(TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  if (canAssign && match(TOKEN_EQUAL)) {
    Expr *expr = new Expr();
    expression(expr);
  } else {
  }
}

static void grouping() {
  Expr *expr = new Expr();
  expression(expr);
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void unary() {
  TokenType operatorType = parser->previous->type;
  Expr *expr = new Expr();
  expression(expr);

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
    Expr *expr = new Expr();
    expression(expr);
  } else if (canAssign && match(TOKEN_LEFT_BRACKET)) {
    Expr *expr = new Expr();
    expression(expr);
    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after indexing");
    if (match(TOKEN_EQUAL)) {
      Expr *expr = new Expr();
      expression(expr);
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
  }
  case TOKEN_OR: {
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
  debugStatements(compiler);

  free(scanner);
  free(parser);
  free(compiler);
}
