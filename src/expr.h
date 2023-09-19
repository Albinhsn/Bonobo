#ifndef EXPR_HEADER
#define EXPR_HEADER

#include "scanner.h"
#include <string>
#include <vector>

enum ExprType {
  BINARY_EXPR,
  GROUPING_EXPR,
  LOGICAL_EXPR,
  LITERAL_EXPR,
  UNARY_EXPR,
  VAR_EXPR,
  CALL_EXPR,
};

enum LiteralType {
  DOUBLE_LITERAL,
  INT_LITERAL,
  BOOL_LITERAL,
  STRING_LITERAL,
};

class Expr {
private:
public:
  ExprType type;
};

class BinaryExpr : Expr {
private:
public:
  Expr *left;
  Token op;
  Expr *right;
};

class GroupingExpr : Expr {
private:
public:
  Expr *expression;
};

class LogicalExpr : Expr {
private:
public:
  Expr *left;
  Token op;
  Expr *right;
};

class LiteralExpr : Expr {
private:
public:
  LiteralType type;
  Token literal;
};

class UnaryExpr : Expr {
private:
public:
  Token op;
  Expr *right;
};

class VarExpr : Expr {
private:
public:
  Token name;
};

class CallExpr : Expr {
private:
public:
  Token callee;
  std::vector<Expr *> arguments;
};

#endif
