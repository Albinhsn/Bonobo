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

enum LogicalOp {
  LESS_EQUAL_LOGICAL,
  LESS_LOGICAL,
  GREATER_LOGICAL,
  GREATER_EQUAL_LOGICAL,
  EQUAL_EQUAL_LOGICAL,
};

enum LiteralType {
  DOUBLE_LITERAL,
  INT_LITERAL,
  BOOL_LITERAL,
  STRING_LITERAL,
};

enum BinaryOp { DIV, ADD, MUL, SUB };

class Expr {
private:
public:
  ExprType type;
};

class BinaryExpr : public Expr {
private:
public:
  Expr *left;
  BinaryOp op;
  Expr *right;
};

class GroupingExpr : public Expr {
private:
public:
  Expr *expression;
};

class LogicalExpr : public Expr {
private:
public:
  Expr *left;
  LogicalOp op;
  Expr *right;
};

class LiteralExpr : public Expr {
private:
public:
  LiteralType literalType;
  Token literal;
};

class UnaryExpr : public Expr {
private:
public:
  Token op;
  Expr *right;
};

class VarExpr : public Expr {
private:
public:
  Token name;
};

class CallExpr : public Expr {
private:
public:
  Token callee;
  std::vector<Expr *> arguments;
};

#endif
