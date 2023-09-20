#ifndef EXPR_HEADER
#define EXPR_HEADER

#include "scanner.h"

enum ExprType {
  BINARY_EXPR,
  GROUPING_EXPR,
  LOGICAL_EXPR,
  LITERAL_EXPR,
  COMPARISON_EXPR,
  UNARY_EXPR,
  VAR_EXPR,
  CALL_EXPR,
};

enum UnaryOp { BANG_UNARY, NEG_UNARY };

enum LogicalOp { AND_LOGICAL, OR_LOGICAL };

enum ComparisonOp {
  LESS_EQUAL_COMPARISON,
  LESS_COMPARISON,
  GREATER_COMPARISON,
  GREATER_EQUAL_COMPARISON,
  EQUAL_EQUAL_COMPARISON,
};

enum LiteralType {
  DOUBLE_LITERAL,
  INT_LITERAL,
  BOOL_LITERAL,
  STRING_LITERAL,
};

enum BinaryOp { ADD, SUB, MUL, DIV};

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

class ComparisonExpr : public Expr {
private:
public:
  Expr *left;
  ComparisonOp op;
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
  UnaryOp op;
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
