#ifndef EXPR_HEADER
#define EXPR_HEADER

#include "compiler.h"
#include "scanner.h"
#include <string>
#include <vector>

enum ExprType {
  ASSIGN_EXPR,
  BINARY_EXPR,
  GROUPING_EXPR,
  LOGICAL_EXPR,
  LITERAL_EXPR,
  UNARY_EXPR,
  SET_EXPR,
  VAR_EXPR,
  CALL_EXPR,
  DOT_EXPR
};

enum LiteralType {
  FLOAT_LITERAL,
  INT_LITERAL,
  BOOL_LITERAL,
  STRING_LITERAL,
};

class Expr {
private:
public:
  ExprType type;
};

class AssignExpr : Expr {
private:
public:
  Token name;
  Expr value;
};

class BinaryExpr : Expr {
private:
public:
  Expr left;
  Token op;
  Expr right;
};

class GroupingExpr : Expr {
private:
public:
  Expr expression;
};

class LogicalExpr : Expr {
private:
public:
  Expr left;
  Token op;
  Expr right;
};

class LiteralExpr : Expr {
private:
public:
  LiteralType type;
  union {
    double dbl;
    int integer;
    std::string string;
    bool boolean;
  } literal;
};

class UnaryExpr : Expr {
private:
public:
  Token op;
  Expr right;
};

class VarExpr : Expr {
private:
public:
  Token name;
};

class CallExpr : Expr {
private:
public:
  Expr callee;
  std::vector<Expr> arguments;
};

typedef struct {
} DotExpr;

#endif
