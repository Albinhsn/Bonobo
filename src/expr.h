#ifndef EXPR_HEADER
#define EXPR_HEADER

enum ExpressionType {
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

typedef struct {
} AssignExpr;

typedef struct {
} BinaryExpr;

typedef struct {
} GroupingExpr;

typedef struct {
} LogicalExpr;

typedef struct {
} LiteralExpr;

typedef struct {
} UnaryExpr;

typedef struct {
} SetExpr;

typedef struct {
} VarExpr;

typedef struct {
} CallExpr;

typedef struct {
} DotExpr;

typedef struct {
  ExpressionType type;
  union expr {
    AssignExpr assign;
    BinaryExpr binary;
    GroupingExpr grouping;
    LogicalExpr logical;
    LiteralExpr literal;
    UnaryExpr unary;
    SetExpr set;
    VarExpr var;
    CallExpr call;
    DotExpr dot;
  };
} Expression;

#endif
