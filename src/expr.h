#ifndef EXPR_HEADER
#define EXPR_HEADER

#include "scanner.h"
#include "variables.h"

enum ExprType {
    BINARY_EXPR,
    INC_EXPR,
    GROUPING_EXPR,
    LOGICAL_EXPR,
    LITERAL_EXPR,
    COMPARISON_EXPR,
    UNARY_EXPR,
    VAR_EXPR,
    INDEX_EXPR,
    ARRAY_EXPR,
    MAP_EXPR,
    CALL_EXPR,
    DOT_EXPR,
};

enum UnaryOp { BANG_UNARY, NEG_UNARY, PLUS_UNARY};

enum LogicalOp { OR_LOGICAL, AND_LOGICAL };

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
    STR_LITERAL,
};

enum BinaryOp { ADD, SUB, MUL, DIV };

enum IncOp { INC, DEC };

class Expr {
  private:
  public:
    ExprType type;
};

class IncExpr : public Expr {
  private:
  public:
    Expr *expr;
    IncOp op;
    IncExpr(Expr *expr, IncOp op) {
        this->expr = expr;
        this->op = op;
        this->type = INC_EXPR;
    }
};

class BinaryExpr : public Expr {
  private:
  public:
    Expr *left;
    BinaryOp op;
    Expr *right;
    BinaryExpr(Expr *left, BinaryOp op) {
        this->left = left;
        this->op = op;
        this->right = nullptr;
        this->type = BINARY_EXPR;
    }
};

class GroupingExpr : public Expr {
  private:
  public:
    Expr *expression;
    GroupingExpr(Expr *expression) {
        this->type = GROUPING_EXPR;
        this->expression = expression;
    }
};

class MapExpr : public Expr {
  private:
  public:
    std::vector<Expr *> values;
    std::vector<Expr *> keys;
    VarType valueType;
    VarType keyType;
    MapExpr() {
        this->type = MAP_EXPR;
        this->values = std::vector<Expr *>();
        this->keys = std::vector<Expr *>();
    }
};

class ArrayExpr : public Expr {
  private:
  public:
    std::vector<Expr *> items;
    Variable *itemType;
    ArrayExpr() {
        this->type = ARRAY_EXPR;
        this->items = std::vector<Expr *>();
        this->itemType = nullptr;
    }
};

class LogicalExpr : public Expr {
  private:
  public:
    Expr *left;
    LogicalOp op;
    Expr *right;
    LogicalExpr(Expr *left, LogicalOp op) {
        this->left = left;
        this->op = op;
        this->right = nullptr;
        this->type = LOGICAL_EXPR;
    }
};

class ComparisonExpr : public Expr {
  private:
  public:
    Expr *left;
    ComparisonOp op;
    Expr *right;
    ComparisonExpr(Expr *left, ComparisonOp op) {
        this->left = left;
        this->op = op;
        this->right = nullptr;
        this->type = COMPARISON_EXPR;
    }
};

class LiteralExpr : public Expr {
  private:
  public:
    LiteralType literalType;
    Token literal;
    LiteralExpr(Token literal, LiteralType literalType) {
        this->type = LITERAL_EXPR;
        this->literalType = literalType;
        this->literal = literal;
    }
};

class UnaryExpr : public Expr {
  private:
  public:
    UnaryOp op;
    Expr *right;
    UnaryExpr(UnaryOp op) {
        this->op = op;
        this->right = nullptr;
        this->type = UNARY_EXPR;
    };
};

class VarExpr : public Expr {
  private:
  public:
    Token name;
    VarExpr(Token name) {
        this->name = name;
        this->type = VAR_EXPR;
    };
};

class DotExpr : public Expr {
  private:
  public:
    Expr *name;
    Token field;
    DotExpr(Expr *name, Token field) {
        this->type = DOT_EXPR;
        this->name = name;
        this->field = field;
    };
};

class CallExpr : public Expr {
  private:
  public:
    Token callee;
    std::vector<Expr *> arguments;
    CallExpr(Token callee) {
        this->callee = callee;
        this->arguments = std::vector<Expr *>();
        this->type = CALL_EXPR;
    };
};

class IndexExpr : public Expr {
  private:
  public:
    Expr *variable;
    Expr *index;
    IndexExpr() {
        this->type = INDEX_EXPR;
        this->index = nullptr;
        this->variable = nullptr;
    }
};

#endif
