#ifndef EXPR_HEADER
#define EXPR_HEADER

#include "scanner.h"
#include "variables.h"
#include <vector>

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

enum UnaryOp { BANG_UNARY, NEG_UNARY, PLUS_UNARY };

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
    Variable *evaluatesTo;
    ExprType type;
    int line;
};

class IncExpr : public Expr {
  private:
  public:
    Expr *expr;
    IncOp op;
    IncExpr(Expr *expr, IncOp op, int line) {
        this->expr = expr;
        this->op = op;
        this->type = INC_EXPR;
        this->line = line;
    }
};

class BinaryExpr : public Expr {
  private:
  public:
    Expr *left;
    BinaryOp op;
    Expr *right;
    BinaryExpr(Expr *left, BinaryOp op, int line) {
        this->left = left;
        this->op = op;
        this->right = nullptr;
        this->type = BINARY_EXPR;
        this->line = line;
    }
};

class GroupingExpr : public Expr {
  private:
  public:
    Expr *expression;
    GroupingExpr(Expr *expression, int line) {
        this->type = GROUPING_EXPR;
        this->expression = expression;
        this->line = line;
    }
};

class MapExpr : public Expr {
  private:
  public:
    std::vector<Expr *> values;
    std::vector<Expr *> keys;
    Variable *mapVar;
    MapExpr(int line) {
        this->type = MAP_EXPR;
        this->values = std::vector<Expr *>();
        this->keys = std::vector<Expr *>();
        this->line = line;
    }
};

class ArrayExpr : public Expr {
  private:
  public:
    std::vector<Expr *> items;
    Variable *itemType;
    ArrayExpr(int line) {
        this->type = ARRAY_EXPR;
        this->items = std::vector<Expr *>();
        this->itemType = nullptr;
        this->line = line;
    }
};

class LogicalExpr : public Expr {
  private:
  public:
    Expr *left;
    LogicalOp op;
    Expr *right;
    LogicalExpr(Expr *left, LogicalOp op, int line) {
        this->left = left;
        this->op = op;
        this->right = nullptr;
        this->type = LOGICAL_EXPR;
        this->line = line;
    }
};

class ComparisonExpr : public Expr {
  private:
  public:
    Expr *left;
    ComparisonOp op;
    Expr *right;
    ComparisonExpr(Expr *left, ComparisonOp op, int line) {
        this->left = left;
        this->op = op;
        this->right = nullptr;
        this->type = COMPARISON_EXPR;
        this->line = line;
    }
};

class LiteralExpr : public Expr {
  private:
  public:
    LiteralType literalType;
    std::string literal;
    LiteralExpr(std::string literal, LiteralType literalType, int line) {
        this->type = LITERAL_EXPR;
        this->literalType = literalType;
        this->literal = literal;
        this->line = line;
    }
};

class UnaryExpr : public Expr {
  private:
  public:
    UnaryOp op;
    Expr *right;
    UnaryExpr(UnaryOp op, int line) {
        this->op = op;
        this->right = nullptr;
        this->type = UNARY_EXPR;
        this->line = line;
    };
};

class VarExpr : public Expr {
  private:
  public:
    std::string name;
    VarExpr(std::string name, int line) {
        this->name = name;
        this->type = VAR_EXPR;
        this->line = line;
    };
};

class DotExpr : public Expr {
  private:
  public:
    Expr *name;
    std::string field;
    DotExpr(Expr *name, std::string field, int line) {
        this->type = DOT_EXPR;
        this->name = name;
        this->field = field;
        this->line = line;
    };
};

class CallExpr : public Expr {
  private:
  public:
    std::string callee;
    std::vector<Expr *> arguments;
    CallExpr(std::string callee, int line) {
        this->callee = callee;
        this->arguments = std::vector<Expr *>();
        this->type = CALL_EXPR;
        this->line = line;
    };
};

class IndexExpr : public Expr {
  private:
  public:
    Expr *variable;
    Expr *index;
    IndexExpr(Expr *variable, Expr *index, int line) {
        this->type = INDEX_EXPR;
        this->index = index;
        this->variable = variable;
        this->line = line;
    }
};

#endif
