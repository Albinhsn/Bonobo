#ifndef STMT_HEADER
#define STMT_HEADER

#include "expr.h"

enum StatementType {
    EXPR_STMT,
    RETURN_STMT,
    VAR_STMT,
    WHILE_STMT,
    STRUCT_STMT,
    IF_STMT,
    FUNC_STMT
};

enum VarType {
    STRING_VAR,
    INT_VAR,
    DOUBLE_VAR,
    BOOL_VAR,
    MAP_VAR,
    ARRAY_VAR,
    STRUCT_VAR
};

class Stmt {
  private:
  public:
    StatementType type;
};

class ExprStmt : public Stmt {
  private:
  public:
    Expr *expression;
};

class ReturnStmt : public Stmt {
  private:
  public:
    Token keyword;
    Expr *value;
};

class VarStmt : public Stmt {
  private:
  public:
    Token name;
    VarType varType;
    Expr *initializer;
};

class AssignStmt : public Stmt {
  private:
  public:
    Token name;
    Expr *value;
};

class WhileStmt : public Stmt {
  private:
  public:
    Expr *condition;
    Stmt body;
};

class StructStmt : public Stmt {
  private:
  public:
    Token name;
    std::vector<Token> fieldNames;
    std::vector<LiteralType> fieldTypes;
};

class IfStmt : public Stmt {
  private:
  public:
    Expr *condition;
    std::vector<Stmt*> thenBranch;
    std::vector<Stmt*> elseBranch;
    IfStmt() {
        this->type = IF_STMT;
        this->condition = NULL;
        this->thenBranch = std::vector<Stmt*>();
        this->elseBranch = std::vector<Stmt*>();
    }
};

class FuncStmt : public Stmt {
  private:
  public:
    Token name;
    std::vector<Token> params;
    std::vector<Stmt> body;
};

#endif
