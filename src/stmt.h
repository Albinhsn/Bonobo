#ifndef STMT_HEADER
#define STMT_HEADER

#include "expr.h"

enum StatementType {
    EXPR_STMT,
    ASSIGN_STMT,
    RETURN_STMT,
    VAR_STMT,
    WHILE_STMT,
    FOR_STMT,
    STRUCT_STMT,
    IF_STMT,
    FUNC_STMT
};

enum VarType {
    STR_VAR,
    INT_VAR,
    DOUBLE_VAR,
    BOOL_VAR,
    MAP_VAR,
    ARRAY_VAR,
    STRUCT_VAR
};

class Variable {
  private:
  public:
    VarType type;
    Token name;
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
    ExprStmt() {
        this->expression = NULL;
        this->type = EXPR_STMT;
    }
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
    Variable var;
    Expr *initializer;
};

class AssignStmt : public Stmt {
  private:
  public:
    Token name;
    Expr *value;
    AssignStmt() {
        this->value = NULL;
        this->type = ASSIGN_STMT;
    }
};

class WhileStmt : public Stmt {
  private:
  public:
    Expr *condition;
    std::vector<Stmt *> body;
    WhileStmt() {
        this->condition = nullptr;
        this->type = WHILE_STMT;
        this->body = std::vector<Stmt *>();
    }
};

class ForStmt : public Stmt {
  private:
  public:
    Stmt *initializer;
    Stmt *condition;
    Stmt *increment;
    std::vector<Stmt *> body;
    ForStmt() {
        this->type = FOR_STMT;
        this->initializer = NULL;
        this->condition = NULL;
        this->increment = NULL;
        this->body = std::vector<Stmt *>();
    }
};

class StructStmt : public Stmt {
  private:
  public:
    Token name;
    std::vector<Variable> fieldNames;
    StructStmt() {
        this->type = STRUCT_STMT;
        this->fieldNames = std::vector<Variable>();
    }
};

class IfStmt : public Stmt {
  private:
  public:
    Expr *condition;
    std::vector<Stmt *> thenBranch;
    std::vector<Stmt *> elseBranch;
    IfStmt() {
        this->type = IF_STMT;
        this->condition = NULL;
        this->thenBranch = std::vector<Stmt *>();
        this->elseBranch = std::vector<Stmt *>();
    }
};

class FuncStmt : public Stmt {
  private:
  public:
    Token name;
    VarType returnType;
    std::vector<Variable> params;
    std::vector<Stmt *> body;
    FuncStmt() {
        this->type = FUNC_STMT;
        this->body = std::vector<Stmt *>();
        this->params = std::vector<Variable>();
    }
};

#endif
