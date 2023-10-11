#ifndef STMT_HEADER
#define STMT_HEADER

#include "expr.h"

enum StatementType {
    EXPR_STMT,
    COMP_ASSIGN_STMT,
    ASSIGN_STMT,
    RETURN_STMT,
    VAR_STMT,
    WHILE_STMT,
    FOR_STMT,
    STRUCT_STMT,
    IF_STMT,
    FUNC_STMT,
    BREAK_STMT
};

class Stmt {
  private:
  public:
    StatementType type;
};

class CompAssignStmt : public Stmt {
  private:
  public:
    BinaryOp op;
    std::string name;
    Expr *right;
    CompAssignStmt(BinaryOp op, std::string name, Expr * right) {
        this->type = COMP_ASSIGN_STMT;
        this->op = op;
        this->name = name;
        this->right = right;
    }
};

class BreakStmt : public Stmt {
  private:
  public:
    BreakStmt() { this->type = BREAK_STMT; }
};

class ExprStmt : public Stmt {
  private:
  public:
    Expr *expression;
    ExprStmt(Expr * expr) {
        this->expression = expr;
        this->type = EXPR_STMT;
    }
};

class ReturnStmt : public Stmt {
  private:
  public:
    Expr *value;
    ReturnStmt(Expr * value) {
        this->value = value;
        this->type = RETURN_STMT;
    }
};

class VarStmt : public Stmt {
  private:
  public:
    Variable *var;
    Expr *initializer;
};

class AssignStmt : public Stmt {
  private:
  public:
    Expr *variable;
    Expr *value;
    AssignStmt(Expr * variable, Expr * value) {
        this->variable = variable;
        this->value = value;
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
    Expr *condition;
    Stmt *increment;
    std::vector<Stmt *> body;
    ForStmt() {
        this->type = FOR_STMT;
        this->initializer = nullptr;
        this->condition = nullptr;
        this->increment = nullptr;
        this->body = std::vector<Stmt *>();
    }
};

class StructStmt : public Stmt {
  private:
  public:
    std::string name;
    std::vector<Variable *> fields;
    StructStmt(std::string name) {
        this->name = name;
        this->type = STRUCT_STMT;
        this->fields = std::vector<Variable *>();
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
        this->condition = nullptr;
        this->thenBranch = std::vector<Stmt *>();
        this->elseBranch = std::vector<Stmt *>();
    }
};

class FuncStmt : public Stmt {
  private:
  public:
    std::string name;
    Variable *returnType;
    std::vector<Variable *> params;
    std::vector<Stmt *> body;
    FuncStmt(std::string name) {
        this->name = name;
        this->type = FUNC_STMT;
        this->body = std::vector<Stmt *>();
        this->params = std::vector<Variable *>();
    }
};

#endif
