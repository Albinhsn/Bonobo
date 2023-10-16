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
    int line;
    StatementType type;
};

class CompAssignStmt : public Stmt {
  private:
  public:
    BinaryOp op;
    std::string name;
    Expr *right;
    CompAssignStmt(BinaryOp op, std::string name, Expr *right, int line) {
        this->type = COMP_ASSIGN_STMT;
        this->op = op;
        this->name = name;
        this->right = right;
        this->line = line;
    }
};

class BreakStmt : public Stmt {
  private:
  public:
    BreakStmt(int line) {
        this->line = line;
        this->type = BREAK_STMT;
    }
};

class ExprStmt : public Stmt {
  private:
  public:
    Expr *expression;
    ExprStmt(Expr *expr, int line) {
        this->expression = expr;
        this->type = EXPR_STMT;
        this->line = line;
    }
};

class ReturnStmt : public Stmt {
  private:
  public:
    Expr *value;
    ReturnStmt(Expr *value, int line) {
        this->value = value;
        this->type = RETURN_STMT;
        this->line = line;
    }
};

class VarStmt : public Stmt {
  private:
  public:
    Variable *var;
    Expr *initializer;
    VarStmt(int line) { this->line = line; }
};

class AssignStmt : public Stmt {
  private:
  public:
    Expr *variable;
    Expr *value;
    AssignStmt(Expr *variable, Expr *value, int line) {
        this->variable = variable;
        this->value = value;
        this->type = ASSIGN_STMT;
        this->line = line;
    }
};

class WhileStmt : public Stmt {
  private:
  public:
    Expr *condition;
    std::vector<Stmt *> body;
    WhileStmt(int line) {
        this->condition = nullptr;
        this->type = WHILE_STMT;
        this->body = std::vector<Stmt *>();
        this->line = line;
    }
};

class ForStmt : public Stmt {
  private:
  public:
    Stmt *initializer;
    Expr *condition;
    Stmt *increment;
    std::vector<Stmt *> body;
    ForStmt(int line) {
        this->type = FOR_STMT;
        this->initializer = nullptr;
        this->condition = nullptr;
        this->increment = nullptr;
        this->body = std::vector<Stmt *>();
        this->line = line;
    }
};

class StructStmt : public Stmt {
  private:
  public:
    std::string name;
    std::vector<Variable *> fields;
    StructStmt(std::string name, int line) {
        this->name = name;
        this->type = STRUCT_STMT;
        this->fields = std::vector<Variable *>();
        this->line = line;
    }
};

class IfStmt : public Stmt {
  private:
  public:
    Expr *condition;
    std::vector<Stmt *> thenBranch;
    std::vector<Stmt *> elseBranch;
    IfStmt(int line) {
        this->type = IF_STMT;
        this->condition = nullptr;
        this->thenBranch = std::vector<Stmt *>();
        this->elseBranch = std::vector<Stmt *>();
        this->line = line;
    }
};

class FuncStmt : public Stmt {
  private:
  public:
    std::string name;
    Variable *returnType;
    std::vector<Variable *> params;
    std::vector<Stmt *> body;
    FuncStmt(std::string name, int line) {
        this->name = name;
        this->type = FUNC_STMT;
        this->body = std::vector<Stmt *>();
        this->params = std::vector<Variable *>();
        this->line = line;
    }
};

void freeStmt(Stmt * stmt);

#endif
