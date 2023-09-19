#ifndef STMT_HEADER
#define STMT_HEADER

#include "expr.h"

enum StatementType {
  PRINT_STMT,
  EXPR_STMT,
  RETURN_STMT,
  VAR_STMT,
  WHILE_STMT,
  BLOCK_STMT,
  STRUCT_STMT,
  IF_STMT,
  FUNC_STMT
};

class Stmt {
private:
public:
  StatementType type;
};

class ExprStmt : Stmt {
private:
public:
  Expr expression;
};

class ReturnStmt : Stmt {
private:
public:
  Token keyword;
  Expr value;
};

class VarStmt : Stmt {
private:
public:
  Token name;
  Expr initializer;
};

class WhileStmt : Stmt {
private:
public:
  Expr condition;
  Stmt body;
};

class BlockStmt : Stmt {
private:
public:
  std::vector<Stmt> statements;
};

class StructStmt : Stmt {
private:
public:
  Token name;
  std::vector<Token> fieldNames;
  std::vector<LiteralType> fieldTypes;
};

class IfStmt : Stmt {
private:
public:
  Expr condition;
  Stmt thenBranch;
  Stmt elseBranch;
};

class FuncStmt : Stmt {
private:
public:
  Token name;
  std::vector<Token> params;
  std::vector<Stmt> body;
};

#endif
