

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

typedef struct {
} ExprStmt;

typedef struct {
} ReturnStmt;

typedef struct {
} VarStmt;

typedef struct {
} WhileStmt;

typedef struct {
} BlockStmt;

typedef struct {
} StructStmt;

typedef struct {
} IfStmt;

typedef struct {
} FuncStmt;

typedef struct {
} PrintStmt;

typedef struct {
  StatementType type;
  union stmt {
    ExprStmt exprStmt;
    ReturnStmt returnStmt;
    VarStmt varStmt;
    WhileStmt whileStmt;
    BlockStmt blockStmt;
    StructStmt structStmt;
    IfStmt ifStmt;
    FuncStmt funcStmt;
    PrintStmt printStmt;
  };
} Statement;
