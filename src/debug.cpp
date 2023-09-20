#include "debug.h"

void debugExpression(Expr *expr) {
  if (expr == NULL) {
    printf("<null expr>");
    return;
  }
  switch (expr->type) {
  case BINARY_EXPR: {
    BinaryExpr *binaryExpr = (BinaryExpr *)expr;
    debugExpression(binaryExpr->left);
    switch (binaryExpr->op) {
    case ADD: {
      printf(" + ");
      break;
    }
    case SUB: {
      printf(" - ");
      break;
    }
    case MUL: {
      printf(" * ");
      break;
    }
    case DIV: {
      printf(" / ");
      break;
    }
    }
    debugExpression(binaryExpr->right);
    break;
  }
  case GROUPING_EXPR: {
    GroupingExpr *groupingExpr = (GroupingExpr *)expr;
    printf("(");
    debugExpression(groupingExpr->expression);
    printf(")");
    break;
  }
  case LOGICAL_EXPR: {
    LogicalExpr *logicalExpr = (LogicalExpr *)expr;
    debugExpression(logicalExpr->left);
    if (logicalExpr->op == AND_LOGICAL) {
      printf(" and ");
    } else {
      printf(" or ");
    }
    debugExpression(logicalExpr->right);
    break;
  }
  case COMPARISON_EXPR: {
    ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
    debugExpression(comparisonExpr->left);
    switch (comparisonExpr->op) {
    case LESS_COMPARISON: {
      printf(" < ");
      break;
    }
    case LESS_EQUAL_COMPARISON: {
      printf(" <= ");
      break;
    }
    case GREATER_COMPARISON: {
      printf(" > ");
      break;
    }
    case GREATER_EQUAL_COMPARISON: {
      printf(" <= ");
      break;
    }
    case EQUAL_EQUAL_COMPARISON: {
      printf(" == ");
      break;
    }
    }
    debugExpression(comparisonExpr->right);
    break;
  }
  case LITERAL_EXPR: {
    LiteralExpr *literalExpr = (LiteralExpr *)expr;
    if (literalExpr->literalType == STRING_LITERAL) {
      printf("\"%.*s\"", literalExpr->literal.length,
             literalExpr->literal.lexeme);
    } else {
      printf("%.*s", literalExpr->literal.length, literalExpr->literal.lexeme);
    }
    break;
  }
  case UNARY_EXPR: {
    UnaryExpr *unaryExpr = (UnaryExpr *)expr;
    if (unaryExpr->op == BANG_UNARY) {
      printf("!");
    } else {
      printf("-");
    }
    debugExpression(unaryExpr->right);
    break;
  }
  case VAR_EXPR: {
    VarExpr *varExpr = (VarExpr *)expr;
    printf("%.*s", varExpr->name.length, varExpr->name.lexeme);
    break;
  }
  case CALL_EXPR: {
    CallExpr *callExpr = (CallExpr *)expr;
    break;
  }
  default: {
    printf("unknown expr");
  }
  }
}

void debugStatements(Compiler *compiler) {
  for (int i = 0; i < compiler->statements.size(); i++) {
    switch (compiler->statements[i]->type) {
    case VAR_STMT: {
      VarStmt *varStmt = (VarStmt *)compiler->statements[i];
      printf("var %.*s = ", varStmt->name.length, varStmt->name.lexeme);
      debugExpression(varStmt->initializer);
      printf(";\n");
      break;
    }
    case STRUCT_STMT: {
      StructStmt *structStmt = (StructStmt *)compiler->statements[i];
      break;
    }
    case RETURN_STMT: {
      ReturnStmt *returnStmt = (ReturnStmt *)compiler->statements[i];
      break;
    }
    case WHILE_STMT: {
      WhileStmt *whileStmt = (WhileStmt *)compiler->statements[i];
      break;
    }
    case BLOCK_STMT: {
      BlockStmt *blockStmt = (BlockStmt *)compiler->statements[i];
      break;
    }
    case IF_STMT: {
      IfStmt *ifStmt = (IfStmt *)compiler->statements[i];
      break;
    }
    case FUNC_STMT: {
      FuncStmt *funcStmt = (FuncStmt *)compiler->statements[i];
      break;
    }
    case EXPR_STMT: {
      ExprStmt *exprStmt = (ExprStmt *)compiler->statements[i];
      break;
    }
    }
  }
}
