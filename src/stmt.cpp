#include "stmt.h"
#include "expr.h"

void freeStmt(Stmt *stmt) {
    switch (stmt->type) {
    case EXPR_STMT: {
        ExprStmt *exprStmt = (ExprStmt *)stmt;
        freeExpr(exprStmt->expression);
        break;
    }
    case COMP_ASSIGN_STMT: {
        CompAssignStmt *compAssignStmt = (CompAssignStmt *)stmt;
        freeExpr(compAssignStmt->right);
        break;
    }
    case ASSIGN_STMT: {
        AssignStmt *assignStmt = (AssignStmt *)stmt;
        freeExpr(assignStmt->value);
        freeExpr(assignStmt->variable);
        break;
    }
    case RETURN_STMT: {
        ReturnStmt *returnStmt = (ReturnStmt *)stmt;
        freeExpr(returnStmt->value);
        break;
    }
    case VAR_STMT: {
        VarStmt *varStmt = (VarStmt *)stmt;
        freeExpr(varStmt->initializer);
        break;
    }
    case WHILE_STMT: {
        WhileStmt *whileStmt = (WhileStmt *)stmt;
        freeExpr(whileStmt->condition);
        for (auto &s : whileStmt->body) {
            freeStmt(s);
        }
        break;
    }
    case FOR_STMT: {
        ForStmt *forStmt = (ForStmt *)stmt;
        freeExpr(forStmt->condition);
        freeStmt(forStmt->increment);
        freeStmt(forStmt->initializer);
        for (auto &s : forStmt->body) {
            freeStmt(s);
        }
        break;
    }
    case STRUCT_STMT: {
        StructStmt *structStmt = (StructStmt *)stmt;
        break;
    }
    case IF_STMT: {
        IfStmt *ifStmt = (IfStmt *)stmt;
        freeExpr(ifStmt->condition);
        for (auto &s : ifStmt->elseBranch) {
            freeStmt(s);
        }
        for (auto &s : ifStmt->thenBranch) {
            freeStmt(s);
        }
        break;
    }
    case FUNC_STMT: {
        FuncStmt *funcStmt = (FuncStmt *)stmt;
        for (auto &s : funcStmt->body) {
            freeStmt(s);
        }
    }
    case BREAK_STMT: {
        break;
    }
    }
}
