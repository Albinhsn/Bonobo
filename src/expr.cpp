#include "expr.h"

void freeExpr(Expr *expr) {
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        freeExpr(binaryExpr->right);
        freeExpr(binaryExpr->left);
        delete (binaryExpr);
        break;
    }
    case INC_EXPR: {
        IncExpr *incExpr = (IncExpr *)expr;
        freeExpr(incExpr->expr);
        delete (incExpr);
        break;
    }
    case GROUPING_EXPR: {
        GroupingExpr *groupingExpr = (GroupingExpr *)expr;
        freeExpr(groupingExpr->expression);
        delete (groupingExpr);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        freeExpr(logicalExpr->left);
        freeExpr(logicalExpr->right);
        delete (logicalExpr);
        break;
    }
    case LITERAL_EXPR: {
        delete (expr);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        freeExpr(comparisonExpr->left);
        freeExpr(comparisonExpr->right);
        delete (comparisonExpr);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        freeExpr(unaryExpr->right);
        delete (unaryExpr);
        break;
    }
    case VAR_EXPR: {
        delete (expr);
        break;
    }
    case INDEX_EXPR: {
        IndexExpr *indexExpr = (IndexExpr *)expr;
        freeExpr(indexExpr->variable);
        freeExpr(indexExpr->index);
        delete (indexExpr);
        break;
    }
    case ARRAY_EXPR: {
        ArrayExpr *arrayExpr = (ArrayExpr *)expr;
        for (auto &exp : arrayExpr->items) {
            freeExpr(exp);
        }
        delete (arrayExpr);
        break;
    }
    case MAP_EXPR: {
        MapExpr *mapExpr = (MapExpr *)expr;
        for (auto &exp : mapExpr->keys) {
            freeExpr(exp);
        }
        for (auto &exp : mapExpr->values) {
            freeExpr(exp);
        }
        delete (mapExpr);
        break;
    }
    case CALL_EXPR: {
        CallExpr *callExpr = (CallExpr *)expr;
        for (auto &exp : callExpr->arguments) {
            freeExpr(exp);
        }
        delete (callExpr);
        break;
    }
    case DOT_EXPR: {
        DotExpr *dotExpr = (DotExpr *)expr;
        freeExpr(dotExpr->name);
        delete (dotExpr);
        break;
    }
    }
}
