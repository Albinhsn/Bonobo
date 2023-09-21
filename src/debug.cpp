#include "debug.h"

void debugExpression(Expr *expr) {
    if (expr == NULL) {
        printf("<null expr>");
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        printf("(");
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
        printf(")");
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
        printf("(");
        debugExpression(logicalExpr->left);
        if (logicalExpr->op == AND_LOGICAL) {
            printf(" and ");
        } else {
            printf(" or ");
        }
        debugExpression(logicalExpr->right);
        printf(")");
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
            printf("%.*s", literalExpr->literal.length,
                   literalExpr->literal.lexeme);
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
const char *debugVarType(VarType varType) {
    switch (varType) {
    case ARRAY_VAR: {
        return "array";
    }
    case BOOL_VAR: {
        return "bool";
    }
    case INT_VAR: {
        return "int";
    }
    case DOUBLE_VAR: {
        return "double";
    }
    case MAP_VAR: {
        return "map";
    }
    case STRUCT_VAR: {
        return "struct";
    }
    case STRING_VAR: {
        return "string";
    }
    default: {
        return "unknown";
    }
    }
}

void debugVariable(Variable var) {
    printf("%.*s:%s", var.name.length, var.name.lexeme, debugVarType(var.type));
}

void debugStatement(Stmt *statement) {
    switch (statement->type) {
    case VAR_STMT: {
        VarStmt *varStmt = (VarStmt *)statement;
        printf("var %.*s: %s = ", varStmt->var.name.length,
               varStmt->var.name.lexeme, debugVarType(varStmt->var.type));
        debugExpression(varStmt->initializer);
        printf(";");
        break;
    }
    case FOR_STMT: {
        ForStmt *forStmt = (ForStmt *)statement;
        printf("for(");
        if (forStmt->initializer == NULL) {
            printf(";");
        } else {
            debugStatement(forStmt->initializer);
        }
        if (forStmt->condition == NULL) {
            printf(";");
        } else {
            debugStatement(forStmt->condition);
        }
        if (forStmt->increment != NULL) {
            debugStatement(forStmt->increment);
        }
        printf("){\n");
        debugStatements(forStmt->body);
        printf("}\n");
    }
    case STRUCT_STMT: {
        StructStmt *structStmt = (StructStmt *)statement;
        break;
    }
    case RETURN_STMT: {
        ReturnStmt *returnStmt = (ReturnStmt *)statement;
        break;
    }
    case WHILE_STMT: {
        WhileStmt *whileStmt = (WhileStmt *)statement;
        printf("while");
        debugExpression(whileStmt->condition);
        printf("\n{\n");
        debugStatements(whileStmt->body);
        printf("}\n");
        break;
    }
    case IF_STMT: {
        IfStmt *ifStmt = (IfStmt *)statement;
        printf("if");
        debugExpression(ifStmt->condition);
        printf("\n{\n");
        debugStatements(ifStmt->thenBranch);
        printf("}");
        if (ifStmt->elseBranch.size()) {
            printf("else\n{\n");
            debugStatements(ifStmt->elseBranch);
            printf("}");
        }
        break;
    }
    case FUNC_STMT: {
        FuncStmt *funcStmt = (FuncStmt *)statement;
        printf("fun %.*s(", funcStmt->name.length, funcStmt->name.lexeme);
        for (int i = 0; i < funcStmt->params.size(); i++) {
            debugVariable(funcStmt->params[i]);
            if (i != funcStmt->params.size() - 1) {
                printf(",");
            }
        }
        printf(") -> %s\n{\n", debugVarType(funcStmt->returnType));
        debugStatements(funcStmt->body);
        printf("}\n");

        break;
    }
    case ASSIGN_STMT: {
        AssignStmt *assignStmt = (AssignStmt *)statement;
        printf("%.*s = ", assignStmt->name.length, assignStmt->name.lexeme);
        debugExpression(assignStmt->value);
        printf(";");
        break;
    }
    case EXPR_STMT: {
        ExprStmt *exprStmt = (ExprStmt *)statement;
        debugExpression(exprStmt->expression);
        printf(";");
        break;
    }
    }
}

void debugStatements(std::vector<Stmt *> statements) {
    for (int i = 0; i < statements.size(); i++) {
        debugStatement(statements[i]);
        printf("\n");
    }
}

void debugToken(Token *token) {
    switch (token->type) {
    case TOKEN_LEFT_PAREN: {
        printf("TOKEN_LEFT_PAREN\n");
        break;
    }
    case TOKEN_RIGHT_PAREN: {
        printf("TOKEN_RIGHT_PAREN\n");
        break;
    }
    case TOKEN_LEFT_BRACE: {
        printf("TOKEN_LEFT_BRACE\n");
        break;
    }
    case TOKEN_RIGHT_BRACE: {
        printf("TOKEN_RIGHT_BRACE\n");
        break;
    }
    case TOKEN_LEFT_BRACKET: {
        printf("TOKEN_LEFT_BRACKET\n");
        break;
    }
    case TOKEN_RIGHT_BRACKET: {
        printf("TOKEN_RIGHT_BRACKET\n");
        break;
    }
    case TOKEN_COMMA: {
        printf("TOKEN_COMMA\n");
        break;
    }
    case TOKEN_DOT: {
        printf("TOKEN_DOT\n");
        break;
    }
    case TOKEN_MINUS: {
        printf("TOKEN_MINUS\n");
        break;
    }
    case TOKEN_PLUS: {
        printf("TOKEN_PLUS\n");
        break;
    }
    case TOKEN_SEMICOLON: {
        printf("TOKEN_SEMICOLON\n");
        break;
    }
    case TOKEN_SLASH: {
        printf("TOKEN_SLASH\n");
        break;
    }
    case TOKEN_STAR: {
        printf("TOKEN_STAR\n");
        break;
    }
    case TOKEN_COLON: {
        printf("TOKEN_COLON\n");
        break;
    }
    case TOKEN_BANG: {
        printf("TOKEN_BANG\n");
        break;
    }
    case TOKEN_BANG_EQUAL: {
        printf("TOKEN_BANG_EQUAL\n");
        break;
    }
    case TOKEN_EQUAL: {
        printf("TOKEN_BANG_EQUAL\n");
        break;
    }
    case TOKEN_EQUAL_EQUAL: {
        printf("TOKEN_EQUAL_EQUAL\n");
        break;
    }
    case TOKEN_GREATER: {
        printf("TOKEN_GREATER\n");
        break;
    }
    case TOKEN_GREATER_EQUAL: {
        printf("TOKEN_GREATER_EQUAL\n");
        break;
    }
    case TOKEN_LESS: {
        printf("TOKEN_LESS\n");
        break;
    }
    case TOKEN_LESS_EQUAL: {
        printf("TOKEN_LESS_EQUAL\n");
        break;
    }
    case TOKEN_ARROW: {
        printf("TOKEN_ARROW\n");
        break;
    }
    case TOKEN_IDENTIFIER: {
        printf("TOKEN_IDENTIFIER\n");
        break;
    }
    case TOKEN_STRING: {
        printf("TOKEN_STRING\n");
        break;
    }
    case TOKEN_INT: {
        printf("TOKEN_INT\n");
        break;
    }
    case TOKEN_DOUBLE: {
        printf("TOKEN_DOUBLE\n");
        break;
    }
    case TOKEN_INT_LITERAL: {
        printf("TOKEN_INT_LITERAL\n");
        break;
    }
    case TOKEN_DOUBLE_LITERAL: {
        printf("TOKEN_DOUBLE_LITERAL\n");
        break;
    }
    case TOKEN_STR: {
        printf("TOKEN_STR\n");
        break;
    }
    case TOKEN_BOOL: {
        printf("TOKEN_BOOL\n");
        break;
    }
    case TOKEN_MAP: {
        printf("TOKEN_MAP\n");
        break;
    }
    case TOKEN_ARRAY: {
        printf("TOKEN_ARRAY\n");
        break;
    }
    case TOKEN_NIL: {
        printf("TOKEN_NIL\n");
        break;
    }
    case TOKEN_STRUCT: {
        printf("TOKEN_STRUCT\n");
        break;
    }
    case TOKEN_PRINT: {
        printf("TOKEN_PRINT\n");
        break;
    }
    case TOKEN_ELSE: {
        printf("TOKEN_ELSE\n");
        break;
    }
    case TOKEN_FALSE: {
        printf("TOKEN_FALSE\n");
        break;
    }
    case TOKEN_FOR: {
        printf("TOKEN_FOR\n");
        break;
    }
    case TOKEN_FUN: {
        printf("TOKEN_FUN\n");
        break;
    }
    case TOKEN_IF: {
        printf("TOKEN_IF\n");
        break;
    }
    case TOKEN_RETURN: {
        printf("TOKEN_RETURN\n");
        break;
    }
    case TOKEN_TRUE: {
        printf("TOKEN_TRUE\n");
        break;
    }
    case TOKEN_WHILE: {
        printf("TOKEN_WHILE\n");
        break;
    }
    case TOKEN_AND: {
        printf("TOKEN_AND\n");
        break;
    }
    case TOKEN_OR: {
        printf("TOKEN_OR\n");
        break;
    }
    case TOKEN_VAR: {
        printf("TOKEN_VAR\n");
        break;
    }
    case TOKEN_ERROR: {
        printf("TOKEN_ERROR\n");
        break;
    }
    case TOKEN_EOF: {
        printf("TOKEN_EOF\n");
        break;
    }
    }
}
