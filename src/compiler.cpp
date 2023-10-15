#include "compiler.h"
#include "common.h"
#include "debug.h"
#include "scanner.h"
#include <iostream>
#include <vector>

Compiler *compiler;
Parser *parser;
Scanner *scanner;

static void initCompiler() {
    compiler = new Compiler;
    compiler->enclosing = nullptr;
    compiler->statements = std::vector<Stmt *>();

    Variable *intVar = new Variable();
    intVar->type = INT_VAR;

    Variable *nilVar = new Variable();
    nilVar->type = NIL_VAR;

    compiler->variables = {{
        {"len", new FuncVariable("len", intVar, {new ArrayVariable("")})},
        {"printf", new FuncVariable("len", nilVar, {})},
        {"keys", new FuncVariable("keys", new ArrayVariable(""), {new MapVariable("")})},
        {"values", new FuncVariable("values", new ArrayVariable(""), {new MapVariable("")})},
    }};
}

static void endCompiler(Compiler *current) {
    compiler = current->enclosing;
    delete (current);
}

static void errorAt(const char *message, int line = 0) {

    Token *token = parser->current;
    if (line == 0) {
        line = token->line;
    }
    fprintf(stderr, "[line %d] Error", token->line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {
        //
    } else {
        fprintf(stderr, " at '%d'", token->line);
    }

    fprintf(stderr, ": %s\n", message);
    exit(1);
}

static void advance() {
    delete (parser->previous);
    parser->previous = parser->current;
    for (;;) {
        parser->current = scanToken(scanner);
        if (parser->current->type != TOKEN_ERROR) {
            break;
        }
        errorAt("error advancing");
    }
}
static void consume(TokenType type, const char *message) {
    if (parser->current->type == type) {
        advance();
        return;
    }
    errorAt(message);
}

static bool match(TokenType type) {
    if (!(parser->current->type == type)) {
        return false;
    }
    advance();
    return true;
}

static bool nextIsBinaryOp() {
    switch (parser->current->type) {
    case TOKEN_PLUS:
    case TOKEN_MINUS:
    case TOKEN_STAR:
    case TOKEN_SLASH: {
        return true;
    }
    default: {
    }
    }
    return false;
}

static VarType getVarType() {
    switch (parser->previous->type) {
    case TOKEN_INT_TYPE: {
        return INT_VAR;
    }
    case TOKEN_DOUBLE_TYPE: {
        return DOUBLE_VAR;
    }
    case TOKEN_BOOL_TYPE: {
        return BOOL_VAR;
    }
    case TOKEN_STR_TYPE: {
        return STR_VAR;
    }
    case TOKEN_ARRAY_TYPE: {
        return ARRAY_VAR;
    }
    case TOKEN_MAP_TYPE: {
        return MAP_VAR;
    }
    case TOKEN_IDENTIFIER: {
        return STRUCT_VAR;
    }
    case TOKEN_NIL: {
        return NIL_VAR;
    }
    default: {
        break;
    }
    }
    debugToken(parser->previous);
    printf("\n");
    errorAt(" Invalid varType");
    exit(1);
}

static Variable *parseVarType(Variable *var) {
    advance();
    var->type = getVarType();
    if (var->type == ARRAY_VAR) {
        ArrayVariable *arrayVar = new ArrayVariable(var->name);
        consume(TOKEN_LEFT_BRACKET, "Need array type");

        arrayVar->items = parseVarType(new Variable());
        consume(TOKEN_RIGHT_BRACKET, "Need ']' after array type");

        return arrayVar;
    } else if (var->type == MAP_VAR) {
        MapVariable *mapVar = new MapVariable(var->name);
        consume(TOKEN_LEFT_BRACKET, "Need map type");

        mapVar->keys = parseVarType(new Variable());
        consume(TOKEN_COMMA, "Need, before map values");

        mapVar->values = parseVarType(new Variable());
        consume(TOKEN_RIGHT_BRACKET, "Need ']' after map type");

        return mapVar;
    } else if (var->type == STRUCT_VAR) {
        return new StructVariable(var->name, parser->previous->lexeme, {});
    } else {
        return var;
    }
}

static Variable *parseVariable() {

    consume(TOKEN_IDENTIFIER, "Expected identifier for variable");
    Variable *var = new Variable(parser->previous->lexeme);

    consume(TOKEN_COLON, "Expected ':' after var name");
    return parseVarType(var);
}

static UnaryOp getUnaryType() {
    switch (parser->previous->type) {
    case TOKEN_BANG: {
        return BANG_UNARY;
    }
    case TOKEN_MINUS: {
        return NEG_UNARY;
    }
    default: {
        errorAt("Can't get unary op for this?");
        exit(1);
    }
    }
}

static ComparisonOp getComparisonOp() {
    switch (parser->previous->type) {
    case TOKEN_LESS: {
        return LESS_COMPARISON;
    }
    case TOKEN_LESS_EQUAL: {
        return LESS_EQUAL_COMPARISON;
    }
    case TOKEN_GREATER: {
        return GREATER_COMPARISON;
    }
    case TOKEN_GREATER_EQUAL: {
        return GREATER_EQUAL_COMPARISON;
    }
    case TOKEN_EQUAL_EQUAL: {
        return EQUAL_EQUAL_COMPARISON;
    }
    default: {
        errorAt("unable to get logical type");
        exit(1);
    }
    }
}

static LogicalOp getLogicalOp() {
    if (parser->previous->type == TOKEN_AND) {
        return AND_LOGICAL;
    } else if (parser->previous->type == TOKEN_OR) {
        return OR_LOGICAL;
    }
    errorAt("Unknown logical op?");
    exit(1);
}

static LiteralType getLiteralType() {
    switch (parser->previous->type) {
    case TOKEN_INT_LITERAL: {
        return INT_LITERAL;
    }
    case TOKEN_DOUBLE_LITERAL: {
        return DOUBLE_LITERAL;
    }
    case TOKEN_TRUE: {
        return BOOL_LITERAL;
    }
    case TOKEN_FALSE: {
        return BOOL_LITERAL;
    }
    case TOKEN_STR_LITERAL: {
        return STR_LITERAL;
    }
    default: {
        break;
    }
    }
    errorAt("Unable to get literal type");
    exit(1);
}

static void literal(Expr *&expr) {
    if (expr == nullptr) {
        expr = new LiteralExpr(parser->previous->lexeme, getLiteralType(), parser->previous->line);
    } else {
        switch (expr->type) {
        case BINARY_EXPR: {
            BinaryExpr *binaryExpr = (BinaryExpr *)expr;
            literal(binaryExpr->right);
            break;
        }
        case LOGICAL_EXPR: {
            LogicalExpr *logicalExpr = (LogicalExpr *)expr;
            literal(logicalExpr->right);
            break;
        }
        case COMPARISON_EXPR: {
            ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
            literal(comparisonExpr->right);
            break;
        }
        case UNARY_EXPR: {
            UnaryExpr *unaryExpr = (UnaryExpr *)expr;
            literal(unaryExpr->right);
            break;
        }
        case CALL_EXPR: {
            CallExpr *callExpr = (CallExpr *)expr;
            literal(callExpr->arguments.back());
            break;
        }
        default: {
            errorAt("Can't add literal to expr");
            break;
        }
        }
    }
}

static void unary(Expr *&expr) {
    if (expr == nullptr) {
        UnaryExpr *unaryExpr = new UnaryExpr(getUnaryType(), parser->previous->line);

        expr = unaryExpr;
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        unary(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        unary(logicalExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        unary(comparisonExpr->right);
        break;
    }
    default: {
        errorAt("Can't add unary to expr");
    }
    }
}

static void grouping(Expr *&expr) {
    if (expr == nullptr) {
        expr = new GroupingExpr(expression(expr), parser->previous->line);
        consume(TOKEN_RIGHT_PAREN, "Grouping wasn't closed");

        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        grouping(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        grouping(logicalExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        grouping(comparisonExpr->right);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        grouping(unaryExpr->right);
        break;
    }
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        CallExpr *callExpr = new CallExpr(varExpr->name, parser->previous->line);
        if (!match(TOKEN_RIGHT_PAREN)) {
            while (true) {
                callExpr->arguments.push_back(expression(nullptr));
                if (match(TOKEN_RIGHT_PAREN)) {
                    break;
                }
                consume(TOKEN_COMMA, "Expect ',' after func param");
            }
        }
        expr = callExpr;
        break;
    }
    default: {
        errorAt("Can't add grouping to this?");
    }
    }
}

static BinaryOp getBinaryOp(Token *token) {
    switch (token->type) {
    case TOKEN_PLUS: {
        return ADD;
    }
    case TOKEN_MINUS: {
        return SUB;
    }
    case TOKEN_SLASH: {
        return DIV;
    }
    case TOKEN_STAR: {
        return MUL;
    }
    default: {
        errorAt("Unknown binaryOp type");
        exit(1);
    }
    }
}

static void operation(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("What can't op without expr");
    }
    BinaryOp op = getBinaryOp(parser->previous);
    switch (expr->type) {
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        operation(comparisonExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        operation(logicalExpr->right);
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;

        if (op >= MUL && binaryExpr->op <= SUB) {
            if (binaryExpr->right != nullptr && binaryExpr->right->type == BINARY_EXPR) {
                operation(binaryExpr->right);
            } else {

                binaryExpr->right = new BinaryExpr(binaryExpr->right, op, binaryExpr->line);
                expr = binaryExpr;
            }
        } else {
            expr = new BinaryExpr(binaryExpr, op, binaryExpr->line);
        }
        break;
    }
    default: {
        expr = new BinaryExpr(expr, op, expr->line);
        break;
    }
    }
}

static void comparison(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("Unable to add logical to empty expr");
    }
    if (expr->type == LOGICAL_EXPR) {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        comparison(logicalExpr->right);
    } else {
        expr = new ComparisonExpr(expr, getComparisonOp(), expr->line);
    }
}

static void identifier(Expr *&expr) {
    if (expr == nullptr) {
        expr = new VarExpr(parser->previous->lexeme, parser->previous->line);
        return;
    }
    switch (expr->type) {
    case INC_EXPR: {
        IncExpr *incExpr = (IncExpr *)expr;
        if (incExpr->expr == nullptr) {
            identifier(incExpr->expr);
            break;
        }
        errorAt("Can't add identifier to inc expr");
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        identifier(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        identifier(logicalExpr->right);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        identifier(unaryExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        identifier(comparisonExpr->right);
        break;
    }
    default: {
        printf("%d ", expr->type);
        printf("woopsie no identifier yet\n");
        exit(1);
    }
    }
}

static void dot(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("Can't add '.' to nothing?\n");
        return;
    }
    switch (expr->type) {
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        consume(TOKEN_IDENTIFIER, "Expect identifier after '.'");
        DotExpr *dotExpr = new DotExpr(varExpr, parser->previous->lexeme, parser->previous->line);
        expr = dotExpr;
        break;
    }
    case CALL_EXPR: {
        CallExpr *callExpr = (CallExpr *)expr;
        consume(TOKEN_IDENTIFIER, "Expect identifier after '.'");
        DotExpr *dotExpr = new DotExpr(callExpr, parser->previous->lexeme, parser->previous->line);
        expr = dotExpr;
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        identifier(binaryExpr->right);
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        identifier(logicalExpr->right);
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        identifier(unaryExpr->right);
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        identifier(comparisonExpr->right);
        break;
    }
    case INDEX_EXPR: {
        IndexExpr *indexExpr = (IndexExpr *)expr;
        consume(TOKEN_IDENTIFIER, "Expect identifier after '.'");
        DotExpr *dotExpr = new DotExpr(indexExpr, parser->previous->lexeme, indexExpr->line);
        expr = dotExpr;
        break;
    }
    default: {
        printf("%d ", expr->type);
        printf("woopsie no dot yet\n");
        exit(1);
    }
    }
}

static bool isChildUnary(Expr *&expr) {
    if (expr == nullptr) {
        return true;
    }
    if (expr->type == BINARY_EXPR) {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        return isChildUnary(binaryExpr->right);
    }

    return false;
}
static bool isEmptyChildUnary(Expr *&expr) {
    if (expr == nullptr) {
        return false;
    } else if (expr->type == UNARY_EXPR) {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        return unaryExpr->right == nullptr ? false : true;
    } else if (expr->type == BINARY_EXPR) {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        return isEmptyChildUnary(binaryExpr->right);
    }

    return false;
}

static void plus(Expr *&expr) {
    if (expr == nullptr) {
        expr = new UnaryExpr(PLUS_UNARY, parser->previous->line);
        return;
    }
    switch (expr->type) {
    case UNARY_EXPR: {
        expr = new IncExpr(nullptr, INC, expr->line);
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        if (binaryExpr->right == nullptr && binaryExpr->op == ADD && binaryExpr->left->type == VAR_EXPR) {
            expr = new IncExpr(binaryExpr->left, INC, binaryExpr->line);
        } else if (isChildUnary(binaryExpr->right) || isEmptyChildUnary(binaryExpr->right)) {
            plus(binaryExpr->right);
            expr = binaryExpr;
        } else {
            expr = new BinaryExpr(expr, ADD, expr->line);
        }
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        plus(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        plus(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    default: {
        expr = new BinaryExpr(expr, ADD, expr->line);
        break;
    }
    }
}
static void minus(Expr *&expr) {
    if (expr == nullptr) {
        expr = new UnaryExpr(NEG_UNARY, parser->previous->line);
        return;
    }
    switch (expr->type) {
    case UNARY_EXPR: {
        expr = new IncExpr(nullptr, DEC, expr->line);
        break;
    }
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        if (binaryExpr->right == nullptr && binaryExpr->op == SUB && binaryExpr->left->type == VAR_EXPR) {
            expr = new IncExpr(binaryExpr->left, DEC, binaryExpr->line);
        } else if (isChildUnary(binaryExpr->right) || isEmptyChildUnary(binaryExpr->right)) {
            minus(binaryExpr->right);
            expr = binaryExpr;
        } else {
            expr = new BinaryExpr(expr, SUB, expr->line);
        }
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        minus(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        minus(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    default: {
        expr = new BinaryExpr(expr, SUB, expr->line);
        break;
    }
    }
}

static void index(Expr *&expr) {
    if (expr == nullptr) {
        expr = arrayDeclaration();
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        index(binaryExpr->right);
        expr = binaryExpr;
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        index(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        index(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        index(unaryExpr->right);
        expr = unaryExpr;
        break;
    }
    default: {
        expr = new IndexExpr(expr, expression(nullptr), expr->line);
        consume(TOKEN_RIGHT_BRACKET, "Expect ']' after index");
        break;
    }
    }
}

static void logical(Expr *&expr) {
    if (expr == nullptr) {
        errorAt("Can't add logical op to empty expr?");
    }

    LogicalOp op = getLogicalOp();

    if (expr->type == LOGICAL_EXPR) {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        if (logicalExpr->op < op) {
            logicalExpr->right = new LogicalExpr(logicalExpr->right, op, logicalExpr->line);
            expr = logicalExpr;
        } else {
            expr = new LogicalExpr(logicalExpr, op, logicalExpr->line);
        }
    } else {
        expr = new LogicalExpr(expr, op, expr->line);
    }
}

static void mapExpression(Expr *&expr) {
    if (expr == nullptr) {
        expr = mapDeclaration();
        return;
    }
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        mapExpression(binaryExpr->right);
        expr = binaryExpr;
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        mapExpression(logicalExpr->right);
        expr = logicalExpr;
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        mapExpression(comparisonExpr->right);
        expr = comparisonExpr;
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        mapExpression(unaryExpr->right);
        expr = unaryExpr;
        break;
    }
    default: {
        errorAt("Don't know how to parse this expr (mapExpression)");
    }
    }
}

static Expr *expression(Expr *expr) {
    while (parser->current->type != TOKEN_SEMICOLON && parser->current->type != TOKEN_RIGHT_PAREN &&
           parser->current->type != TOKEN_RIGHT_BRACKET && parser->current->type != TOKEN_RIGHT_BRACE &&
           parser->current->type != TOKEN_COLON && parser->current->type != TOKEN_COMMA) {
        advance();

        // debugExpression(expr);
        // printf("\n");
        // debugToken(parser->previous);
        // printf("\n");

        switch (parser->previous->type) {
        case TOKEN_INT_LITERAL: {
            literal(expr);
            break;
        }
        case TOKEN_DOUBLE_LITERAL: {
            literal(expr);
            break;
        }
        case TOKEN_STR_LITERAL: {
            literal(expr);
            break;
        }
        case TOKEN_IDENTIFIER: {
            identifier(expr);
            break;
        }
        case TOKEN_TRUE: {
            literal(expr);
            break;
        }
        case TOKEN_FALSE: {
            literal(expr);
            break;
        }
        case TOKEN_PLUS: {
            plus(expr);
            break;
        }
        case TOKEN_MINUS: {
            minus(expr);
            break;
        }
        case TOKEN_STAR: {
            operation(expr);
            break;
        }
        case TOKEN_SLASH: {
            operation(expr);
            break;
        }
        case TOKEN_BANG: {
            unary(expr);
            break;
        }
        case TOKEN_LESS: {
            comparison(expr);
            break;
        }
        case TOKEN_LESS_EQUAL: {
            comparison(expr);
            break;
        }
        case TOKEN_GREATER: {
            comparison(expr);
            break;
        }
        case TOKEN_GREATER_EQUAL: {
            comparison(expr);
            break;
        }
        case TOKEN_EQUAL_EQUAL: {
            comparison(expr);
            break;
        }
        case TOKEN_LEFT_PAREN: {
            grouping(expr);
            break;
        }
        case TOKEN_LEFT_BRACKET: {
            index(expr);
            break;
        }
        case TOKEN_LEFT_BRACE: {
            mapExpression(expr);
            break;
        }
        case TOKEN_AND: {
            logical(expr);
            break;
        }
        case TOKEN_OR: {
            logical(expr);
            break;
        }
        case TOKEN_DOT: {
            dot(expr);
            break;
        }
        default: {
            errorAt("Can't parse expr with this token");
        }
        }
    }
    return expr;
}

static Expr *arrayDeclaration() {
    ArrayExpr *arrayExpr = new ArrayExpr(parser->previous->line);
    if (parser->current->type != TOKEN_RIGHT_BRACKET) {
        do {
            arrayExpr->items.push_back(expression(nullptr));
        } while (match(TOKEN_COMMA));
    }
    consume(TOKEN_RIGHT_BRACKET, "Expect ']' after array declarations.");
    return arrayExpr;
}

static Expr *mapDeclaration() {
    MapExpr *mapExpr = new MapExpr(parser->previous->line);
    if (parser->current->type != TOKEN_RIGHT_BRACE) {
        do {
            mapExpr->keys.push_back(expression(nullptr));
            consume(TOKEN_COLON, "Expect colon between key and value");
            mapExpr->values.push_back(expression(nullptr));
        } while (match(TOKEN_COMMA));
    }

    consume(TOKEN_RIGHT_BRACE, "Expect '}' after map items.");
    return mapExpr;
}

static Stmt *varDeclaration() {
    VarStmt *varStmt = new VarStmt(parser->previous->line);
    varStmt->type = VAR_STMT;
    varStmt->var = parseVariable();
    consume(TOKEN_EQUAL, "Expected assignment at var declaration");

    varStmt->initializer = expression(nullptr);
    // Change this xD
    if (varStmt->initializer->type == ARRAY_EXPR && varStmt->var->type == ARRAY_VAR) {
        ArrayExpr *expr = (ArrayExpr *)varStmt->initializer;
        ArrayVariable *var = (ArrayVariable *)varStmt->var;
        expr->itemType = var->items;
        if (expr->itemType->type == ARRAY_VAR) {
            while (true) {
                if (var->items->type == ARRAY_VAR) {
                    ArrayVariable *arrayVar = (ArrayVariable *)var->items;
                    for (int i = 0; i < expr->items.size(); ++i) {
                        ArrayExpr *item = (ArrayExpr *)expr->items[i];
                        item->itemType = arrayVar->items;
                    }
                    expr = (ArrayExpr *)expr->items.back();
                    var = arrayVar;
                } else {
                    break;
                }
            }
        }
    } else if (varStmt->initializer->type == MAP_EXPR) {
        MapExpr *mapExpr = (MapExpr *)varStmt->initializer;
        mapExpr->mapVar = varStmt->var;
    }

    consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration");

    return (Stmt *)varStmt;
}
static Stmt *variableStatement(std::string ident) {
    if (match(TOKEN_EQUAL)) {
        return new AssignStmt(new VarExpr(ident, parser->previous->line), expression(nullptr), parser->previous->line);
    } else if (nextIsBinaryOp()) {
        advance();
        BinaryOp op = getBinaryOp(parser->previous);
        if (match(TOKEN_EQUAL)) {
            return new CompAssignStmt(op, ident, expression(nullptr), parser->previous->line);
        } else {
            return new ExprStmt(
                expression(new BinaryExpr(new VarExpr(ident, parser->previous->line), op, parser->previous->line)),
                parser->previous->line);
        }

    } else if (match(TOKEN_LEFT_BRACKET)) {
        IndexExpr *indexExpr =
            new IndexExpr(new VarExpr(ident, parser->previous->line), expression(nullptr), parser->previous->line);
        consume(TOKEN_RIGHT_BRACKET, "Expected ']' after index");
        while (match(TOKEN_LEFT_BRACKET)) {
            indexExpr = new IndexExpr(indexExpr, expression(nullptr), parser->previous->line);
            consume(TOKEN_RIGHT_BRACKET, "Expected ']' after index");
        }
        if (match(TOKEN_EQUAL)) {
            return new AssignStmt(indexExpr, expression(nullptr), parser->previous->line);
        } else if (match(TOKEN_DOT)) {
            consume(TOKEN_IDENTIFIER, "Expect identifier after '.'");
            DotExpr *dotExpr = new DotExpr(indexExpr, parser->previous->lexeme, indexExpr->line);
            if (match(TOKEN_EQUAL)) {
                return new AssignStmt(dotExpr, expression(nullptr), dotExpr->line);
            }
            return new ExprStmt(expression(dotExpr), dotExpr->line);
        }
        return new ExprStmt(expression(indexExpr), indexExpr->line);
    } else if (match(TOKEN_DOT)) {
        consume(TOKEN_IDENTIFIER, "Expect identifier after '.'");
        DotExpr *dotExpr =
            new DotExpr(new VarExpr(ident, parser->previous->line), parser->previous->lexeme, parser->previous->line);
        if (match(TOKEN_EQUAL)) {
            return new AssignStmt(dotExpr, expression(nullptr), dotExpr->line);
        }
        return new ExprStmt(expression(dotExpr), dotExpr->line);
    }
    return new ExprStmt(expression(new VarExpr(ident, parser->previous->line)), parser->previous->line);
}

static Stmt *expressionStatement() {
    if (match(TOKEN_IDENTIFIER)) {
        return variableStatement(parser->previous->lexeme);
    }
    return new ExprStmt(expression(nullptr), parser->current->line);
}

static Stmt *forStatement() {
    ForStmt *forStmt = new ForStmt(parser->previous->line);
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");

    if (match(TOKEN_SEMICOLON)) {
    } else if (match(TOKEN_VAR)) {
        forStmt->initializer = varDeclaration();
    } else {
        forStmt->initializer = expressionStatement();
        consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    }

    if (!match(TOKEN_SEMICOLON)) {
        forStmt->condition = expression(nullptr);
        consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
    }

    if (!match(TOKEN_RIGHT_PAREN)) {
        forStmt->increment = expressionStatement();
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");
    }

    consume(TOKEN_LEFT_BRACE, "Expect '{' after 'for()'");
    while (!match(TOKEN_RIGHT_BRACE)) {
        forStmt->body.push_back(declaration());
    }
    return forStmt;
}

static Stmt *ifStatement() {
    IfStmt *ifStmt = new IfStmt(parser->previous->line);
    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");

    grouping(ifStmt->condition);

    consume(TOKEN_LEFT_BRACE, "Expect '{' after condition.");
    while (!match(TOKEN_RIGHT_BRACE)) {
        ifStmt->thenBranch.push_back(declaration());
    }

    if (match(TOKEN_ELSE)) {
        consume(TOKEN_LEFT_BRACE, "Expect '{' after else");
        while (!match(TOKEN_RIGHT_BRACE)) {
            ifStmt->elseBranch.push_back(declaration());
        }
    }
    return ifStmt;
}

static Stmt *returnStatement() {
    ReturnStmt *returnStmt = new ReturnStmt(expression(nullptr), parser->previous->line);
    consume(TOKEN_SEMICOLON, "Expect ';' after expressionStatement");

    return returnStmt;
}

static Stmt *whileStatement() {
    WhileStmt *whileStmt = new WhileStmt(parser->previous->line);

    consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
    grouping(whileStmt->condition);

    consume(TOKEN_LEFT_BRACE, "Expect '{' after while()");
    while (!match(TOKEN_RIGHT_BRACE)) {
        whileStmt->body.push_back(declaration());
    }
    return whileStmt;
}

static Stmt *structDeclaration() {
    int line = parser->previous->line;
    consume(TOKEN_IDENTIFIER, "Expect struct name");
    StructStmt *structStmt = new StructStmt(parser->previous->lexeme, line);
    consume(TOKEN_LEFT_BRACE, "Expect '{' before struct body.");
    while (!match(TOKEN_RIGHT_BRACE)) {
        structStmt->fields.push_back(parseVariable());
        consume(TOKEN_SEMICOLON, "Expect semicolon after struct field identifier");
    }
    consume(TOKEN_SEMICOLON, "Expect ';' after struct end.");
    return structStmt;
}

static Stmt *funDeclaration() {
    int line = parser->previous->line;
    if (compiler->variables.size() != 1) {
        errorAt("Can only declare functions in outer scope", line);
    }
    consume(TOKEN_IDENTIFIER, "Need function name in func declaration");
    FuncStmt *funcStmt = new FuncStmt(parser->previous->lexeme, line);

    consume(TOKEN_LEFT_PAREN, "Expect '(' after func name");
    if (!match(TOKEN_RIGHT_PAREN)) {
        funcStmt->params.push_back(parseVariable());
        while (match(TOKEN_COMMA)) {
            funcStmt->params.push_back(parseVariable());
        }
        consume(TOKEN_RIGHT_PAREN, "Expect ')' after func params");
    }

    consume(TOKEN_ARROW, "Expect '->' after func params");

    funcStmt->returnType = parseVarType(new Variable());

    if (funcStmt->returnType->type == STRUCT_VAR) {
        funcStmt->returnType->name = parser->previous->lexeme;
    }

    consume(TOKEN_LEFT_BRACE, "Expect '{' after returntype in func declaration");
    while (!match(TOKEN_RIGHT_BRACE)) {
        funcStmt->body.push_back(declaration());
    }
    return funcStmt;
}

static Stmt *statement() {
    if (match(TOKEN_FOR)) {
        return forStatement();
    } else if (match(TOKEN_IF)) {
        return ifStatement();
    } else if (match(TOKEN_RETURN)) {
        return returnStatement();
    } else if (match(TOKEN_WHILE)) {
        return whileStatement();
    } else if (match(TOKEN_BREAK)) {
        BreakStmt *stmt = new BreakStmt(parser->previous->line);
        consume(TOKEN_SEMICOLON, "Expect ';' after break");
        return stmt;
    } else {
        Stmt *stmt = expressionStatement();
        consume(TOKEN_SEMICOLON, "Expect ';' after expressionStatement");
        return stmt;
    }
}

static Stmt *declaration() {
    if (match(TOKEN_FUN)) {
        return funDeclaration();
    } else if (match(TOKEN_VAR)) {
        return varDeclaration();
    } else if (match(TOKEN_STRUCT_TYPE)) {
        return structDeclaration();
    } else {
        return statement();
    }
}

static void initParser() {
    parser = (Parser *)malloc(sizeof(Parser));
    parser->current = nullptr;
    parser->previous = nullptr;
}

static void checkParamMatch(std::vector<Variable *> vars, std::vector<Expr *> exprs, int line) {
    if (vars.size() != exprs.size()) {
        errorAt("Number of params doesn't match", line);
    }
    for (int i = 0; i < vars.size(); i++) {
        if (vars[i]->type != exprs[i]->evaluatesTo->type && vars[i]->type != ARRAY_VAR && exprs[i]->evaluatesTo->type != STR_VAR) {
            errorAt("Mismatch in call params types", line);
        }
    }
}

static void fixExprEvaluatesToExpr(Expr *expr) {
    switch (expr->type) {
    case BINARY_EXPR: {
        BinaryExpr *binaryExpr = (BinaryExpr *)expr;
        fixExprEvaluatesToExpr(binaryExpr->left);
        fixExprEvaluatesToExpr(binaryExpr->right);

        Variable *leftEvaluation = binaryExpr->left->evaluatesTo;
        Variable *rightEvaluation = binaryExpr->right->evaluatesTo;

        if (leftEvaluation->type == rightEvaluation->type) {
            binaryExpr->evaluatesTo = leftEvaluation;
        }

        else if (leftEvaluation->type == DOUBLE_VAR && rightEvaluation->type == INT_VAR ||
                 leftEvaluation->type == INT_VAR && rightEvaluation->type == DOUBLE_VAR) {
            Variable *var = new Variable();
            var->type = DOUBLE_VAR;
            binaryExpr->evaluatesTo = var;
        } else {
            errorAt("Unable to do binaryExpr with these types", binaryExpr->line);
        }

        break;
    }
    case INC_EXPR: {
        IncExpr *incExpr = (IncExpr *)expr;
        fixExprEvaluatesToExpr(incExpr->expr);

        VarType varType = incExpr->expr->evaluatesTo->type;
        if (varType != INT_VAR && varType != DOUBLE_VAR) {
            errorAt("Unable to do inc/dec expression on this type", incExpr->line);
        }

        incExpr->evaluatesTo = incExpr->expr->evaluatesTo;
        break;
    }
    case GROUPING_EXPR: {
        GroupingExpr *groupingExpr = (GroupingExpr *)expr;
        fixExprEvaluatesToExpr(groupingExpr->expression);
        groupingExpr->evaluatesTo = groupingExpr->expression->evaluatesTo;
        break;
    }
    case LOGICAL_EXPR: {
        LogicalExpr *logicalExpr = (LogicalExpr *)expr;
        fixExprEvaluatesToExpr(logicalExpr->left);
        fixExprEvaluatesToExpr(logicalExpr->right);
        if (logicalExpr->left->evaluatesTo->type != logicalExpr->right->evaluatesTo->type) {
            errorAt("Can't do logical expression with different types", logicalExpr->line);
        }
        logicalExpr->evaluatesTo = new Variable();
        logicalExpr->evaluatesTo->type = BOOL_VAR;
        break;
    }
    case LITERAL_EXPR: {
        LiteralExpr *literalExpr = (LiteralExpr *)expr;
        literalExpr->evaluatesTo = new Variable();
        switch (literalExpr->literalType) {
        case DOUBLE_LITERAL: {
            literalExpr->evaluatesTo->type = DOUBLE_VAR;
            break;
        }
        case INT_LITERAL: {
            literalExpr->evaluatesTo->type = INT_VAR;
            break;
        }
        case BOOL_LITERAL: {
            literalExpr->evaluatesTo->type = BOOL_VAR;
            break;
        }
        case STR_LITERAL: {
            literalExpr->evaluatesTo->type = STR_VAR;
            break;
        }
        }
        break;
    }
    case COMPARISON_EXPR: {
        ComparisonExpr *comparisonExpr = (ComparisonExpr *)expr;
        fixExprEvaluatesToExpr(comparisonExpr->left);
        fixExprEvaluatesToExpr(comparisonExpr->right);
        if (comparisonExpr->left->evaluatesTo->type != comparisonExpr->right->evaluatesTo->type) {
            errorAt("Can't do logical expression with different types", comparisonExpr->line);
        }
        comparisonExpr->evaluatesTo = new Variable();
        comparisonExpr->evaluatesTo->type = BOOL_VAR;
        break;
    }
    case UNARY_EXPR: {
        UnaryExpr *unaryExpr = (UnaryExpr *)expr;
        fixExprEvaluatesToExpr(unaryExpr->right);

        Variable *evalsTo = unaryExpr->right->evaluatesTo;
        if (unaryExpr->op == BANG_UNARY && evalsTo->type != BOOL_VAR) {
            errorAt("Can't do '!' expr with non bool", unaryExpr->line);
        }
        if (unaryExpr->op == NEG_UNARY && evalsTo->type != INT_VAR && evalsTo->type != DOUBLE_VAR) {
            errorAt("Can't do '-' expr with non bool", unaryExpr->line);
        }

        unaryExpr->evaluatesTo = evalsTo;
        break;
    }
    case INDEX_EXPR: {
        IndexExpr *indexExpr = (IndexExpr *)expr;
        fixExprEvaluatesToExpr(indexExpr->index);
        fixExprEvaluatesToExpr(indexExpr->variable);

        Variable *variable = indexExpr->variable->evaluatesTo;
        Variable *evalsTo = indexExpr->index->evaluatesTo;
        if (variable == nullptr) {
            errorAt("var was nullptr?", 0);
        }
        if (variable->type == MAP_VAR) {
            MapVariable *mapVar = (MapVariable *)variable;
            if (evalsTo->type != mapVar->keys->type) {
                errorAt("Invalid key type", indexExpr->line);
            }
            indexExpr->evaluatesTo = mapVar->values;
        } else if (variable->type == ARRAY_VAR) {
            ArrayVariable *arrayVar = (ArrayVariable *)variable;
            if (evalsTo->type != INT_VAR) {
                errorAt("Invalid key type, can only index array with int", indexExpr->line);
            }
            indexExpr->evaluatesTo = arrayVar->items;
        } else if (variable->type == STR_VAR) {
            if (evalsTo->type != INT_VAR) {
                errorAt("Invalid key type, can only index str with int", indexExpr->line);
            }
            indexExpr->evaluatesTo = variable;
        } else {
            debugVariable(variable);
            errorAt("\nCan't index this type?", indexExpr->line);
        }
        break;
    }
    case ARRAY_EXPR: {
        ArrayExpr *arrayExpr = (ArrayExpr *)expr;
        for (int i = 0; i < arrayExpr->items.size(); ++i) {
            fixExprEvaluatesToExpr(arrayExpr->items[i]);
            if (arrayExpr->itemType == nullptr) {
                arrayExpr->itemType = arrayExpr->items[i]->evaluatesTo;
            }

            if (arrayExpr->items[i]->evaluatesTo->type != arrayExpr->itemType->type) {
                errorAt("Mismatch in array item type", arrayExpr->line);
            }
        }
        ArrayVariable *arrayVar = new ArrayVariable("");
        arrayVar->items = arrayExpr->itemType;
        arrayExpr->evaluatesTo = arrayVar;
        break;
    }
    case MAP_EXPR: {
        MapExpr *mapExpr = (MapExpr *)expr;
        mapExpr->evaluatesTo = mapExpr->mapVar;
        MapVariable *mapVar = (MapVariable *)mapExpr->mapVar;
        for (auto &item : mapExpr->keys) {
            fixExprEvaluatesToExpr(item);
            if (mapVar->keys->type != item->evaluatesTo->type) {
                errorAt("Mismatch in key for map expression", mapExpr->line);
            }
        }
        for (auto &item : mapExpr->values) {
            fixExprEvaluatesToExpr(item);
            if (mapVar->values->type != item->evaluatesTo->type) {
                errorAt("Mismatch in key for map expression", mapExpr->line);
            }
        }
        break;
    }
    case CALL_EXPR: {
        CallExpr *callExpr = (CallExpr *)expr;
        for (auto &arg : callExpr->arguments) {
            fixExprEvaluatesToExpr(arg);
        }
        // check whether params match
        // figureut return type aka evaluatesto
        for (auto &scope : compiler->variables) {
            if (scope.count(callExpr->callee)) {
                Variable *var = scope[callExpr->callee];
                switch (var->type) {
                case STRUCT_VAR: {
                    StructVariable *structVar = (StructVariable *)var;
                    if (structVar->structName == callExpr->callee) {
                        checkParamMatch(structVar->fields, callExpr->arguments, callExpr->line);
                        callExpr->evaluatesTo = var;
                        return;
                    }
                    break;
                }
                case FUNC_VAR: {
                    FuncVariable *funcVar = (FuncVariable *)var;
                    if (funcVar->name == callExpr->callee) {
                        if (funcVar->name != "printf") {
                            checkParamMatch(funcVar->params, callExpr->arguments, callExpr->line);
                        }
                        callExpr->evaluatesTo = funcVar->returnType;
                        return;
                    }
                    break;
                }
                default: {
                    if (var->name == callExpr->callee) {
                        printf("\n%d\n", var->type);
                        errorAt(("Can't call this variable - " + var->name).c_str(), callExpr->line);
                    }
                }
                }
            }
        }
        break;
    }
    case DOT_EXPR: {
        DotExpr *dotExpr = (DotExpr *)expr;
        fixExprEvaluatesToExpr(dotExpr->name);
        Variable *var = dotExpr->name->evaluatesTo;

        switch (var->type) {
        case STRUCT_VAR: {
            StructVariable *structVar = (StructVariable *)var;
            for (auto &variable : structVar->fields) {
                if (variable->name == dotExpr->field) {
                    dotExpr->evaluatesTo = variable;
                    break;
                }
            }
            break;
        }
        case FUNC_VAR: {
            FuncVariable *funcVar = (FuncVariable *)var;
            dotExpr->evaluatesTo = funcVar->returnType;
            break;
        }
        default: {
        }
        }
        break;
    }
    case VAR_EXPR: {
        VarExpr *varExpr = (VarExpr *)expr;
        for (int i = compiler->variables.size() - 1; i >= 0; i--) {
            std::map<std::string, Variable *> scope = compiler->variables.back();
            if (scope.count(varExpr->name)) {
                varExpr->evaluatesTo = scope[varExpr->name];
                return;
            }
            errorAt(("Unable to find variable " + varExpr->name).c_str(), varExpr->line);
            break;
        }
    }
    }
}

static void fixExprEvaluatesToStmt(Stmt *stmt) {
    switch (stmt->type) {
    case EXPR_STMT: {
        ExprStmt *exprStmt = (ExprStmt *)stmt;
        fixExprEvaluatesToExpr(exprStmt->expression);
        break;
    }
    case COMP_ASSIGN_STMT: {
        CompAssignStmt *compAssignStmt = (CompAssignStmt *)stmt;
        fixExprEvaluatesToExpr(compAssignStmt->right);
        break;
    }
    case ASSIGN_STMT: {
        AssignStmt *assignStmt = (AssignStmt *)stmt;
        fixExprEvaluatesToExpr(assignStmt->value);
        fixExprEvaluatesToExpr(assignStmt->variable);
        break;
    }
    case RETURN_STMT: {
        ReturnStmt *returnStmt = (ReturnStmt *)stmt;
        fixExprEvaluatesToExpr(returnStmt->value);
        break;
    }
    case VAR_STMT: {
        VarStmt *varStmt = (VarStmt *)stmt;
        if (compiler->variables.back().count(varStmt->var->name)) {
            errorAt(("Can't redeclare a variable in the same scope - " + varStmt->var->name).c_str(), varStmt->line);
        }
        fixExprEvaluatesToExpr(varStmt->initializer);
        compiler->variables.back()[varStmt->var->name] = varStmt->var;
        break;
    }
    case WHILE_STMT: {
        WhileStmt *whileStmt = (WhileStmt *)stmt;
        fixExprEvaluatesToExpr(whileStmt->condition);
        for (auto &bodyStmt : whileStmt->body) {
            fixExprEvaluatesToStmt(bodyStmt);
        }
        break;
    }
    case FOR_STMT: {
        ForStmt *forStmt = (ForStmt *)stmt;
        fixExprEvaluatesToStmt(forStmt->initializer);
        fixExprEvaluatesToExpr(forStmt->condition);
        fixExprEvaluatesToStmt(forStmt->increment);
        for (auto &bodyStmt : forStmt->body) {
            fixExprEvaluatesToStmt(bodyStmt);
        }
        break;
    }
    case IF_STMT: {
        IfStmt *ifStmt = (IfStmt *)stmt;
        fixExprEvaluatesToExpr(ifStmt->condition);
        for (auto &bodyStmt : ifStmt->thenBranch) {
            fixExprEvaluatesToStmt(bodyStmt);
        }
        for (auto &bodyStmt : ifStmt->elseBranch) {
            fixExprEvaluatesToStmt(bodyStmt);
        }
        break;
    }
    case FUNC_STMT: {
        FuncStmt *funcStmt = (FuncStmt *)stmt;
        compiler->variables.back()[funcStmt->name] =
            new FuncVariable(funcStmt->name, funcStmt->returnType, funcStmt->params);
        // ToDo Please change this xD
        std::map<std::string, Variable *> prevVariables = compiler->variables.back();
        compiler->variables.push_back({});
        for (auto &param : funcStmt->params) {
            compiler->variables.back()[param->name] = param;
        }
        for (auto &bodyStmt : funcStmt->body) {
            fixExprEvaluatesToStmt(bodyStmt);
        }
        compiler->variables.pop_back();
        break;
    }
    case BREAK_STMT: {
        break;
    }
    case STRUCT_STMT: {
        StructStmt *structStmt = (StructStmt *)stmt;
        compiler->variables.back()[structStmt->name] = new StructVariable("", structStmt->name, structStmt->fields);
        break;
    }
    }
}

Compiler *compile(std::string source) {
    scanner = new Scanner();
    initScanner(scanner, source.c_str());

    initParser();

    initCompiler();
    advance();
    while (!match(TOKEN_EOF)) {
        Stmt *stmt = declaration();
        fixExprEvaluatesToStmt(stmt);
        compiler->statements.push_back(stmt);
    }
    // debugStatements(compiler->statements);

    delete (scanner);
    free(parser);

    return compiler;
}
