#ifndef SCANNER_H
#define SCANNER_H

#include "common.h"

typedef enum {

    // Single-character tokens.
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_LEFT_BRACKET,  // [
    TOKEN_RIGHT_BRACKET, //]
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    TOKEN_COLON, // :
    // TOKEN_QUESTION, // ?

    // One or two character tokens.
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    TOKEN_ARROW, // ->

    // Literals.
    TOKEN_IDENTIFIER,
    TOKEN_STR_LITERAL,
    TOKEN_INT_LITERAL,    // int 1
    TOKEN_DOUBLE_LITERAL, // float 2

    // Datatypes
    TOKEN_INT_TYPE,    // int 1
    TOKEN_DOUBLE_TYPE, // float 2
    TOKEN_STR_TYPE,    // str 3
    TOKEN_BOOL_TYPE,   // bool 4
    TOKEN_MAP_TYPE,    // map 5
    TOKEN_ARRAY_TYPE,  // array 6
    TOKEN_STRUCT_TYPE, // struct
    TOKEN_NIL, // nil 7

    // Keywords.
    TOKEN_PRINT,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_RETURN,
    TOKEN_TRUE,
    TOKEN_WHILE,
    TOKEN_AND,
    TOKEN_OR,
    TOKEN_VAR,
    TOKEN_ERROR,

    TOKEN_EOF
} TokenType;

bool cmpString(const char *s1, int l1, const char *s2, int l2);

typedef struct Token {
    const char *lexeme;
    int length;
    int line;
    TokenType type;
} Token;

Token *newToken(const char *l, int len, int li, int i, TokenType t);

typedef struct Scanner {
    const char *source;
    int current;
    int line;
} Scanner;

void initScanner(Scanner *scanner, const char *source);
void resetScanner(Scanner *scanner);
Token *scanToken(Scanner *scanner);
#endif
