#include "scanner.h"
#include <cstring>
#include <stdexcept>

void resetScanner(Scanner *scanner) {
    scanner->current = 0;
    scanner->line = 1;
    scanner->source = "";
}

void initScanner(Scanner *scanner, const char *source) {
    scanner->source = source;
    scanner->current = 0;
    scanner->line = 1;
}

Token *newToken(const char *lexeme, int length, int line, TokenType type) {
    Token *token = NULL;
    token = (Token *)malloc(sizeof(Token));
    token->type = type;
    token->length = length;
    token->lexeme = lexeme;
    token->line = line;

    return token;
}

bool cmpString(const char *s1, int l1, const char *s2, int l2) {
    return (l1 == l2 && memcmp(s1, s2, l1) == 0);
}

static bool isAtEnd(Scanner *scanner) {
    return scanner->source[scanner->current] == '\0';
}

static inline char currentChar(Scanner *scanner) {
    return scanner->source[scanner->current];
}

static bool match(Scanner *scanner, char needle) {
    if (!isAtEnd(scanner) && currentChar(scanner) == needle) {
        scanner->current++;
        return true;
    }
    return false;
}

static inline int getLength(Scanner *scanner, const char *current) {
    return (int)(&scanner->source[scanner->current] - current);
}

static Token *parseNumber(Scanner *scanner) {
    const char *current = &scanner->source[scanner->current - 1];
    while (!isAtEnd(scanner) && isdigit(currentChar(scanner))) {
        scanner->current++;
    }
    if (currentChar(scanner) == '.') {
        scanner->current++;
        while (!isAtEnd(scanner) && isdigit(currentChar(scanner))) {
            scanner->current++;
        }
        return newToken(current, getLength(scanner, current), scanner->line,
                        TOKEN_DOUBLE_LITERAL);
    } else {
        return newToken(current, getLength(scanner, current), scanner->line,
                        TOKEN_INT_LITERAL);
    }
}

static inline TokenType checkKeyword(const char *current, const char *keyword,
                                     int check_len, int length,
                                     TokenType type) {
    return (check_len == length && memcmp(current, keyword, length) == 0)
               ? type
               : TOKEN_IDENTIFIER;
}
static TokenType isKeyword(const char *current, int len) {
    switch (current[0]) {
    case 'a': {
        return checkKeyword(current, "and", 3, len, TOKEN_AND);
    }
    case 'f': {
        switch (current[1]) {
        case 'a': {
            return checkKeyword(current, "false", 5, len, TOKEN_FALSE);
        }
        case 'b': {
            return checkKeyword(current, "bool", 4, len, TOKEN_BOOL_TYPE);
        }
        case 'l': {
            return checkKeyword(current, "double", 5, len, TOKEN_DOUBLE_TYPE);
        }
        case 'o': {
            return checkKeyword(current, "for", 3, len, TOKEN_FOR);
        }
        case 'u': {
            return checkKeyword(current, "fun", 3, len, TOKEN_FUN);
        }
        default:
            return TOKEN_IDENTIFIER;
        }
    }
    case 'e': {
        return checkKeyword(current, "else", 4, len, TOKEN_ELSE);
    }
    case 'i': {
        switch (current[1]) {
        case 'n': {
            return checkKeyword(current, "int", 3, len, TOKEN_INT_TYPE);
        }
        case 'f': {
            return checkKeyword(current, "if", 2, len, TOKEN_IF);
        }
        default: {
            return TOKEN_IDENTIFIER;
        }
        }
    }
    case 'n': {
        return checkKeyword(current, "nil", 3, len, TOKEN_NIL);
    }
    case 'o': {
        return checkKeyword(current, "or", 2, len, TOKEN_OR);
    }
    case 'p': {
        return checkKeyword(current, "print", 5, len, TOKEN_PRINT);
    }
    case 'r': {
        return checkKeyword(current, "return", 6, len, TOKEN_RETURN);
    }
    case 's': {
        switch (current[1]) {
        case 't': {
            switch (current[2]) {
            case 'r': {
                if (len == 3) {
                  return TOKEN_STR_TYPE;
                }
                switch (current[3]) {
                case 'u': {
                    return checkKeyword(current, "struct", 6, len,
                                        TOKEN_STRUCT_TYPE);
                }
                default: {
                    return TOKEN_IDENTIFIER;
                }
                }
            }
            default: {
                return TOKEN_IDENTIFIER;
            }
            }
        }
        default: {
            return TOKEN_IDENTIFIER;
        }
        }
    }
    case 't': {
        return checkKeyword(current, "true", 4, len, TOKEN_TRUE);
    }
    case 'v': {
        return checkKeyword(current, "var", 3, len, TOKEN_VAR);
    }
    case 'w': {
        return checkKeyword(current, "while", 5, len, TOKEN_WHILE);
    }
    default: {
        return TOKEN_IDENTIFIER;
    }
    }
}

static inline bool isAlpha(char c) {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

static Token *parseIdentifier(Scanner *scanner) {
    const char *current = &scanner->source[scanner->current - 1];
    while (!isAtEnd(scanner) && (isAlpha(currentChar(scanner))) ||
           isdigit(currentChar(scanner))) {
        scanner->current++;
    }

    int len = getLength(scanner, current);
    return newToken(current, len, scanner->line, isKeyword(current, len));
}

static Token *parseString(Scanner *scanner) {
    const char *current = &scanner->source[scanner->current];
    while (!isAtEnd(scanner) && currentChar(scanner) != '"' &&
           currentChar(scanner) != '\n') {
        scanner->current++;
    }

    if (isAtEnd(scanner)) {
        std::cout << "Hit eof with unterminated string.";
        exit(1);
    }

    scanner->current++;
    return newToken(current, getLength(scanner, current) - 1, scanner->line,
                    TOKEN_STR_LITERAL);
}

void skipWhitespace(Scanner *scanner) {
    for (;;) {
        if (isAtEnd(scanner)) {
            return;
        }
        switch (currentChar(scanner)) {
        case '/': {
            scanner->current++;
            if (match(scanner, '/')) {
                while (!isAtEnd(scanner) and !match(scanner, '\n')) {
                    scanner->current++;
                }
                scanner->line++;
                return;
            }
            scanner->current--;
            return;
        }
        case ' ':
        case '\t':
        case '\r': {
            break;
        }
        case '\n': {
            scanner->line++;
            break;
        }
        default:
            return;
        }
        scanner->current++;
    }
}

Token *scanToken(Scanner *scanner) {
    skipWhitespace(scanner);
    if (isAtEnd(scanner)) {
        return newToken("EOF", 3, scanner->line, TOKEN_EOF);
    }
    scanner->current++;
    char c = scanner->source[scanner->current - 1];
    if (isdigit(c)) {
        return parseNumber(scanner);
    }
    if (isAlpha(c)) {
        return parseIdentifier(scanner);
    }
    switch (c) {
    case '"': {
        return parseString(scanner);
    }
    case '(': {
        return newToken("(", 1, scanner->line, TOKEN_LEFT_PAREN);
    }
    case ')': {
        return newToken(")", 1, scanner->line, TOKEN_RIGHT_PAREN);
    }
    case '{': {
        return newToken("{", 1, scanner->line, TOKEN_LEFT_BRACE);
    }
    case '}': {
        return newToken("}", 1, scanner->line, TOKEN_RIGHT_BRACE);
    }
    case '[': {
        return newToken("[", 1, scanner->line, TOKEN_LEFT_BRACKET);
    }
    case ']': {
        return newToken("]", 1, scanner->line, TOKEN_RIGHT_BRACKET);
    }
    case ';': {
        return newToken(";", 1, scanner->line, TOKEN_SEMICOLON);
    }
    case ',': {
        return newToken(",", 1, scanner->line, TOKEN_COMMA);
    }
    case '.': {
        return newToken(".", 1, scanner->line, TOKEN_DOT);
    }
    case '+': {
        return newToken("+", 1, scanner->line, TOKEN_PLUS);
    }
    case '*': {
        return newToken("*", 1, scanner->line, TOKEN_STAR);
    }
    case ':': {
        return newToken(":", 1, scanner->line, TOKEN_COLON);
    }
    case '!': {
        if (match(scanner, '=')) {
            return newToken("!=", 2, scanner->line, TOKEN_BANG_EQUAL);
        }
        return newToken("!", 1, scanner->line, TOKEN_BANG);
    }
    case '=': {
        if (match(scanner, '=')) {
            return newToken("==", 2, scanner->line, TOKEN_EQUAL_EQUAL);
        }
        return newToken("=", 1, scanner->line, TOKEN_EQUAL);
    }
    case '<': {
        if (match(scanner, '=')) {
            return newToken("<=", 2, scanner->line, TOKEN_LESS_EQUAL);
        }
        return newToken("<", 1, scanner->line, TOKEN_LESS);
    }
    case '>': {
        if (match(scanner, '=')) {
            return newToken(">=", 2, scanner->line, TOKEN_GREATER_EQUAL);
        }
        return newToken(">", 1, scanner->line, TOKEN_GREATER);
    }
    case '-': {
        if (match(scanner, '>')) {
            return newToken("->", 2, scanner->line, TOKEN_ARROW);
        }
        return newToken("-", 1, scanner->line, TOKEN_MINUS);
    }
    case '/': {
        return newToken("/", 1, scanner->line, TOKEN_SLASH);
    }
    default:
        std::cout << "Unknown characther " << c << "\n";
        exit(1);
    }
}
