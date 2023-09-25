#include "scanner.h"
#include "trie.h"
#include <cstring>
#include <stdexcept>

Trie *trie;

void resetScanner(Scanner *scanner) {
    scanner->current = 0;
    scanner->line = 1;
    scanner->source = "";
}

void initScanner(Scanner *scanner, std::string source) {
    trie = new Trie();
    scanner->source = source;
    scanner->current = 0;
    scanner->line = 1;
}

Token *newToken(std::string lexeme, int line, TokenType type) {
    Token *token = new Token();
    token->type = type;
    token->lexeme = lexeme;
    token->line = line;

    return token;
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

static Token *parseNumber(Scanner *scanner) {
    int current = scanner->current - 1;
    while (!isAtEnd(scanner) && isdigit(currentChar(scanner))) {
        scanner->current++;
    }
    if (currentChar(scanner) == '.') {
        scanner->current++;
        while (!isAtEnd(scanner) && isdigit(currentChar(scanner))) {
            scanner->current++;
        }
        return newToken(
            scanner->source.substr(current, scanner->current - current),
            scanner->line, TOKEN_DOUBLE_LITERAL);
    } else {
        return newToken(
            scanner->source.substr(current, scanner->current - current),
            scanner->line, TOKEN_INT_LITERAL);
    }
}

static inline bool isAlpha(char c) {
    return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

static Token *parseIdentifier(Scanner *scanner) {
    int current = scanner->current - 1;
    while (!isAtEnd(scanner) && (isAlpha(currentChar(scanner))) ||
           isdigit(currentChar(scanner))) {
        scanner->current++;
    }
    std::string ident =
        scanner->source.substr(current, scanner->current - current);
    return newToken(ident, scanner->line, trie->isKeyword(ident));
}

static Token *parseString(Scanner *scanner) {
    int current = scanner->current;
    while (!isAtEnd(scanner) && currentChar(scanner) != '"') {
        scanner->current++;
    }

    if (isAtEnd(scanner)) {
        std::cout << "Hit eof with unterminated string.";
        exit(1);
    }

    scanner->current++;
    return newToken(
        scanner->source.substr(current, scanner->current - current - 1),
        scanner->line, TOKEN_STR_LITERAL);
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
        return newToken("EOF", scanner->line, TOKEN_EOF);
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
        return newToken("(", scanner->line, TOKEN_LEFT_PAREN);
    }
    case ')': {
        return newToken(")", scanner->line, TOKEN_RIGHT_PAREN);
    }
    case '{': {
        return newToken("{", scanner->line, TOKEN_LEFT_BRACE);
    }
    case '}': {
        return newToken("}", scanner->line, TOKEN_RIGHT_BRACE);
    }
    case '[': {
        return newToken("[", scanner->line, TOKEN_LEFT_BRACKET);
    }
    case ']': {
        return newToken("]", scanner->line, TOKEN_RIGHT_BRACKET);
    }
    case ';': {
        return newToken(";", scanner->line, TOKEN_SEMICOLON);
    }
    case ',': {
        return newToken(",", scanner->line, TOKEN_COMMA);
    }
    case '.': {
        return newToken(".", scanner->line, TOKEN_DOT);
    }
    case '+': {
        return newToken("+", scanner->line, TOKEN_PLUS);
    }
    case '*': {
        return newToken("*", scanner->line, TOKEN_STAR);
    }
    case ':': {
        return newToken(":", scanner->line, TOKEN_COLON);
    }
    case '!': {
        if (match(scanner, '=')) {
            return newToken("!=", scanner->line, TOKEN_BANG_EQUAL);
        }
        return newToken("!", scanner->line, TOKEN_BANG);
    }
    case '=': {
        if (match(scanner, '=')) {
            return newToken("==", scanner->line, TOKEN_EQUAL_EQUAL);
        }
        return newToken("=", scanner->line, TOKEN_EQUAL);
    }
    case '<': {
        if (match(scanner, '=')) {
            return newToken("<=", scanner->line, TOKEN_LESS_EQUAL);
        }
        return newToken("<", scanner->line, TOKEN_LESS);
    }
    case '>': {
        if (match(scanner, '=')) {
            return newToken(">=", scanner->line, TOKEN_GREATER_EQUAL);
        }
        return newToken(">", scanner->line, TOKEN_GREATER);
    }
    case '-': {
        if (match(scanner, '>')) {
            return newToken("->", scanner->line, TOKEN_ARROW);
        }
        return newToken("-", scanner->line, TOKEN_MINUS);
    }
    case '/': {
        return newToken("/", scanner->line, TOKEN_SLASH);
    }
    default:
        std::cout << "Unknown characther " << c << "\n";
        exit(1);
    }
}
