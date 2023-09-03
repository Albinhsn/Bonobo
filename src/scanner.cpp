#include "scanner.h"
#include "debug.h"

#include <cctype>
#include <cstring>
#include <iostream>
#include <map>
#include <stdexcept>

void resetScanner(Scanner *scanner) {
  scanner->current = 0;
  scanner->indent = 0;
  scanner->line = 1;
  scanner->source = "";
}

static bool isAtEnd(Scanner *scanner) {
  return scanner->source[scanner->current] == '\0';
}

static bool match(Scanner *scanner, char needle) {
  if (!isAtEnd(scanner) && scanner->source[scanner->current] == needle) {
    scanner->current++;
    return true;
  }
  return false;
}

static Token *parseNumber(Scanner *scanner) {
  const char *current = &scanner->source[scanner->current - 1];
  while (!isAtEnd(scanner) && isdigit(scanner->source[scanner->current])) {
    scanner->current++;
  }
  if (scanner->source[scanner->current] == '.') {
    scanner->current++;
  }
  while (!isAtEnd(scanner) && isdigit(scanner->source[scanner->current])) {
    scanner->current++;
  }
  return new Token(current, (int)(&scanner->source[scanner->current] - current),
                   scanner->line, scanner->indent, TOKEN_NUMBER);
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
    return checkKeyword(current, "if", 2, len, TOKEN_IF);
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
    return checkKeyword(current, "struct", 6, len, TOKEN_STRUCT);
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

static bool isAlpha(char c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

static Token *parseIdentifier(Scanner *scanner) {
  const char *current = &scanner->source[scanner->current - 1];
  while (!isAtEnd(scanner) && isAlpha(scanner->source[scanner->current])) {
    scanner->current++;
  }
  int len = (int)(&scanner->source[scanner->current] - current);
  return new Token(current, len, scanner->line, scanner->indent,
                   isKeyword(current, len));
}

static Token *parseString(Scanner *scanner) {
  const char *current = &scanner->source[scanner->current];
  while (!isAtEnd(scanner) && scanner->source[scanner->current] != '"' &&
         scanner->source[scanner->current] != '\n') {
    scanner->current++;
  }

  if (isAtEnd(scanner)) {
    throw std::invalid_argument("Hit eof with unterminated string.");
  }

  scanner->current++;
  return new Token(current,
                   (int)(&scanner->source[scanner->current] - current) - 1,
                   scanner->line, scanner->indent, TOKEN_STRING);
}

void skipWhitespace(Scanner *scanner) {
  for (;;) {
    if (isAtEnd(scanner)) {
      return;
    }
    switch (scanner->source[scanner->current]) {
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
    case ' ': {
      scanner->indent++;
      break;
    }
    case '\n': {
      scanner->line++;
      scanner->indent = 0;
      break;
    }
    case '\t': {
      scanner->indent += 4;
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
    return new Token("EOF", 3, scanner->line, scanner->indent, TOKEN_EOF);
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
    return new Token("(", 1, scanner->line, scanner->indent, TOKEN_LEFT_PAREN);
  }
  case ')': {
    return new Token(")", 1, scanner->line, scanner->indent, TOKEN_RIGHT_PAREN);
  }
  case '{': {
    return new Token("{", 1, scanner->line, scanner->indent, TOKEN_LEFT_BRACE);
  }
  case '}': {
    return new Token("}", 1, scanner->line, scanner->indent, TOKEN_RIGHT_BRACE);
  }
  case '[': {
    return new Token("[", 1, scanner->line, scanner->indent,
                     TOKEN_LEFT_BRACKET);
  }
  case ']': {
    return new Token("]", 1, scanner->line, scanner->indent,
                     TOKEN_RIGHT_BRACKET);
  }
  case ';': {
    return new Token(";", 1, scanner->line, scanner->indent, TOKEN_SEMICOLON);
  }
  case ',': {
    return new Token(",", 1, scanner->line, scanner->indent, TOKEN_COMMA);
  }
  case '.': {
    return new Token(".", 1, scanner->line, scanner->indent, TOKEN_DOT);
  }
  case '+': {
    return new Token("+", 1, scanner->line, scanner->indent, TOKEN_PLUS);
  }
  case '*': {
    return new Token("*", 1, scanner->line, scanner->indent, TOKEN_STAR);
  }
  case ':': {
    return new Token(":", 1, scanner->line, scanner->indent, TOKEN_COLON);
  }
  case '!': {
    if (match(scanner, '=')) {
      return new Token("!=", 2, scanner->line, scanner->indent,
                       TOKEN_BANG_EQUAL);
    }
    return new Token("!", 1, scanner->line, scanner->indent, TOKEN_BANG);
  }
  case '=': {
    if (match(scanner, '=')) {
      return new Token("==", 2, scanner->line, scanner->indent,
                       TOKEN_EQUAL_EQUAL);
    }
    return new Token("=", 1, scanner->line, scanner->indent, TOKEN_EQUAL);
  }
  case '<': {
    if (match(scanner, '=')) {
      return new Token("<=", 2, scanner->line, scanner->indent,
                       TOKEN_LESS_EQUAL);
    }
    return new Token("<", 1, scanner->line, scanner->indent, TOKEN_LESS);
  }
  case '>': {
    if (match(scanner, '=')) {
      return new Token(">=", 2, scanner->line, scanner->indent,
                       TOKEN_GREATER_EQUAL);
    }
    return new Token(">", 1, scanner->line, scanner->indent, TOKEN_GREATER);
  }
  case '-': {
    if (match(scanner, '>')) {
      return new Token("->", 2, scanner->line, scanner->indent, TOKEN_ARROW);
    }
    return new Token("-", 1, scanner->line, scanner->indent, TOKEN_MINUS);
  }
  case '/': {
    return new Token("/", 1, scanner->line, scanner->indent, TOKEN_SLASH);
  }
  default:
    std::string exception = "Unknown characther ";
    exception.push_back(c);
    throw std::invalid_argument(exception);
  }
}
