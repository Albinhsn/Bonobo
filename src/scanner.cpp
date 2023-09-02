#include "scanner.h"
#include "debug.h"

#include <cctype>
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
  int current = scanner->current - 1;
  while (!isAtEnd(scanner) && isdigit(scanner->source[scanner->current])) {
    scanner->current++;
  }
  if (scanner->source[scanner->current] == '.') {
    scanner->current++;
  }
  while (!isAtEnd(scanner) && isdigit(scanner->source[scanner->current])) {
    scanner->current++;
  }
  return new Token(scanner->source.substr(current, scanner->current - current),
                   scanner->line, scanner->indent, TOKEN_NUMBER);
}
static TokenType isKeyword(std::string literal) {
  std::map<std::string, TokenType> m{
      {"struct", TOKEN_STRUCT}, {"else", TOKEN_ELSE}, {"false", TOKEN_FALSE},
      {"for", TOKEN_FOR},       {"if", TOKEN_IF},     {"nil", TOKEN_NIL},
      {"return", TOKEN_RETURN}, {"true", TOKEN_TRUE}, {"while", TOKEN_WHILE},
      {"print", TOKEN_PRINT},   {"var", TOKEN_VAR},   {"fun", TOKEN_FUN},
      {"and", TOKEN_AND},       {"or", TOKEN_OR}};
  return m.count(literal) ? m[literal] : TOKEN_IDENTIFIER;
}

static bool isAlpha(char c) {
  return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_';
}

static Token *parseIdentifier(Scanner *scanner) {
  int current = scanner->current - 1;
  while (!isAtEnd(scanner) && isAlpha(scanner->source[scanner->current])) {
    scanner->current++;
  }
  std::string literal =
      scanner->source.substr(current, scanner->current - current);
  return new Token(literal, scanner->line, scanner->indent, isKeyword(literal));
}

static Token *parseString(Scanner *scanner) {
  int current = scanner->current;
  while (!isAtEnd(scanner) && scanner->source[scanner->current] != '"' &&
         scanner->source[scanner->current] != '\n') {
    scanner->current++;
  }

  if (isAtEnd(scanner)) {
    throw std::invalid_argument("Hit eof with unterminated string.");
  }

  scanner->current++;
  return new Token(
      scanner->source.substr(current, scanner->current - current - 1),
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
    return new Token("EOF", scanner->line, scanner->indent, TOKEN_EOF);
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
    return new Token("(", scanner->line, scanner->indent, TOKEN_LEFT_PAREN);
  }
  case ')': {
    return new Token(")", scanner->line, scanner->indent, TOKEN_RIGHT_PAREN);
  }
  case '{': {
    return new Token("{", scanner->line, scanner->indent, TOKEN_LEFT_BRACE);
  }
  case '}': {
    return new Token("}", scanner->line, scanner->indent, TOKEN_RIGHT_BRACE);
  }
  case '[': {
    return new Token("[", scanner->line, scanner->indent, TOKEN_LEFT_BRACKET);
  }
  case ']': {
    return new Token("]", scanner->line, scanner->indent, TOKEN_RIGHT_BRACKET);
  }
  case ';': {
    return new Token(";", scanner->line, scanner->indent, TOKEN_SEMICOLON);
  }
  case ',': {
    return new Token(",", scanner->line, scanner->indent, TOKEN_COMMA);
  }
  case '.': {
    return new Token(".", scanner->line, scanner->indent, TOKEN_DOT);
  }
  case '+': {
    return new Token("+", scanner->line, scanner->indent, TOKEN_PLUS);
  }
  case '*': {
    return new Token("*", scanner->line, scanner->indent, TOKEN_STAR);
  }
  case ':': {
    return new Token(":", scanner->line, scanner->indent, TOKEN_COLON);
  }
  case '!': {
    if (match(scanner, '=')) {
      return new Token("!=", scanner->line, scanner->indent, TOKEN_BANG_EQUAL);
    }
    return new Token("!", scanner->line, scanner->indent, TOKEN_BANG);
  }
  case '=': {
    if (match(scanner, '=')) {
      return new Token("==", scanner->line, scanner->indent, TOKEN_EQUAL_EQUAL);
    }
    return new Token("=", scanner->line, scanner->indent, TOKEN_EQUAL);
  }
  case '<': {
    if (match(scanner, '=')) {
      return new Token("<=", scanner->line, scanner->indent, TOKEN_LESS_EQUAL);
    }
    return new Token("<", scanner->line, scanner->indent, TOKEN_LESS);
  }
  case '>': {
    if (match(scanner, '=')) {
      return new Token(">=", scanner->line, scanner->indent,
                       TOKEN_GREATER_EQUAL);
    }
    return new Token(">", scanner->line, scanner->indent, TOKEN_GREATER);
  }
  case '-': {
    if (match(scanner, '>')) {
      return new Token("->", scanner->line, scanner->indent, TOKEN_ARROW);
    }
    return new Token("-", scanner->line, scanner->indent, TOKEN_MINUS);
  }
  case '/': {
    return new Token("/", scanner->line, scanner->indent, TOKEN_SLASH);
  }
  default:
    std::string exception = "Unknown characther ";
    exception.push_back(c);
    throw std::invalid_argument(exception);
  }
}
