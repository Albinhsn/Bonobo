module Lexer where

import Token

parseTokens :: (String, [Token]) -> (String, [Token])
parseTokens (s, t) = (str, tok)
  where
    (str, tok)
      | null s = (s, t ++ [Token {typ = EOF, literal = "EOF"}])
      | isValidSpecialChar (head s) = parseTokens (removeFirstChar s, t ++ [parseChar (head s)])
      | isDoubleChar (head s) = parseTokens (parseDoubleChar (head s, removeFirstChar s, t))
      | isNumber (head s) = parseTokens (readNumber (s, t ++ [Token {typ = INT, literal = ""}]))
      | isLetter (head s) = parseTokens (readIdentifier (s, t ++ [Token {typ = IDENT, literal = ""}]))
      | isEmptyChar (head s) = parseTokens (removeFirstChar s, t)
      | otherwise = parseTokens (removeFirstChar s, t ++ [Token {typ = ILLEGAL, literal = "ILLEGAL"}])

isValidSpecialChar :: Char -> Bool
isValidSpecialChar ch = ch == '-' || ch == '>' || ch == '<' || ch == '*' || ch == '/' || ch == '+' || ch == ',' || ch == ';' || ch == '(' || ch == ')' || ch == '{' || ch == '}'

isDoubleChar :: Char -> Bool
isDoubleChar ch = ch == '=' || ch == '!'

parseDoubleChar :: (Char, String, [Token]) -> (String, [Token])
parseDoubleChar (ch, s, t) = (string, token)
  where
    (string, token)
      | ch == '=' && ch == head s = (removeFirstChar s, t ++ [Token {typ = EQUALS, literal = "=="}])
      | ch == '!' && '=' == head s = (removeFirstChar s, t ++ [Token {typ = NOT_EQUALS, literal = "!="}])
      | ch == '!' = (s, t ++ [Token {typ = BANG, literal = "!"}])
      | ch == '=' = (s, t ++ [Token {typ = ASSIGN, literal = "="}])

isEmptyChar :: Char -> Bool
isEmptyChar ch = ch == ' ' || ch == '\n' || ch == '\t'

isLetter :: Char -> Bool
isLetter c = b
  where
    b
      | 'a' <= c && c >= 'z' || 'A' <= c && c >= 'Z' || c == '_' = True
      | otherwise = False

removeFirstChar :: String -> String
removeFirstChar xs = case xs of
  [] -> []
  x : xs -> xs

readIdentifier :: (String, [Token]) -> (String, [Token])
readIdentifier (s, t) = (str, tok)
  where
    (str, tok)
      | s == "" = (s, pop t ++ [validateIdentifier (last t)])
      | isLetter (head s) = readIdentifier (removeFirstChar s, addIdentifierTolastToken (head s, t))
      | otherwise = (s, pop t ++ [validateIdentifier (last t)])

addIdentifierTolastToken :: (Char, [Token]) -> [Token]
addIdentifierTolastToken (ch, t) = pop t ++ [Token {typ = IDENT, literal = getLastLiteral t ++ [ch]}]

addNumberToLastToken :: (Char, [Token]) -> [Token]
addNumberToLastToken (c, t) = pop t ++ [Token {typ = INT, literal = getLastLiteral t ++ [c]}]

getLastLiteral :: [Token] -> String
getLastLiteral t = s
  where
    s
      | null t = ""
      | otherwise = literal (last t)

pop :: [a] -> [a]
pop [] = []
pop a = init a

validateIdentifier :: Token -> Token
validateIdentifier t =
  case literal t of
    "int" -> Token {typ = INT, literal = literal t}
    "fn" -> Token {typ = FUNCTION, literal = literal t}
    "let" -> Token {typ = LET, literal = literal t}
    "if" -> Token {typ = IF, literal = literal t}
    "else" -> Token {typ = ELSE, literal = literal t}
    "return" -> Token {typ = RETURN, literal = literal t}
    "true" -> Token {typ = TRUE, literal = literal t}
    "false" -> Token {typ = FALSE, literal = literal t}
    _ -> Token {typ = IDENT, literal = literal t}

readNumber :: (String, [Token]) -> (String, [Token])
readNumber (s, t) = (str, tok)
  where
    (str, tok)
      | s == "" = (s, t)
      | isNumber (head s) = readNumber (removeFirstChar s, addNumberToLastToken (head s, t))
      | otherwise = (s, t)

isNumber :: Char -> Bool
isNumber ch =
  case ch of
    '1' -> True
    '2' -> True
    '3' -> True
    '4' -> True
    '5' -> True
    '6' -> True
    '7' -> True
    '8' -> True
    '9' -> True
    '0' -> True
    _ -> False

parseChar :: Char -> Token
parseChar ch =
  case ch of
    '=' -> Token {typ = ASSIGN, literal = "="}
    '+' -> Token {typ = PLUS, literal = "+"}
    '-' -> Token {typ = MINUS, literal = "-"}
    ',' -> Token {typ = COMMA, literal = ","}
    ';' -> Token {typ = SEMICOLON, literal = ";"}
    '(' -> Token {typ = LPAREN, literal = "("}
    ')' -> Token {typ = RPAREN, literal = ")"}
    '{' -> Token {typ = LBRACE, literal = "{"}
    '}' -> Token {typ = RBRACE, literal = "}"}
    '*' -> Token {typ = ASTERISK, literal = "*"}
    '/' -> Token {typ = SLASH, literal = "/"}
    '<' -> Token {typ = LESS_T, literal = "<"}
    '>' -> Token {typ = GREATER_T, literal = ">"}
    '!' -> Token {typ = BANG, literal = "!"}
    _ -> Token {typ = ILLEGAL, literal = "ILLEGAL"}
