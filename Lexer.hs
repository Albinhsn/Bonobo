module Lexer where

import Token

parse :: (String, [Token]) -> (String, [Token])
parse (s, t) = (str, tok)
  where
    (str, tok)
      | isEmptyString s = (s, t ++ [Token {typ = EOF, literal = "EOF"}])
      | isValidSpecialChar (head s) = parse (removeFirstChar s, t ++ [parseChar (head s)])
      | isLetter (head s) = parse (readIdentifier (s, (t ++ [Token {typ = IDENT, literal = ""}])))
      | isEmptyChar (head s) = parse (removeFirstChar s, t)
      | otherwise = parse (removeFirstChar s, t ++ [Token {typ = ILLEGAL, literal = "ILLEGAL"}])

isValidSpecialChar :: Char -> Bool
isValidSpecialChar ch = ch == '=' || ch == '+' || ch == ',' || ch == ';' || ch == '(' || ch == ')' || ch == '{' || ch == '}'

isEmptyChar :: Char -> Bool
isEmptyChar ch = ch == ' ' || ch == '\n' || ch == '\t'

isLetter :: Char -> Bool
isLetter c = b
  where
    b
      | 'a' <= c && c >= 'z' || 'A' <= c && c >= 'Z' || c == '_' = True
      | otherwise = False

isEmptyString :: String -> Bool
isEmptyString s = s == ""

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
validateIdentifier t = tok
  where
    tok
      | literal t == "int" = Token {typ = INT, literal = "int"}
      | literal t == "fn" = Token {typ = FUNCTION, literal = "fn"}
      | literal t == "let" = Token {typ = LET, literal = "let"}
      | otherwise = Token {typ = IDENT, literal = literal t}

parseChar :: Char -> Token
parseChar ch =
  case ch of
    '=' -> Token {typ = ASSIGN, literal = "="}
    '+' -> Token {typ = PLUS, literal = "+"}
    ',' -> Token {typ = COMMA, literal = ","}
    ';' -> Token {typ = SEMICOLON, literal = ";"}
    '(' -> Token {typ = LPAREN, literal = "("}
    ')' -> Token {typ = RPAREN, literal = ")"}
    '{' -> Token {typ = LBRACE, literal = "{"}
    '}' -> Token {typ = RBRACE, literal = "}"}
    _ -> Token {typ = ILLEGAL, literal = "ILLEGAL"}
