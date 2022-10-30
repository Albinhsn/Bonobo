module Lexer where

import Token

parseTokens :: (Int, String, [Token]) -> (Int, String, [Token])
parseTokens (i, s, t) = (inte, str, tok)
  where
    (inte, str, tok)
      | null s = (i, s, t ++ [Token {line = i, typ = EOF, literal = "EOF"}])
      | isValidSpecialChar (head s) = parseTokens (i, removeFirstChar s, t ++ [parseChar (i, head s)])
      | isNewLine (head s) = parseTokens(i+1, removeFirstChar s, t)
      | head s == '"' =  parseTokens(parseString(i, removeFirstChar s, t ++ [Token {line = i, typ = STRING, literal = ""}]))
      | isDoubleChar (head s) = parseTokens (parseDoubleChar (i, head s, removeFirstChar s, t))
      | isNumber (head s) = parseTokens (readNumber (i, s, t ++ [Token {line = i, typ = INT, literal = ""}]))
      | isLetter (head s) = parseTokens (readIdentifier (i, s, t ++ [Token {line = i, typ = IDENT, literal = ""}]))
      | isEmptyChar (head s) = parseTokens (i, removeFirstChar s, t)
      | otherwise = parseTokens (i, removeFirstChar s, t ++ [Token {line = i, typ = ILLEGAL, literal = "ILLEGAL"}])


parseString :: (Int, String, [Token]) -> (Int, String, [Token])
parseString (i, s, t) = (inte, string, token)
  where 
    (inte, string, token)
      | null s = error "Couldn't finish parsing string"
      | head s == '"' = (i,removeFirstChar s,t)
      | otherwise = parseString(i, removeFirstChar s, pop t ++ [Token{line = i, typ = STRING, literal = literal (last t) ++ [head s]}])

isValidSpecialChar :: Char -> Bool
isValidSpecialChar ch = ch == ':' || ch == '[' || ch == ']' || ch == '-' || ch == '>' || ch == '<' || ch == '*' || ch == '/' || ch == '+' || ch == ',' || ch == ';' || ch == '(' || ch == ')' || ch == '{' || ch == '}'

isDoubleChar :: Char -> Bool
isDoubleChar ch = ch == '=' || ch == '!'

parseDoubleChar :: (Int, Char, String, [Token]) -> (Int, String, [Token])
parseDoubleChar (i, ch, s, t) = (inte, string, token)
  where
    (inte, string, token)
      | null s && ch == '!' = (i, s, t ++ [Token {line = i, typ = BANG, literal = "!"}])
      | null s && ch == '=' = (i, s, t ++ [Token {line = i, typ = ASSIGN, literal = "="}])
      | ch == '=' && ch == head s = (i, removeFirstChar s, t ++ [Token {line = i, typ = EQUALS, literal = "=="}])
      | ch == '!' && '=' == head s = (i, removeFirstChar s, t ++ [Token {line = i, typ = NOT_EQUALS, literal = "!="}])
      | ch == '!' = (i, s, t ++ [Token {line = i, typ = BANG, literal = "!"}])
      | ch == '=' = (i, s, t ++ [Token {line = i, typ = ASSIGN, literal = "="}])

isEmptyChar :: Char -> Bool
isEmptyChar ch = ch == ' ' || ch == '\t'

isNewLine :: Char -> Bool
isNewLine ch = ch == '\n'

isLetter :: Char -> Bool
isLetter c = b
  where
    b
      | ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c == '_' = True
      | otherwise = False

removeFirstChar :: String -> String
removeFirstChar xs = case xs of
  [] -> []
  x : xs -> xs

readIdentifier :: (Int, String, [Token]) -> (Int, String, [Token])
readIdentifier (i, s, t) = (inte, str, tok)
  where
    (inte, str, tok)
      | s == "" = (i, s, pop t ++ [validateIdentifier (last t)])
      | isLetter (head s) = readIdentifier (i, removeFirstChar s, addIdentifierTolastToken (i, head s, t))
      | otherwise = (i, s, pop t ++ [validateIdentifier (last t)])

addIdentifierTolastToken :: (Int, Char, [Token]) -> [Token]
addIdentifierTolastToken (i, ch, t) = pop t ++ [Token {line = i, typ = IDENT, literal = getLastLiteral t ++ [ch]}]

addNumberToLastToken :: (Int, Char, [Token]) -> [Token]
addNumberToLastToken (i, c, t) = pop t ++ [Token {line = i, typ = INT, literal = getLastLiteral t ++ [c]}]

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
    "int" -> Token {line = line t, typ = INT, literal = literal t}
    "fn" -> Token {line = line t,typ = FUNCTION, literal = literal t}
    "let" -> Token {line = line t,typ = LET, literal = literal t}
    "if" -> Token {line = line t,typ = IF, literal = literal t}
    "else" -> Token {line = line t,typ = ELSE, literal = literal t}
    "return" -> Token {line = line t,typ = RETURN, literal = literal t}
    "True" -> Token {line = line t,typ = TRUE, literal = literal t}
    "False" -> Token {line = line t,typ = FALSE, literal = literal t}
    _ -> Token {line = line t,typ = IDENT, literal = literal t}

readNumber :: (Int, String, [Token]) -> (Int, String, [Token])
readNumber (i, s, t) = (inte, str, tok)
  where
    (inte, str, tok)
      | s == "" = (i, s, t)
      | isNumber (head s) = readNumber (i, removeFirstChar s, addNumberToLastToken (i, head s, t))
      | otherwise = (i, s, t)

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

parseChar :: (Int, Char) -> Token
parseChar (i, ch) =
  case ch of
    '=' -> Token {line = i,typ = ASSIGN, literal = "="}
    '+' -> Token {line = i,typ = PLUS, literal = "+"}
    '-' -> Token {line = i,typ = MINUS, literal = "-"}
    ',' -> Token {line = i,typ = COMMA, literal = ","}
    ';' -> Token {line = i,typ = SEMICOLON, literal = ";"}
    '(' -> Token {line = i,typ = LPAREN, literal = "("}
    ')' -> Token {line = i,typ = RPAREN, literal = ")"}
    '{' -> Token {line = i,typ = LBRACE, literal = "{"}
    '}' -> Token {line = i,typ = RBRACE, literal = "}"}
    '*' -> Token {line = i,typ = ASTERISK, literal = "*"}
    '/' -> Token {line = i,typ = SLASH, literal = "/"}
    '<' -> Token {line = i,typ = LESS_T, literal = "<"}
    '>' -> Token {line = i,typ = GREATER_T, literal = ">"}
    '!' -> Token {line = i,typ = BANG, literal = "!"}
    '[' -> Token {line = i,typ = LBRACKET, literal = "["}
    ']' -> Token {line = i,typ = RBRACKET, literal = "]"}
    ':' -> Token{line = i, typ = COLON, literal = ":"}
    _ -> Token {line = i,typ = ILLEGAL, literal = "ILLEGAL"}
