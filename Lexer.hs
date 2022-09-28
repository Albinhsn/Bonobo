module Lexer where

import Token

data Lexer = Lexer {input :: !String, position :: !Int, readPosition :: !Int, tokens :: ![Token]} deriving (Show)

isValidSpecialChar :: Char -> Bool
isValidSpecialChar ch = ch == '=' || ch == '+' || ch == ',' || ch == ';' || ch == '(' || ch == ')' || ch == '{' || ch == '}'

isEmptyChar :: Char -> Bool
isEmptyChar ch = ch == ' ' || ch == '\n' || ch == '\t'

parseChar :: Char -> Token
parseChar ch = t
  where
    t
      | ch == '=' = Token {typ = ASSIGN, literal = "="}
      | ch == '+' = Token {typ = PLUS, literal = "+"}
      | ch == ',' = Token {typ = COMMA, literal = ","}
      | ch == ';' = Token {typ = SEMICOLON, literal = ";"}
      | ch == '(' = Token {typ = LPAREN, literal = "("}
      | ch == ')' = Token {typ = RPAREN, literal = ")"}
      | ch == '{' = Token {typ = LBRACE, literal = "{"}
      | ch == '}' = Token {typ = RBRACE, literal = "}"}
      | otherwise = Token {typ = ILLEGAL, literal = "ILLEGAL"}

isInvalid :: Char -> Bool
isInvalid ch = True

getCurrentChar :: Lexer -> [Char]
getCurrentChar lexer = [input lexer !! position lexer]

eof :: Lexer -> Lexer
eof l =
  Lexer
    { input = input l,
      position = position l,
      readPosition = readPosition l,
      tokens = tokens l ++ [Token {typ = EOF, literal = "EOF"}]
    }

getToken :: Lexer -> Lexer
getToken lexer = l
  where
    l
      | length (input lexer) < readPosition lexer = Lexer {input = input lexer, position = position lexer, readPosition = readPosition lexer, tokens = (tokens lexer) ++ [Token {typ = EOF, literal = "EOF"}]}
      | isValidSpecialChar (head (getCurrentChar lexer)) =
        getToken
          Lexer
            { input = input lexer,
              position = readPosition lexer,
              readPosition = readPosition lexer + 1,
              tokens =
                tokens lexer
                  ++ [parseChar (head (getCurrentChar lexer))]
            }
      | isLetter (head (getCurrentChar lexer)) =
        getToken
          ( readIdentifier
              Lexer
                { input = input lexer,
                  position = readPosition lexer,
                  readPosition = readPosition lexer + 1,
                  tokens =
                    tokens lexer
                      ++ validateIdentifier
                        Token
                          { typ = IDENT,
                            literal = getCurrentChar lexer
                          }
                }
          )
      | isEmptyChar (head (getCurrentChar lexer)) =
        getToken
          Lexer
            { input = input lexer,
              position = readPosition lexer,
              readPosition = readPosition lexer + 1,
              tokens = tokens lexer
            }
      | otherwise =
        getToken
          Lexer
            { input = input lexer,
              position = readPosition lexer,
              readPosition = readPosition lexer + 1,
              tokens =
                tokens lexer
                  ++ [ Token
                         { typ = ILLEGAL,
                           literal = "ILLEGAL"
                         }
                     ]
            }

isLetter :: Char -> Bool
isLetter c = b
  where
    b
      | 'a' <= c && c >= 'z' || 'A' <= c && c >= 'Z' || c == '_' = True
      | otherwise = False

readIdentifier :: Lexer -> Lexer
readIdentifier lexer = l
  where
    l
      | readPosition lexer > length (input lexer) =
        Lexer
          { input = input lexer,
            position = readPosition lexer,
            readPosition = readPosition lexer + 1,
            tokens = pop (tokens lexer) ++ validateIdentifier (last (tokens lexer))
          }
      | isLetter (input lexer !! position lexer) =
        readIdentifier
          Lexer
            { input = input lexer,
              position = readPosition lexer,
              readPosition = readPosition lexer + 1,
              tokens =
                pop (tokens lexer)
                  ++ [ Token
                         { typ = IDENT,
                           literal = literal (getLastToken lexer) ++ getCurrentChar lexer
                         }
                     ]
            }
      | otherwise =
        Lexer
          { input = input lexer,
            position = position lexer,
            readPosition = readPosition lexer,
            tokens = pop (tokens lexer) ++ validateIdentifier (last (tokens lexer))
          }

getLastToken :: Lexer -> Token
getLastToken l = tokens l !! (length (tokens l) - 1)

validateIdentifier :: Token -> [Token]
validateIdentifier t = token
  where
    token
      | literal t == "int" = [Token {typ = INT, literal = "int"}]
      | literal t == "fn" = [Token {typ = FUNCTION, literal = "fn"}]
      | literal t == "let" = [Token {typ = LET, literal = "let"}]
      | otherwise = [Token {typ = IDENT, literal = literal t}]

pop :: [a] -> [a]
pop [] = []
pop a = init a

charToString :: Char -> String
charToString c = [c]

createLexer :: String -> Lexer
createLexer s = Lexer {input = s, position = 0, readPosition = 1, tokens = []}
