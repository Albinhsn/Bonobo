module Lexer where

import Token

data Lexer = Lexer {input :: !String, position :: !Int, readPosition :: !Int, tokens :: ![Token]} deriving (Show)

isValidSpecialChar :: Char -> Bool
isValidSpecialChar ch = ch == '=' || ch == '+' || ch == ',' || ch == ';' || ch == '(' || ch == ')' || ch == '{' || ch == '}'

isEmptyChar :: Char -> Bool
isEmptyChar ch = ch == ' ' || ch == '\n' || ch == '\t'

isInvalid :: Char -> Bool
isInvalid ch = True

getCurrentChar :: Lexer -> [Char]
getCurrentChar lexer = [input lexer !! position lexer]

getToken :: Lexer -> Lexer
getToken lexer = l
  where
    l
      | length (input lexer) < readPosition lexer = lexer
      | isEmptyChar (head (getCurrentChar lexer)) =
        getToken
          Lexer
            { input = input lexer,
              position = readPosition lexer,
              readPosition = readPosition lexer + 1,
              tokens = tokens lexer
            }
      | isValidSpecialChar (head (getCurrentChar lexer)) =
        getToken
          Lexer
            { input = input lexer,
              position = readPosition lexer,
              readPosition = readPosition lexer + 1,
              tokens =
                tokens lexer
                  ++ [ Token
                         { typ = ASSIGN,
                           literal = getCurrentChar lexer
                         }
                     ]
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
      | otherwise =
        Lexer
          { input = input lexer,
            position = readPosition lexer,
            readPosition = readPosition lexer + 1,
            tokens =
              tokens lexer
                ++ [ Token
                       { typ = EOF,
                         literal = "EOF"
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
      | readPosition lexer > length (input lexer) = lexer
      | isLetter (input lexer !! position lexer) =
        readIdentifier
          Lexer
            { input = input lexer,
              position = readPosition lexer,
              readPosition = readPosition lexer + 1,
              tokens =
                [ Token
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
            tokens = pop (tokens lexer) ++ validateIdentifier (head (tokens lexer))
          }

getLastToken :: Lexer -> Token
getLastToken l = tokens l !! (length (tokens l) - 1)

validateIdentifier :: Token -> [Token]
validateIdentifier t = token
  where
    token
      | literal t == "int" = [Token {typ = INT, literal = "int"}]
      | literal t == "function" = [Token {typ = FUNCTION, literal = "functiofunctionn"}]
      | literal t == "let" = [Token {typ = LET, literal = "let"}]
      | otherwise = [Token {typ = IDENT, literal = literal t}]

pop :: [a] -> [a]
pop [] = []
pop a = init a

charToString :: Char -> String
charToString c = [c]

createLexer :: String -> Lexer
createLexer s = Lexer {input = s, position = 0, readPosition = 1, tokens = []}
