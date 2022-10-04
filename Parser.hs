module Parser where

import Ast
import Lexer
import Token

parseProgram :: String -> [Token]
parseProgram s = snd (parseTokens (s, []))

parseStatements :: ([Token], [Statement]) -> ([Token], [Statement])
parseStatements (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | null t = (t, s)
      | typ (head t) == LET = parseStatements (parseLetStatement (removeFirstToken t, s ++ [LetStatement {identifier = "", expression = Expression {expr = ""}}]))
      | typ (head t) == RETURN = (t, s)
      | typ (head t) == IF = (t, s)
      | typ (head t) == EOF = (removeFirstToken t, s)
      | otherwise = error "couldn't parse token for statement"

parseLetStatement :: ([Token], [Statement]) -> ([Token], [Statement])
parseLetStatement (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | identifier (last s) == "" = parseLetStatement (parseTokenIdentifier (t, s))
      | expr (expression (last s)) == "" && typ (head t) == ASSIGN = parseExpression (removeFirstToken t, s)
      | typ (head t) == EOF = (removeFirstToken t, s)
      | otherwise = error "couldn't parse letStatement"

parseTokenIdentifier :: ([Token], [Statement]) -> ([Token], [Statement])
parseTokenIdentifier (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == IDENT = (removeFirstToken t, pop s ++ [LetStatement {identifier = literal (head t), expression = Expression {expr = ""}}])
      | otherwise = error "Couldn't parse literal"

parseExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == INT = parseIntegerExpression (removeFirstToken t, pop s ++ [LetStatement {identifier = identifier (last s), expression = IntegerLiteralExpression {integerLiteral = stringToInt (literal (head t))}}])
      | typ (head t) == SEMICOLON = (removeFirstToken t, s)
      | otherwise = (t, s)

parseIntegerExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseIntegerExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | isArtithmetic (head t) =
          parseIntegerExpression
            ( parseOperatorExpression
                ( removeFirstToken t,
                  pop s
                    ++ [ LetStatement
                           { identifier = identifier (last s),
                             expression =
                               OperatorExpression
                                 { leftOperator = expression (last s), -- Always integer literal?
                                   operator = head t,
                                   rightOperator = Expression {expr = ""}
                                 }
                           }
                       ]
                )
            )
      | typ (head t) == LPAREN = (t, s) -- parseGroupedExpression
      | otherwise = (t, s)

parseOperatorExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseOperatorExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == INT =
          parseOperatorExpression
            ( removeFirstToken t,
              pop s
                ++ [ LetStatement
                       { identifier = identifier (last s),
                         expression =
                           OperatorExpression
                             { leftOperator = leftOperator (expression (last s)),
                               operator = operator (expression (last s)),
                               rightOperator =
                                 IntegerLiteralExpression
                                   { integerLiteral = stringToInt (literal (head t))
                                   }
                             }
                       }
                   ]
            )
      | typ (head t) == IDENT = (t, s) -- How to handle this
      | typ (head t) == LPAREN = (t, s) -- parseGroupedExpression
      | typ (head t) == SEMICOLON = (removeFirstToken t, s)
      | otherwise = (t, s)

isArtithmetic :: Token -> Bool
isArtithmetic t = typ t == PLUS || typ t == ASTERISK || typ t == SLASH || typ t == MINUS

stringToInt :: String -> Int
stringToInt s = read s :: Int

removeFirstToken :: [Token] -> [Token]
removeFirstToken xs = case xs of
  [] -> []
  x : xs -> xs
