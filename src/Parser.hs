module Parser where

import Ast
import Lexer
import Token
import Utils

parseProgram :: String -> [Token]
parseProgram s = snd (parseTokens (s, []))

parseStatements :: ([Token], [Statement]) -> ([Token], [Statement])
parseStatements (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | null t = (t, s)
      | typ (head t) == LET =
          parseStatements
            ( parseExpression
                ( parseIdentifier
                    ( removeFirstToken t,
                      s
                        ++ [ Statement
                               { statementType =
                                   LetStatement {identifier = ""},
                                 expression = Expression {expressionType = EMPTYEXP}
                               }
                           ]
                    )
                )
            )
      | typ (head t) == RETURN =
          parseStatements
            ( parseExpression
                ( removeFirstToken t,
                  s
                    ++ [ Statement
                           { statementType = ReturnStatement {},
                             expression = Expression {expressionType = EMPTYEXP}
                           }
                       ]
                )
            )
      | typ (head t) == EOF = (removeFirstToken t, s)
      | otherwise = (t, s) -- Parse expression statement

parseExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | null t = (t, s)
      | typ (head t) == INT =
          parseIntegerExpression
            ( removeFirstToken t,
              pop s
                ++ [ Statement
                       { statementType = statementType (last s),
                         expression =
                           IntegerLiteralExpression
                             { expressionType = INTEGERLITERALEXP,
                               integerLiteral = literal (head t)
                             }
                       }
                   ]
            )
      | isValidInfix (head t) =
          parseInfixExpression
            ( removeFirstToken t,
              pop s
                ++ [ Statement
                       { statementType = statementType (last s),
                         expression = InfixExpression {expressionType = INFIXEXP, infixOperator = head t, infixExpression = Expression {expressionType = EMPTYEXP}} -- Parse infix and then parseIntegerExpression?
                       }
                   ]
            )
      | otherwise = (t, s)

parseIntegerExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseIntegerExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | isPrefix (head t) = (t, s)
      | typ (head t) == SEMICOLON = (removeFirstToken t, s)
      | otherwise = error "failed to parse integer expression"

parseOperatorExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseOperatorExpression (t, s) = (t, s)

parseInfixExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseInfixExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | typ (head t) == INT = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = infixOperator (expression (last s)), infixExpression = IntegerLiteralExpression {expressionType = INTEGERLITERALEXP, integerLiteral = literal (head t)}}}])
      | otherwise = (t, s)
