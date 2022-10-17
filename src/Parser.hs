module Parser where

import Ast
import Lexer
import Token
import Utils


-- isOperator :: Token -> Bool 
-- isOperator t = typ t == PLUS || typ t == ASTERISK || typ t == SLASH 

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
        parseExpression (
          parseIntegerExpression (t, s)
          )
      | isOperator (head t) = 
        parseExpression (
          parseOperatorExpression (t, s) 
          )
      | isValidInfix (head t)=
        parseExpression (
          parseInfixExpression (t,s )
          )
      | typ (head t) == SEMICOLON = (removeFirstToken t, s)
      | otherwise = (t, s)

parseIntegerExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseIntegerExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}}])  
      | getLastExpressionType s == INFIXEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = getLastInfixOperator s, infixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}}}])
      | getLastExpressionType s == OPERATOREXP = 
        (
          removeFirstToken t, 
          pop s ++ [
            Statement 
              {
                statementType = statementType (last s), 
                expression = OperatorExpression 
                  {
                    expressionType = OPERATOREXP,
                    leftOperator = leftOperator (getLastExpression s),
                    operator = getLastOperator s,
                    rightOperator = addToLastRightOperator (head t, getLastRightOperator s)
                  }
              }
          ]
        ) 
      | otherwise = error "failed to parse integer expression"


addToLastRightOperator :: (Token, Expression) -> Expression
addToLastRightOperator (t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = IntegerLiteralExpression{expressionType = INTEXP, integerLiteral = literal t}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addToLastRightOperator (t, rightOperator e))}
      | otherwise = error "addToLastRightOperator"

parseOperatorExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseOperatorExpression (t, s) = (tok, sta)
  where
    (tok,sta)
      | getLastExpressionType s == INTEXP = (removeFirstToken t, pop s ++[Statement {statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = expression (last s), operator = head t, rightOperator = Expression {expressionType = EMPTYEXP}}}])
      | getLastExpressionType s == OPERATOREXP && checkPrecedence (head t, last s) == True = 
        (
          removeFirstToken t,
          pop s ++ [
            Statement {
              statementType = statementType (last s),
              expression = OperatorExpression 
                {
                  expressionType = OPERATOREXP,
                  leftOperator = getLastLeftOperator s,
                  operator = getLastOperator s,
                  rightOperator = OperatorExpression 
                    {
                      expressionType = OPERATOREXP,
                      leftOperator = getLastRightOperator s,
                      operator = head t,
                      rightOperator = Expression {expressionType = EMPTYEXP}
                    }
                }
              }
            ]
          )
      | getLastExpressionType s == OPERATOREXP && checkPrecedence (head t, last s) == False = 
        (
          removeFirstToken t,
          pop s ++ [
            Statement {
              statementType = statementType (last s),
              expression = OperatorExpression 
                {
                  expressionType = OPERATOREXP,
                  leftOperator = getLastExpression s,
                  operator = head t,
                  rightOperator = Expression {expressionType = EMPTYEXP} 
                }
              }
            ]
          )

      | otherwise = error "error parsing operator exp"

-- If precedence of operator compared to last:
--  X + Y * = X + (Y *)
-- Else:
--  X * Y + = (X * Y) +  
checkPrecedence :: (Token, Statement) -> Bool
checkPrecedence (t, s)= getPrecedence (typ t) > getPrecedence (typ (operator (expression s))) 

parseInfixExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseInfixExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = head t, infixExpression = Expression {expressionType = EMPTYEXP}}}]) 
      | otherwise = error "couldn't parse infix expression" 
