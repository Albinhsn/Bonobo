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
        parseExpression (
          parseIntegerExpression (t, s)
          )
      | 
        (
          (getLastExpressionType s == OPERATOREXP && expressionType (rightOperator (getLastExpression s)) == EMPTYEXP) 
          || getLastExpressionType s== EMPTYEXP
          || getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == EMPTYEXP
          || getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == OPERATOREXP  
        ) 
        && 
          typ (head t) == MINUS = 
            parseExpression (
              parseInfixExpression (t, s)
            )
      | isOperator (head t) = 
        parseExpression (
          parseOperatorExpression (t, s) 
          )
      | isValidInfix (head t)=
        parseExpression (
          parseInfixExpression (t,s )
          )
      | isBoolPrefix (head t) =
        parseExpression (
          parseBoolExpression (t, s)
        )
      | typ (head t) == SEMICOLON = (removeFirstToken t, s)
      | otherwise = (t, s)

parseIntegerExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseIntegerExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}}])  
      | getLastExpressionType s == INFIXEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = getLastInfixOperator s, infixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}}}])
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == EMPTYEXP = (
        removeFirstToken t,
        pop s ++ [
          Statement {
              statementType = statementType (last s),
              expression = BoolExpression {
                expressionType = BOOLEXP, 
                leftBool = getLastLeftBool s,
                boolOperator = getLastBoolOperator s,
                rightBool = IntegerLiteralExpression {
                  expressionType = INTEXP, 
                  integerLiteral = literal (head t)
                }
              }
            }
          ]
        )
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == INFIXEXP = (
        removeFirstToken t,
        pop s ++ [
          Statement {
              statementType = statementType (last s),
              expression = BoolExpression {
                expressionType = BOOLEXP, 
                leftBool = getLastLeftBool s,
                boolOperator = getLastBoolOperator s,
                rightBool = InfixExpression {
                  expressionType = INFIXEXP,
                  infixOperator = infixOperator (getLastRightBool s), 
                  infixExpression = IntegerLiteralExpression {
                      expressionType = INTEXP,
                      integerLiteral = literal (head t)
                    }
                }
              }
            }
          ]
      )
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == OPERATOREXP = 
        (
          removeFirstToken t, 
          pop s ++ [
            Statement {
                statementType = statementType (last s), 
                expression = BoolExpression  
                  {
                    expressionType = BOOLEXP,
                    leftBool = getLastLeftBool s,
                    boolOperator = getLastBoolOperator s,
                    rightBool = addExpToLastRightOperator (head t, getLastRightBool s)
                  }
              }
          ]
        )
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
                    rightOperator = addExpToLastRightOperator (head t, getLastRightOperator s)
                  }
              }
          ]
        ) 
      | otherwise = error "failed to parse integer expression"


addExpToLastRightOperator :: (Token, Expression) -> Expression
addExpToLastRightOperator (t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = IntegerLiteralExpression{expressionType = INTEXP, integerLiteral = literal t}
      | expressionType e == INFIXEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = infixOperator e, infixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal t}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addExpToLastRightOperator (t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addExpToLastRightOperator (t, rightBool e)}
      | expressionType e == INTEXP = error "shouldn't call this with int"
      | otherwise = error "addToLastRightOperator"

addInfixToLastRightOperator :: (Token, Expression) ->  Expression 
addInfixToLastRightOperator (t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = t, infixExpression = Expression {expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addInfixToLastRightOperator (t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addInfixToLastRightOperator (t, rightBool e)}
      | otherwise = error "addInfixToLastRightOperator"

parseOperatorExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseOperatorExpression (t, s) = (tok, sta)
  where
    (tok,sta)
      | getLastExpressionType s == INTEXP = (removeFirstToken t, pop s ++[Statement {statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = expression (last s), operator = head t, rightOperator = Expression {expressionType = EMPTYEXP}}}])
      | getLastExpressionType s == OPERATOREXP && checkPrecedence (head t, expression (last s)) == True = 
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
      | getLastExpressionType s == OPERATOREXP && checkPrecedence (head t, expression (last s)) == False = 
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
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == INTEXP = 
        (removeFirstToken t, 
        pop s ++ [
          Statement {statementType = statementType (last s), expression = BoolExpression {
              expressionType = BOOLEXP, 
              leftBool = getLastLeftBool s, 
              boolOperator = getLastBoolOperator s, 
              rightBool = OperatorExpression {
                  expressionType = OPERATOREXP, 
                  leftOperator = getLastRightBool s,
                  operator = head t,
                  rightOperator = Expression {expressionType = EMPTYEXP}
                }
            }}
        ])
      | getLastExpressionType s == BOOLEXP 
        && 
        expressionType (getLastRightBool s) == OPERATOREXP 
        && 
        checkPrecedence (head t, getLastRightBool s) == True 
        = (
            removeFirstToken t, 
            pop s ++ [
              Statement {
                statementType = statementType (last s), 
                expression = BoolExpression {
                    expressionType = BOOLEXP,
                    leftBool = getLastLeftBool s, 
                    boolOperator = getLastBoolOperator s, 
                    rightBool = OperatorExpression { 
                      expressionType = OPERATOREXP,
                      leftOperator = rightOperator (getLastRightBool s),
                      operator = head t,
                      rightOperator = Expression {expressionType = EMPTYEXP}
                    }
                  }
              }
            ]
          )
      | getLastExpressionType s == BOOLEXP 
        && 
        expressionType (getLastRightBool s) == OPERATOREXP 
        && 
        checkPrecedence (head t, getLastRightBool s) == False 
        = (
          removeFirstToken t,
          pop s ++ [
            Statement {
                statementType = statementType (last s),
                expression = BoolExpression {
                    expressionType= BOOLEXP, 
                    leftBool = getLastLeftBool s,
                    boolOperator = getLastBoolOperator s,
                    rightBool = OperatorExpression {
                        expressionType = OPERATOREXP,
                        leftOperator = getLastRightBool s,
                        operator = head t, 
                        rightOperator = Expression {expressionType = EMPTYEXP}
                      }
                  }
              }
          ]
        ) 
      | otherwise = error "error parsing operator exp"

checkPrecedence :: (Token, Expression) -> Bool
checkPrecedence (t, e)= getPrecedence (typ t) > getPrecedence (typ (operator e)) 

parseInfixExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseInfixExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = head t, infixExpression = Expression {expressionType = EMPTYEXP}}}]) 
      | getLastExpressionType s == OPERATOREXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = getLastLeftOperator s, operator = getLastOperator s, rightOperator= InfixExpression {expressionType = INFIXEXP, infixOperator = head t, infixExpression = Expression {expressionType = EMPTYEXP}}}}])
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == OPERATOREXP = (
        removeFirstToken t, 
        pop s ++ [
          Statement {
              statementType = statementType (last s),
              expression = BoolExpression {
              expressionType = BOOLEXP,
              leftBool = getLastLeftBool s, 
              boolOperator = getLastBoolOperator s, 
              rightBool = addInfixToLastRightOperator (head t, getLastRightBool s)
              }
            }
        ]
      )
      | getLastExpressionType s == BOOLEXP = (removeFirstToken t, pop s ++ [
        Statement {
          statementType = statementType (last s), 
          expression = BoolExpression {
            expressionType = BOOLEXP, 
            leftBool = getLastLeftBool s, 
            boolOperator = getLastBoolOperator s, 
            rightBool = InfixExpression {
              expressionType = INFIXEXP, 
              infixOperator = head t, 
              infixExpression = Expression {expressionType = EMPTYEXP }
            }
          }
        }]) 
      | otherwise = error "error parsing infix exp" 


parseBoolExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseBoolExpression (t, s) = (tok ,sta)
  where
    (tok, sta)
      | otherwise = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = BoolExpression {expressionType = BOOLEXP, leftBool = getLastExpression s, boolOperator = head t, rightBool = Expression {expressionType = EMPTYEXP}}}]) 
      -- | otherwise = error "error parsing bool expression"
