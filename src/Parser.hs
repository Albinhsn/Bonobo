module Parser where

import Ast
import Lexer
import Token
import Utils
import ParserUtils


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
          isValidMinus (getLastExpression s)  
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
      | isTF (head t) = 
        parseExpression(
          parseTFExpression (t, s) 
        )
      | typ (head t) == LPAREN = 
        parseExpression (
          parseGroupedExpression(t, s)
          )
      | typ (head t) == RPAREN = 
        parseExpression(
          removeFirstToken t, pop s ++ [
          Statement {
            statementType = statementType (last s), 
            expression = closeLastExpression (getLastExpression s)}])
      | typ (head t) == SEMICOLON = (removeFirstToken t, s)
      | otherwise = (t, s)

parseGroupedExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseGroupedExpression (t, s) = (tok, sta)
  where 
    (tok, sta)
      | typ (head t) == RPAREN = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = closeLastExpression (getLastExpression s)}])
      | typ (head t) == EOF || typ (head t) == SEMICOLON = error "found eof/semicolon in grouped exp"
      | getLastExpressionType s == OPERATOREXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = addGroupToLastExp(getLastExpression s)}])
      | getLastExpressionType s == BOOLEXP = (removeFirstToken t, pop s ++ [
        Statement {statementType = statementType (last s), expression = BoolExpression {
            expressionType = BOOLEXP, leftBool = getLastLeftBool s, boolOperator = getLastBoolOperator s, rightBool = addGroupToLastExp (getLastRightBool s) 
          }}
      ])
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t, pop s ++ [
        Statement {statementType = statementType (last s), expression = GroupedExpression {expressionType = GROUPEDEXP, groupedExpression = Expression {expressionType = EMPTYEXP}, closed = False}}
      ])
      | otherwise = error "error parsing grouped exp" 



parseIntegerExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseIntegerExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}}])  
      | getLastExpressionType s == INFIXEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = getLastInfixOperator s, infixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}}}])
      | getLastExpressionType s == BOOLEXP =  
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
                    rightBool = addIntToLastExp(head t, getLastRightBool s)
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
                    rightOperator = addIntToLastExp(head t, getLastRightOperator s)
                  }
              }
          ]
        ) 
      | getLastExpressionType s == GROUPEDEXP = (removeFirstToken t, pop s ++ [
        Statement {statementType = statementType (last s), expression = GroupedExpression {
            expressionType = GROUPEDEXP, closed = closed (getLastExpression s), groupedExpression = addIntToLastExp (head t, groupedExpression (getLastExpression s ))
          }}
      ])
      | otherwise = error "failed to parse integer expression"


parseOperatorExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseOperatorExpression (t, s) = (tok, sta)
  where
    (tok,sta)
      | getLastExpressionType s == INTEXP = (removeFirstToken t, pop s ++[Statement {statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = expression (last s), operator = head t, rightOperator = Expression {expressionType = EMPTYEXP}}}])
      | getLastExpressionType s == OPERATOREXP && opHasNoGroup (getLastExpression s) && checkPrecedence (head t, expression (last s)) == True = 
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
      | getLastExpressionType s == OPERATOREXP && opHasNoGroup (getLastExpression s) && checkPrecedence (head t, expression (last s)) == False = 
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
      | getLastExpressionType s == OPERATOREXP = 
        (removeFirstToken t, pop s ++ [
          Statement {statementType = statementType (last s), expression = OperatorExpression {
              expressionType = OPERATOREXP,
              leftOperator = leftOperator (getLastExpression s),
              operator = getLastOperator s,
              rightOperator = addOperatorToLastExp (head t, rightOperator (getLastExpression s))
            }}
        ])
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == INTEXP = 
        (removeFirstToken t, 
        pop s ++ [
          Statement {statementType = statementType (last s), expression = BoolExpression {
              expressionType = BOOLEXP, 
              leftBool = getLastLeftBool s, 
              boolOperator = getLastBoolOperator s, 
              rightBool = addOperatorToLastExp (head t, getLastRightBool s)
              }}
        ])
      | getLastExpressionType s == BOOLEXP 
        && 
        expressionType (getLastRightBool s) == OPERATOREXP 
        = (
            removeFirstToken t, 
            pop s ++ [
              Statement {
                statementType = statementType (last s), 
                expression = BoolExpression {
                    expressionType = BOOLEXP,
                    leftBool = getLastLeftBool s, 
                    boolOperator = getLastBoolOperator s, 
                    rightBool = addOperatorToLastExp (head t, getLastRightBool s) 
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
      | getLastExpressionType s == GROUPEDEXP && closed (getLastExpression s) == True = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = groupedExpression (getLastExpression s), operator = head t, rightOperator = Expression {expressionType= EMPTYEXP }}}])
      | getLastExpressionType s == GROUPEDEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = GroupedExpression {expressionType = GROUPEDEXP, closed= closed (getLastExpression s), groupedExpression = addOperatorToLastExp(head t, groupedExpression (getLastExpression s))}}])
      | otherwise = error "error parsing operator exp"


parseInfixExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseInfixExpression (t, s) = (tokens, statements)
  where
    (tokens, statements)
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = head t, infixExpression = Expression {expressionType = EMPTYEXP}}}]) 
      | getLastExpressionType s == OPERATOREXP && expressionType (rightOperator (getLastExpression s)) == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = getLastLeftOperator s, operator = getLastOperator s, rightOperator = InfixExpression {expressionType = INFIXEXP, infixOperator = head t, infixExpression = Expression {expressionType = EMPTYEXP}}}}]) 
      | getLastExpressionType s == OPERATOREXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = getLastLeftOperator s, operator = getLastOperator s, rightOperator= addInfixToLastExp (head t, getLastExpression s)}}])
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == OPERATOREXP = (
        removeFirstToken t, 
        pop s ++ [
          Statement {
              statementType = statementType (last s),
              expression = BoolExpression {
              expressionType = BOOLEXP,
              leftBool = getLastLeftBool s, 
              boolOperator = getLastBoolOperator s, 
              rightBool = addInfixToLastExp(head t, getLastRightBool s)
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
      | getLastExpressionType s == GROUPEDEXP = (removeFirstToken t, pop s ++ [
        Statement {
            statementType = statementType (last s),
            expression = GroupedExpression {
                expressionType = GROUPEDEXP,
                closed = closed (getLastExpression s),
                groupedExpression = addInfixToLastExp (head t, getLastExpression s)
              }
          }
      ])
      | otherwise = error "error parsing infix exp" 


parseBoolExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseBoolExpression (t, s) = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = BoolExpression {expressionType = BOOLEXP, leftBool = getLastExpression s, boolOperator = head t, rightBool = Expression {expressionType = EMPTYEXP}}}]) 
  -- where
  --   (tok, sta)
  --     | otherwise = 

parseTFExpression :: ([Token], [Statement]) -> ([Token], [Statement])
parseTFExpression (t,s ) = (tok ,sta)
  where
    (tok ,sta)
      | getLastExpressionType s == EMPTYEXP = (removeFirstToken t,s) 
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == EMPTYEXP = (removeFirstToken t, pop s ++ [Statement {statementType = statementType (last s), expression = BoolExpression {expressionType = BOOLEXP, leftBool = getLastLeftBool s, boolOperator = getLastBoolOperator s, rightBool = TFExpression {expressionType = TFEXP, bool = typ (head t)}}}]) 
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == BOOLEXP = (removeFirstToken t, pop s ++ [
        Statement {statementType = statementType (last s), expression = BoolExpression {expressionType = BOOLEXP, leftBool = getLastLeftBool s, boolOperator = getLastOperator s, rightBool = addTFToLastBool (head t, getLastRightBool s)}}
      ]) 
      | getLastExpressionType s == INFIXEXP && typ (getLastInfixOperator s) == BANG = (removeFirstToken t, pop s ++ [
        Statement {statementType = statementType (last s), expression = InfixExpression{expressionType = INFIXEXP, infixOperator = getLastInfixOperator s, infixExpression = TFExpression {expressionType = TFEXP, bool = typ (head t)}}}
      ]) 
      | otherwise = error "error parsing TF"
