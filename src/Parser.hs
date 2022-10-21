module Parser where

import Ast
import Lexer
import Token
import Utils
import ParserUtils


parseProgram :: String -> [Token]
parseProgram s = snd (parseTokens (s, []))

parseStatements :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseStatements (b, (t, s)) = (bok, (tokens, statements))
  where
    (bok, (tokens, statements))
      | null t = (b, (t, s))
      | typ (head t) == LET =
          parseStatements
            (b,
            ( snd (parseExpression
                (b, ( 
                parseIdentifier
                    (b,( removeFirstToken t,
                      addToStatement(b, s, Statement {statementType = LETSTA, statementUni = LetStatement{identifier = ""}, expression = Expression {expressionType = EMPTYEXP}})
                    ))
                )
                ))
            ))
      | typ (head t) == RETURN =
          parseStatements 
            (parseExpression
                (b,( removeFirstToken t,
                  s
                    ++ [ Statement
                           { 
                             statementUni = ReturnStatement {},
                             statementType = RETSTA,  
                             expression = Expression {expressionType = EMPTYEXP}
                           }
                       ]
                )
            ))
      | typ (head t) == IF = parseStatements(parseExpression((EXP,(removeFirstToken t, s ++ [
                Statement {
                  statementType= IFSTA,
                  statementUni = IfStatement {
                      con = [],
                      alt = [] }, 
                      expression = Expression {expressionType = EMPTYEXP}}])
        )))
      | typ (head t) == LBRACE && b == EXP = parseStatements(CON, (removeFirstToken t, s))
      | typ (head t) == LBRACE && b == CON = parseStatements(ALT, (removeFirstToken t, s))
      | typ (head t) == EOF = (b, (removeFirstToken t, s))
      | otherwise = error "error parsing statement" 



parseExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseExpression (b, (t, s)) = (block, (tokens, statements))
  where
    (block, (tokens, statements))
      | null t = (b, (t, s))
      | typ (head t) == INT =
        parseExpression (
          parseIntegerExpression (b, (t, s))
          )
      | 
          isValidMinus (getLastExpression s)  
        && 
          typ (head t) == MINUS = 
            parseExpression (
              parseInfixExpression (b, (t, s))
            )
      | isOperator (head t) = 
        parseExpression (
          parseOperatorExpression (b, (t, s))
          )
      | isValidInfix (head t)=
        parseExpression (
          parseInfixExpression (b, (t,s ))
          )
      | isBoolPrefix (head t) =
        parseExpression (
          parseBoolExpression (b, (t, s))
        )
      | isTF (head t) = 
        parseExpression(
          parseTFExpression (b, (t, s))
        )
      | typ (head t) == LPAREN = 
        parseExpression (
          parseGroupedExpression(b, (t, s))
          )
      | typ (head t) == RBRACE = (b, (removeFirstToken t, s))
      | typ (head t) == RPAREN = 
        parseExpression(b, (
          removeFirstToken t, pop s ++ [
          Statement {
            statementUni = statementUni (last s), 
            statementType = statementType (last s), 
            expression = closeLastExpression (getLastExpression s)}]))
      | typ (head t) == SEMICOLON = (b, (removeFirstToken t, s))
      | typ (head t) == EOF = (b, (removeFirstToken t, s))
      |typ (head t) == LBRACE && b == EXP = (b, (t, s)) 
      | otherwise = error "error parsing expression" 

parseGroupedExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseGroupedExpression (b, (t, s)) = (block, (tok, sta))
  where 
    (block, (tok, sta))
      | typ (head t) == RPAREN = (b, (removeFirstToken t, pop s ++ [Statement {statementUni = statementUni(last s), statementType = statementType (last s), expression = closeLastExpression (getLastExpression s)}]))
      | typ (head t) == EOF || typ (head t) == SEMICOLON = error "found eof/semicolon in grouped exp"
      | getLastExpressionType s == OPERATOREXP = (b, (removeFirstToken t, pop s ++ [Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = addGroupToLastExp(getLastExpression s)}]))
      | getLastExpressionType s == BOOLEXP = (b, (removeFirstToken t, pop s ++ [
        Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = BoolExpression {
            expressionType = BOOLEXP, leftBool = getLastLeftBool s, boolOperator = getLastBoolOperator s, rightBool = addGroupToLastExp (getLastRightBool s) 
          }}
      ]))
      | getLastExpressionType s == EMPTYEXP = (b, (removeFirstToken t, pop s ++ [
        Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = GroupedExpression {expressionType = GROUPEDEXP, groupedExpression = Expression {expressionType = EMPTYEXP}, closed = False}}
      ]))
      | typ (head t) == LPAREN = (b, (removeFirstToken t, pop s ++ [Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = addGroupToLastExp (getLastExpression s)}]))
      | otherwise = error "error parsing grouped exp" 



parseIntegerExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseIntegerExpression (b, (t, s)) = (block, (tokens, statements))
  where
    (block, (tokens, statements))
      | getLastExpressionType s == EMPTYEXP = (b, (removeFirstToken t, addToStatement (b, s, Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}})))
      | getLastExpressionType s == INFIXEXP = (b, (removeFirstToken t, addToStatement (b, s, Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = InfixExpression {expressionType = INFIXEXP, infixOperator = getLastInfixOperator s, infixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal (head t)}}})))
      | getLastExpressionType s == BOOLEXP =  
        (b, (
          removeFirstToken t, 
            addToStatement (b, s, 
            Statement {
                statementUni = statementUni (last s), 
                statementType = statementType (last s), 
                expression = BoolExpression  
                  {
                    expressionType = BOOLEXP,
                    leftBool = getLastLeftBool s,
                    boolOperator = getLastBoolOperator s,
                    rightBool = addIntToLastExp(head t, getLastRightBool s)
                  }
              }
          
        ))
        )
      | getLastExpressionType s == OPERATOREXP = 
        (b, (
          removeFirstToken t, 
            addToStatement (b, s, 
            Statement 
              {
                statementUni = statementUni (last s), 
                statementType = statementType (last s), 
                expression = OperatorExpression 
                  {
                    expressionType = OPERATOREXP,
                    leftOperator = leftOperator (getLastExpression s),
                    operator = getLastOperator s,
                    rightOperator = addIntToLastExp(head t, getLastRightOperator s)
                  }
              }
          )
          )
        ) 
        
      | getLastExpressionType s == GROUPEDEXP = (b, (removeFirstToken t, addToStatement (b, s,  
        Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = GroupedExpression {
            expressionType = GROUPEDEXP, closed = closed (getLastExpression s), groupedExpression = addIntToLastExp (head t, groupedExpression (getLastExpression s ))
          }}
      )))
      | otherwise = error "failed to parse integer expression"


parseOperatorExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseOperatorExpression (b, (t, s)) = (block, (tok, sta))
  where
    (block, (tok,sta))
      | getLastExpressionType s == INTEXP = (b, (removeFirstToken t, addToStatement (b, s, Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = expression (last s), operator = head t, rightOperator = Expression {expressionType = EMPTYEXP}}})))
      | getLastExpressionType s == OPERATOREXP = 
        (b, (removeFirstToken t, addToStatement (b, s, 
          Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = addOperatorToLastExp(head t, getLastExpression s)}   
        )))
      | getLastExpressionType s == INFIXEXP = (b, (removeFirstToken t, addToStatement (b, s, 
        Statement{
          statementUni = statementUni (last s),
          statementType = statementType (last s),
          expression = addOperatorToLastExp (head t, getLastExpression s) }
          )
        )
        )
      | getLastExpressionType s == BOOLEXP = (b, (removeFirstToken t, addToStatement (b, s,  
        Statement {
          statementUni = statementUni (last s),
          statementType = statementType (last s),
          expression = BoolExpression {
              expressionType = BOOLEXP,
              leftBool = getLastLeftBool s,
              boolOperator = getLastBoolOperator s,
              rightBool = addOperatorToLastExp (head t, getLastRightBool s)}}
              )
          )
        )
      | getLastExpressionType s == GROUPEDEXP && findGrouped(getLastExpression s) == False = (b, (removeFirstToken t, addToStatement(b, s, Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = OperatorExpression {expressionType = OPERATOREXP, leftOperator = getLastExpression s, operator = head t, rightOperator = Expression {expressionType= EMPTYEXP }}})))
      | getLastExpressionType s == GROUPEDEXP = (b, (removeFirstToken t, addToStatement(b,s, Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = GroupedExpression {expressionType = GROUPEDEXP, closed= closed (getLastExpression s), groupedExpression = addOperatorToLastExp(head t, groupedExpression (getLastExpression s))}})))
      | otherwise = error "error parsing operator exp"


parseInfixExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseInfixExpression (b, (t, s)) = (block, (tokens, statements))
  where
    (block, (tokens, statements))
      | getLastExpressionType s == EMPTYEXP = (b, (removeFirstToken t, addToStatement(b,s, 
        Statement {
          statementUni = statementUni (last s),
          statementType = statementType (last s),
          expression = InfixExpression {expressionType = INFIXEXP,
          infixOperator = head t, infixExpression = Expression {expressionType = EMPTYEXP}}})))
      | getLastExpressionType s == OPERATOREXP = (b, (
        removeFirstToken t, addToStatement(b,s, 
          Statement {
            statementUni = statementUni (last s), 
            statementType = statementType (last s), 
            expression = OperatorExpression {
              expressionType = OPERATOREXP,
              leftOperator = getLastLeftOperator s,
              operator = getLastOperator s,
              rightOperator= addInfixToLastExp (head t, getLastRightOperator s)}})))
      | getLastExpressionType s == BOOLEXP = (b, (
        removeFirstToken t, 
          addToStatement(b,s, 
          Statement {
              statementUni = statementUni (last s),
              statementType = statementType (last s),
              expression = BoolExpression {
              expressionType = BOOLEXP,
              leftBool = getLastLeftBool s, 
              boolOperator = getLastBoolOperator s, 
              rightBool = addInfixToLastExp(head t, getLastRightBool s)
              }})))
      | getLastExpressionType s == GROUPEDEXP = (b, (removeFirstToken t, addToStatement(b,s,Statement {statementUni = statementUni (last s),statementType = statementType (last s),expression = GroupedExpression {expressionType = GROUPEDEXP,closed = closed (getLastExpression s),groupedExpression = addInfixToLastExp (head t, (groupedExpression (getLastExpression s)))}}))) 
      | otherwise = error "error parsing infix exp" 


parseBoolExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseBoolExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement(b,s,Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = addBoolToLastExp (head t, getLastExpression s)})))

parseTFExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseTFExpression (b, (t,s )) = (block, (tok ,sta))
  where
    (block, (tok ,sta))
      | getLastExpressionType s == EMPTYEXP = (b, (removeFirstToken t,s)) 
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == EMPTYEXP = (b, (removeFirstToken t, addToStatement(b,s,Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = BoolExpression {expressionType = BOOLEXP, leftBool = getLastLeftBool s, boolOperator = getLastBoolOperator s, rightBool = TFExpression {expressionType = TFEXP, bool = typ (head t)}}})))
      | getLastExpressionType s == BOOLEXP && expressionType (getLastRightBool s) == BOOLEXP = (b, (removeFirstToken t, addToStatement(b,s, 
        Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = BoolExpression {expressionType = BOOLEXP, leftBool = getLastLeftBool s, boolOperator = getLastOperator s, rightBool = addTFToLastBool (head t, getLastRightBool s)}}
      ))) 
      | getLastExpressionType s == INFIXEXP && typ (getLastInfixOperator s) == BANG = (b, (removeFirstToken t,addToStatement(b,s, 
        Statement {statementUni = statementUni (last s), statementType = statementType (last s), expression = InfixExpression{expressionType = INFIXEXP, infixOperator = getLastInfixOperator s, infixExpression = TFExpression {expressionType = TFEXP, bool = typ (head t)}}}
      ))) 
      | otherwise = error "error parsing TF"
