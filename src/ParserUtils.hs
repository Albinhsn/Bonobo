module ParserUtils where 

import Ast 
import Token 
import Lexer 
import Utils 

checkPrecedence :: (Token, Expression) -> Bool
checkPrecedence (t, e)= getPrecedence (typ t) > getPrecedence (typ (operator e)) 

checkBoolPrecedence :: (Token, Expression) -> Bool
checkBoolPrecedence (t, e)= getPrecedence (typ t) >= getPrecedence (typ (boolOperator e)) 

addIntToLastExp:: (Token, Expression) -> Expression
addIntToLastExp(t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = IntegerLiteralExpression{expressionType = INTEXP, integerLiteral = literal t}
      | expressionType e == INFIXEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = infixOperator e, infixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal t}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIntToLastExp(t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIntToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP && expressionType (groupedExpression e) == EMPTYEXP= GroupedExpression {expressionType = GROUPEDEXP, closed= False, groupedExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal t}}
      | expressionType e == GROUPEDEXP = GroupedExpression {expressionType = GROUPEDEXP, closed= False, groupedExpression = addIntToLastExp (t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIntToLastExp(t, assignExpression e)}
      | expressionType e == INTEXP = error "shouldn't call this with int"
      | otherwise = error "addIntToLastExp"

addBoolToLastExp :: (Token, Expression) -> Expression
addBoolToLastExp (t, e) = exp
  where
    exp 
      | expressionType e == INTEXP || expressionType e == INFIXEXP ||expressionType e == OPERATOREXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addBoolToLastExp (t, rightBool e)} 
      | expressionType e == GROUPEDEXP && closed e == False = GroupedExpression {expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addBoolToLastExp (t, groupedExpression e)} 
      | expressionType e == GROUPEDEXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addBoolToLastExp(t, assignExpression e)}
      | otherwise = error "addBoolToLastExp"


addOperatorToLastExp:: (Token, Expression) -> Expression 
addOperatorToLastExp(t, e) = exp
  where
    exp
      | expressionType e == INTEXP || expressionType e == INFIXEXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression {expressionType = EMPTYEXP}}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addOperatorToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP && closed e == True = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e,operator = t, rightOperator = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addOperatorToLastExp(t, groupedExpression e)}
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) == GROUPEDEXP && closed (rightOperator e) == True && checkPrecedence(t, e) == False = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression {expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) /= INTEXP && expressionType (rightOperator e) /= INFIXEXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addOperatorToLastExp(t, rightOperator e))}
      | expressionType e == OPERATOREXP && checkPrecedence (t, e) == True = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = OperatorExpression {expressionType = OPERATOREXP, leftOperator = rightOperator e, operator = t, rightOperator = Expression {expressionType = EMPTYEXP}}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e,operator = t,rightOperator = Expression {expressionType = EMPTYEXP}}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addOperatorToLastExp(t, assignExpression e)}
      | expressionType e == EMPTYEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = t, infixExpression = Expression {expressionType = EMPTYEXP}}
      | otherwise = error "addOperatorToLastExpression" 

addInfixToLastExp:: (Token, Expression) ->  Expression 
addInfixToLastExp(t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = t, infixExpression = Expression {expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addInfixToLastExp(t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addInfixToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addInfixToLastExp(t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addInfixToLastExp(t, assignExpression e)}
      | otherwise = error "addInfixToLastRightOperator"

addTFToLastBool :: (Token, Expression) -> Expression 
addTFToLastBool (t, e) = exp
  where
    exp
      | expressionType e == BOOLEXP && expressionType (rightBool e) == EMPTYEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = TFExpression {expressionType = TFEXP, bool = typ t}} 
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addTFToLastBool(t, rightBool e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addTFToLastBool(t, assignExpression e)}
      | otherwise = error "addTFToLastBool"

isTF :: Token -> Bool
isTF t = typ t == FALSE || typ t == TRUE

addGroupToLastExp :: Expression -> Expression
addGroupToLastExp e = exp
  where 
    exp 
      | expressionType e == EMPTYEXP = GroupedExpression {expressionType = GROUPEDEXP, closed=False, groupedExpression = Expression{expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = addGroupToLastExp (rightOperator e)}
      | expressionType e == BOOLEXP =  BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addGroupToLastExp( rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addGroupToLastExp (groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addGroupToLastExp(assignExpression e)}
      | otherwise = error "error AddingGroupToLastExp"


closeLastGrouped :: Expression -> Expression
closeLastGrouped e = exp 
  where 
    exp 
      -- Found last grouped  
      | expressionType e == GROUPEDEXP && findGrouped (groupedExpression e) == False = GroupedExpression {expressionType = GROUPEDEXP, closed = True, groupedExpression = groupedExpression e}  
      -- Found grouped nested 
      | expressionType e == GROUPEDEXP = GroupedExpression {closed = closed e, expressionType = GROUPEDEXP, groupedExpression = closeLastGrouped (groupedExpression e)}

      --Exists deeper inside left
      | expressionType e == OPERATOREXP && findGrouped (leftOperator e) == True = OperatorExpression {expressionType = OPERATOREXP, operator = operator e, leftOperator = closeLastGrouped (leftOperator e), rightOperator = rightOperator e}   
      | expressionType e == BOOLEXP && findGrouped (leftBool e) == True = BoolExpression {expressionType = BOOLEXP, leftBool = closeLastGrouped (leftBool e), boolOperator = boolOperator e, rightBool = rightBool e} 
      -- Found grouped in right 
      | expressionType e == OPERATOREXP && findGrouped (rightOperator e) == True= OperatorExpression {expressionType = OPERATOREXP, operator = operator e, rightOperator= closeLastGrouped (rightOperator e), leftOperator = leftOperator e} 
      | expressionType e == BOOLEXP && findGrouped (rightBool e) ==True = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = closeLastGrouped (rightBool e)}
      -- didn't find 
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = closeLastGrouped(assignExpression e)}
      | otherwise = error "couldn't close last grouped" 



findGrouped :: Expression -> Bool 
findGrouped e = b 
  where
    b 
      -- Found grouped 
      | expressionType e == GROUPEDEXP && closed e == False = True 
      | expressionType e == GROUPEDEXP = findGrouped (groupedExpression e)  
      | expressionType e == OPERATOREXP && expressionType (leftOperator e) == GROUPEDEXP && closed (leftOperator e) == False= True 
      | expressionType e == BOOLEXP && expressionType (leftBool e) == GROUPEDEXP && closed (leftBool e) == False= True 
      -- Check left if possible
      | expressionType e == OPERATOREXP && expressionType (leftOperator e) == OPERATOREXP && findGrouped (leftOperator e) == True = True 
      | expressionType e == OPERATOREXP && expressionType (leftOperator e) == BOOLEXP && findGrouped (leftBool e) == True =True  
      | expressionType e == BOOLEXP && expressionType (leftBool e) == BOOLEXP && findGrouped (leftBool e)= True 
      | expressionType e == BOOLEXP && expressionType (leftBool e) == OPERATOREXP && findGrouped (leftOperator e)=True 
      -- Found grouped in right 
      | expressionType e == OPERATOREXP && expressionType (rightOperator e)  == GROUPEDEXP  && closed (rightOperator e) == False= True 
      | expressionType e == BOOLEXP && expressionType (rightBool e) == GROUPEDEXP && closed (rightBool e) == False= True 
      --Check if right is possible
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) == OPERATOREXP && findGrouped (rightOperator e) = True 
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) == BOOLEXP && findGrouped (rightBool e) = True
      | expressionType e == BOOLEXP && expressionType (rightBool e) == BOOLEXP && findGrouped (rightBool e)= True 
      | expressionType e == BOOLEXP && expressionType (rightBool e) == OPERATOREXP && findGrouped (rightOperator e)=True  
      -- didn't find 
      | otherwise = False

opHasNoGroup:: Expression -> Bool 
opHasNoGroup e = expressionType (rightOperator e) /= GROUPEDEXP

noPopAddToStatement:: (BlockType, [Statement], Statement) -> [Statement]
noPopAddToStatement(b, s, sta) = 
  case b of 
    CON -> pop s ++ [
      Statement {
        statementType = statementType (last s), 
        statementUni = IfStatement {
          alt = alt (statementUni (last s)),
          closedAlt = closedAlt (statementUni (last s)),
          closedCon = closedCon (statementUni (last s)),
          con = (con (statementUni (last s))) ++ [sta]},
        expression = expression (last s) 
        }] 
    ALT -> pop s ++ [
      Statement {
        statementType = statementType (last s), 
        statementUni = IfStatement {
          closedAlt = closedAlt (statementUni (last s)),
          alt = alt (statementUni (last s)) ++ [sta],
          con = (con (statementUni (last s))),
          closedCon = closedCon (statementUni (last s))
        },
        expression = expression (last s) 
        }] 
    BOD ->  pop s ++ [
      Statement {
        statementType = statementType (last s), 
        statementUni = FuncStatement{
          params = params (statementUni (last s)),
          body = body (statementUni (last s)) ++ [sta] 
        },
        expression = expression (last s) 
        }] 
    EXP -> pop s ++ [sta] 
addToStatement :: (BlockType, [Statement], Statement) -> [Statement]
addToStatement (b, s, sta) = 
  case b of 
    CON -> pop s ++ [
      Statement {
        statementType = statementType (last s), 
        statementUni = IfStatement {
          alt = alt (statementUni (last s)),
          closedAlt = closedAlt (statementUni (last s)),
          closedCon = closedCon (statementUni (last s)),
          con = pop (con (statementUni (last s))) ++ [sta]},
        expression = expression (last s) 
        }] 
    ALT -> pop s ++ [
      Statement {
        statementType = statementType (last s), 
        statementUni = IfStatement {
          closedAlt = closedAlt (statementUni (last s)),
          alt = pop (alt (statementUni (last s))) ++ [sta],
          con = con (statementUni (last s)),
          closedCon = closedCon (statementUni (last s))
        },
        expression = expression (last s) 
        }] 
    PAR -> pop s ++ [
      Statement {
          statementType = statementType(last s),
          statementUni = FuncStatement{
              params = pop (params (statementUni (last s))) ++ [last (params(statementUni(sta)))],
              body = []
            },
          expression = expression (last s)
        }
      ]
    BOD -> pop s ++ [
      Statement {
          statementType = statementType(last s),
          statementUni = FuncStatement{
              params = params (statementUni (last s)),
              body = pop (body (statementUni (last s))) ++ [last (body (statementUni(sta)))]
            },
          expression = expression (last s)
        }
      ]
    EXP -> pop s ++ [sta] 


addToLastStatement :: (BlockType, Token, ExpressionType, [Statement]) ->Statement 
addToLastStatement (b, t, e, s) = sta 
  where 
    sta 
      | statementType (last s) /= IFSTA && (b /= PAR && b /= BOD) = Statement{statementType = statementType (last s), statementUni = statementUni (last s), expression = addXToExp (t, e, expression (last s))}
      | b == EXP && null (con (statementUni (last s))) = Statement{statementType = statementType (last s), statementUni = statementUni (last s), expression = addXToExp (t, e, expression (last s))}
      | b == BOD = Statement {
          statementType = statementType (last s), 
          statementUni = FuncStatement{
            params = params (statementUni (last s)),
            body = pop (body (statementUni (last s))) ++ [addToLastStatement(EXP, t, e, body(statementUni(last s)))]}, 
          expression = expression (last s)
          }
      | b == PAR = Statement{
        statementType = FUNCSTA,
        statementUni = FuncStatement{
          params = pop (params (statementUni (last s))) ++ [addXToExp(t, e, getLastParam(params (statementUni(last s))))],
          body = []
          },
        expression = expression (last s)
        } 
      --Assumes if it's null both other checks covers EXP
      | (b == CON || b == ALT ) && closedCon (statementUni (last s)) == False = addToLastStatement(b,t,e, con(statementUni (last s))) 
      | (b == CON || b == ALT ) && closedAlt (statementUni (last s)) == False = addToLastStatement(b,t,e, alt(statementUni (last s))) 
      | otherwise = error "addToLastStatement" 

getLastParam :: [Expression] -> Expression 
getLastParam e = 
  case e of 
    [] -> Expression{expressionType = EMPTYEXP}
    _ -> last e


addXToExp :: (Token, ExpressionType, Expression) -> Expression 
addXToExp (t,et, e) = exp 
  where 
    exp 
      | typ t == INT = addIntToLastExp(t, e)
      | typ t == MINUS && et == INFIXEXP =  addInfixToLastExp(t, e)
      | typ t == MINUS = addOperatorToLastExp(t,e)
      | typ t == BANG = addInfixToLastExp(t, e)
      | typ t == TRUE =  addTFToLastBool(t, e)
      | typ t == FALSE = addTFToLastBool(t, e)
      | typ t == LPAREN = addGroupToLastExp(e)
      | typ t == RPAREN = closeLastGrouped(e)
      | typ t == PLUS = addOperatorToLastExp(t, e)
      | typ t == SLASH = addOperatorToLastExp(t, e)
      | typ t == ASTERISK = addOperatorToLastExp(t, e)
      | typ t == LESS_T = addBoolToLastExp(t,e)
      | typ t == GREATER_T = addBoolToLastExp(t,e)
      | typ t == EQUALS = addBoolToLastExp(t,e)
      | typ t == NOT_EQUALS= addBoolToLastExp(t,e)
      | typ t == IDENT = addIdentifierToLastExp(t, e)
      | typ t == ASSIGN = addAssignToLastExp(e)
      | otherwise = error "addXToExp"

addAssignToLastExp:: Expression -> Expression 
addAssignToLastExp e = exp 
  where
    exp  
      | expressionType e == IDENTEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = e, assignExpression = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == EMPTYEXP = Expression {expressionType = EMPTYEXP}
      | otherwise = error "parseAssignToLastExp"

parseIdentifierToLet :: [Token] -> Statement
parseIdentifierToLet t = s
  where
    s  
      | typ (head t) == IDENT = Statement {statementType = LETSTA, statementUni= LetStatement {identifier = literal (head t)}, expression = Expression {expressionType = EMPTYEXP}}
      | otherwise = error "parseIdentifierToLet"

addIdentifierToLastExp:: (Token, Expression) ->  Expression 
addIdentifierToLastExp(t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = IdentExpression{expressionType = IDENTEXP,  ident = literal t}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIdentifierToLastExp(t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIdentifierToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addIdentifierToLastExp(t, groupedExpression e)}
      | otherwise = error "addIdentifierToLastExp"

closeLastOpen:: (BlockType, [Statement]) -> [Statement]
closeLastOpen(b, s) = sta 
  where   
    sta
      | statementType (last s) == IFSTA && closedCon (statementUni (last s)) == False && findLastOpen(con(statementUni(last s))) == False = 
        pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
            closedCon = True,
            closedAlt = False,
            con = con (statementUni (last s)),
            alt = alt (statementUni (last s))},
            expression = expression (last s)}
          ] 
      | statementType (last s) == IFSTA && b == CON = pop s ++ [Statement{
        statementType = statementType (last s),
        expression = expression (last s),
        statementUni = IfStatement {
          closedCon = True,
          closedAlt = False,
          con = con (statementUni (last s)),
          alt = closeLastOpen(b, alt(statementUni (last s)))}}]
      | statementType (last s) == IFSTA && closedAlt(statementUni (last s)) == False && findLastOpen(alt(statementUni(last s))) == False = 
        pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
            closedCon = True,
            closedAlt = True,
            con = con (statementUni (last s)),
            alt = alt (statementUni (last s))},
            expression = expression (last s)}
          ] 
      | statementType (last s) == IFSTA && b == ALT= pop s ++ [Statement{
        statementType = statementType (last s),
        expression = expression (last s),
        statementUni = IfStatement {
          closedCon = True,
          closedAlt = False,
          con = con (statementUni (last s)),
          alt = closeLastOpen(b, alt(statementUni (last s)))}}]
      | otherwise = error "closeLastIf"

findLastOpen:: ([Statement]) -> Bool
findLastOpen(s) = bool
  where 
    bool
      | null s == True = False
      | statementType (last s) == IFSTA && closedCon (statementUni (last s)) == False = True 
      | statementType (last s) == IFSTA && closedAlt (statementUni (last s)) == False = True 
      | otherwise = False


changeSta :: (Token, [Statement]) -> [Statement]
changeSta (t,s) = sta 
  where   
    sta 
      | statementType (last s) == NOSTA && typ t == ASSIGN = pop s ++ [Statement{
          statementType = ASSIGNSTA,
          statementUni = AssignStatement{}, 
          expression = expression (last s)
        }]
      | statementType (last s) == NOSTA && typ t == LPAREN = pop s ++ [Statement{
          statementType = FUNCSTA,
          statementUni = FuncStatement{params = [], body = []},
          expression = expression (last s) 
        }]
      | otherwise = s 
changeBlock :: (BlockType, Token, [Statement]) -> BlockType 
changeBlock (b, t,s) = sta 
  where   
    sta 
      | statementType (last s) == ASSIGNSTA && typ t == LPAREN && b == EXP = PAR 
      | otherwise = b 

changeFuncToCall :: [Statement] -> [Statement] 
changeFuncToCall s = sta
  where
    sta   
      | statementType (last s) == FUNCSTA && null (body (statementUni(last s))) = pop s ++ [Statement{
          statementUni=CallStatement{callParams = params (statementUni(last s))},
          statementType=CALLSTA, 
          expression = expression (last s)
          }]
      | otherwise = error "changeFuncToCall"
-- TODO USE THIS 
findLastFunc :: [Statement] -> Bool 
findLastFunc s = True

paramHasOpen :: [Statement] -> Bool
paramHasOpen s = b
  where 
    b 
      | null s == True = False 
      | 
      -- | statementType (last s) == CALLSTA  
      | otherwise = False
