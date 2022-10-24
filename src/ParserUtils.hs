module ParserUtils where 

import Ast 
import Token 
import Lexer 
import Utils 

checkPrecedence :: (Token, Expression) -> Bool
checkPrecedence (t, e)= getPrecedence (typ t) > getPrecedence (typ (operator e)) 

checkBoolPrecedence :: (Token, Expression) -> Bool
checkBoolPrecedence (t, e)= getPrecedence (typ t) >= getPrecedence (typ (boolOperator e)) 

addIntToLastExp:: (BlockType, Token, Expression) -> Expression
addIntToLastExp(b, t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = IntegerLiteralExpression{expressionType = INTEXP, integerLiteral = literal t}
      | expressionType e == INFIXEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = infixOperator e, infixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal t}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIntToLastExp(b, t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIntToLastExp(b, t, rightBool e)}
      | expressionType e == GROUPEDEXP && expressionType (groupedExpression e) == EMPTYEXP= GroupedExpression {expressionType = GROUPEDEXP, closed= False, groupedExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = literal t}}
      | expressionType e == GROUPEDEXP = GroupedExpression {expressionType = GROUPEDEXP, closed= False, groupedExpression = addIntToLastExp (b, t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIntToLastExp(b, t, assignExpression e)}
      | expressionType e == CALLEXP = CallExpression {expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addIntToLastExp(b, t, last(callParams e))]}
      | expressionType e == IDENTEXP && b == PAR= CallExpression {
            expressionType = CALLEXP,
            callParams = [IntegerLiteralExpression{expressionType = INTEXP, integerLiteral = literal t}],  
            callIdent= e,
            closedCall = False 
          }
      | expressionType e == INTEXP = error "shouldn't call this with int"
      | otherwise = error "addIntToLastExp"

addBoolToLastExp :: (Token, Expression) -> Expression
addBoolToLastExp (t, e) = exp
  where
    exp 
      | expressionType e == IDENTEXP || expressionType e == CALLEXP || expressionType e == INTEXP || expressionType e == INFIXEXP ||expressionType e == OPERATOREXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addBoolToLastExp (t, rightBool e)} 
      | expressionType e == GROUPEDEXP && closed e == False = GroupedExpression {expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addBoolToLastExp (t, groupedExpression e)} 
      | expressionType e == GROUPEDEXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addBoolToLastExp(t, assignExpression e)}
      | otherwise = error "addBoolToLastExp"


addOperatorToLastExp:: (Token, Expression) -> Expression 
addOperatorToLastExp(t, e) = exp
  where
    exp
      | expressionType e == IDENTEXP || expressionType e == INTEXP || expressionType e == INFIXEXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression {expressionType = EMPTYEXP}}
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
    CON -> pop s ++ [sta] 
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
    PAR -> pop s ++ [sta]
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
      | statementType (last s) /= IFSTA && (b /= PAR && b /= BOD) =Statement{statementType = statementType (last s), statementUni = statementUni (last s), expression = addXToExp (b, t, e, expression (last s))}
      | b == EXP && null (con (statementUni (last s))) = Statement{statementType = statementType (last s), statementUni = statementUni (last s), expression = addXToExp (b, t, e, expression (last s))}
      | b == BOD = Statement {
          statementType = statementType (last s), 
          statementUni = FuncStatement{
            params = params (statementUni (last s)),
            body = pop (body (statementUni (last s))) ++ [addToLastStatement(EXP, t, e, body(statementUni(last s)))]}, 
          expression = expression (last s)
          }
      | b == PAR  && statementType (last s) == FUNCSTA = Statement{
        statementType = FUNCSTA,
        statementUni = FuncStatement{
          params = pop (params (statementUni (last s))) ++ [addXToExp(b, t, e, getLastParam(params (statementUni(last s))))],
          body = []
          },
        expression = expression (last s)
        } 
      | b == PAR = Statement{statementType = statementType (last s), statementUni = statementUni (last s), expression = addXToExp(b, t,e,expression (last s))}
      --Assumes if it's null both other checks covers EXP
      | b == CON && null (con(statementUni(last s))) = Statement{statementType = IFSTA, statementUni = statementUni (last s), expression = addXToExp(EXP, t, e, expression (last s))} 
      |  (b == CON || b == ALT ) && closedCon (statementUni (last s)) == False = Statement{
          statementType = IFSTA,
          statementUni = IfStatement{
            closedCon = False,
            closedAlt = False,
            con = pop (con (statementUni(last s))) ++ [addToLastStatement(b,t,e, con(statementUni (last s)))],
            alt = []
            },
          expression = expression (last s)
        }
      |  statementType (last s) == IFSTA && b == ALT && closedAlt (statementUni (last s)) == False = addToLastStatement(b,t,e, alt(statementUni (last s))) 
      |  statementType (last s) == IFSTA && b == CON && closedAlt (statementUni (last s)) == False = addToLastStatement(b,t,e, con(statementUni (last s))) 
      | otherwise = error (statementToString(last (alt (statementUni (last s)))))




getLastParam :: [Expression] -> Expression 
getLastParam e = 
  case e of 
    [] -> Expression{expressionType = EMPTYEXP}
    _ -> last e



addXToExp :: (BlockType, Token, ExpressionType, Expression) -> Expression 
addXToExp (b, t,et, e) = exp 
  where 
    exp 
      | typ t == INT = addIntToLastExp(b, t, e)
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
      | typ t == IDENT = addIdentifierToLastExp(b, t, e)
      | typ t == ASSIGN = addAssignToLastExp(e)
      | otherwise = error (literal t)

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

addIdentifierToLastExp:: (BlockType, Token, Expression) ->  Expression 
addIdentifierToLastExp(b, t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = IdentExpression{expressionType = IDENTEXP,  ident = literal t}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIdentifierToLastExp(b, t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIdentifierToLastExp(b, t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addIdentifierToLastExp(b, t, groupedExpression e)}
      | expressionType e == INFIXEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = infixOperator e, infixExpression = addIdentifierToLastExp(b, t, infixExpression e)}
      | expressionType e == CALLEXP = CallExpression{expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addIdentifierToLastExp(b, t, getLastParam(callParams e))]}
      | expressionType e == IDENTEXP && b == PAR = CallExpression{expressionType = CALLEXP, closedCall = False, callIdent = e, callParams = [IdentExpression{expressionType = IDENTEXP,  ident = literal t}]}
      | otherwise = error (expressionToString (e)) 



changeSta :: (BlockType, Token, [Statement]) -> [Statement]
changeSta (b, t,s) = sta 
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



changeFuncToCall :: (BlockType, [Statement]) -> [Statement] 
changeFuncToCall (b,s) = sta
  where
    sta   
      | statementType (last s) == FUNCSTA && null (body (statementUni(last s))) = pop s ++ [Statement{
          statementUni=CallStatement{},
          statementType=CALLSTA, 
          expression = CallExpression {
            expressionType = CALLEXP,
            callParams = params (statementUni (last s)),
            callIdent= expression  (last s), 
            closedCall = False 
          }}]
      | otherwise = pop s ++ [changeLastFuncToCall(b, last s)]

changeLastFuncToCall :: (BlockType, Statement) -> Statement 
changeLastFuncToCall (b,s) = sta 
  where   
    sta 
      | statementType s == FUNCSTA && null (body (statementUni s)) = Statement{
          statementUni=CallStatement{},
          statementType=CALLSTA, 
          expression = CallExpression {
            expressionType = CALLEXP,
            callParams = params (statementUni s),
            callIdent= expression  s,
            closedCall = False 
          }}
      | statementType s == RETSTA || statementType s == LETSTA = Statement {statementType = statementType s, statementUni = statementUni s, expression = changeLastFuncExp(expression s)} 
      | otherwise = error "changeLastFuncToCall" 

changeLastFuncExp :: Expression -> Expression 
changeLastFuncExp e = exp
  where   
    exp 
      | expressionType e == IDENTEXP = CallExpression {
            expressionType = CALLEXP,
            callParams = [],  
            callIdent= e,
            closedCall = False 
          }
      | expressionType e == INFIXEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = infixOperator e, infixExpression = changeLastFuncExp(infixExpression e)}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = changeLastFuncExp(rightOperator e)}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = changeLastFuncExp(rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {expressionType = GROUPEDEXP, groupedExpression = changeLastFuncExp(groupedExpression e), closed = closed e}
      | expressionType e == CALLEXP = CallExpression{
          expressionType = CALLEXP, 
          callParams = callParams e,
          callIdent = callIdent e,
          closedCall = True
        }
      | otherwise = error "changeLastFuncExp" 

paramHasOpen :: (BlockType, [Statement]) -> Bool
paramHasOpen (b,s) = bo
  where 
    bo 
      | null s == True = False 
      | getLastExpressionType(b, s) == GROUPEDEXP && closed (getLastNestedExpression(b, s))== False && checkFuncClosed(getLastNestedExpression(b,s)) == True = True 
      | statementType (last s) == FUNCSTA && getLastExpressionType(b, s) == GROUPEDEXP &&  findGrouped(last (params (statementUni(last s)))) == True = True 
      | otherwise = False 



checkFuncClosed :: Expression -> Bool 
checkFuncClosed e = b 
  where 
    b 
      | expressionType e == CALLEXP && closedCall e == False = False 
      | expressionType e == BOOLEXP = checkFuncClosed(rightBool e)
      | expressionType e == OPERATOREXP = checkFuncClosed(rightOperator e)
      | expressionType e == INFIXEXP = checkFuncClosed(infixExpression e)
      | expressionType e == GROUPEDEXP = checkFuncClosed(groupedExpression e)
      | otherwise = False 

getLastNestedExpression:: (BlockType, [Statement]) -> Expression
getLastNestedExpression(b, s) = e 
  where   
    e 
      | statementType (last s) == FUNCSTA = last(params(statementUni(last s)))
      | b == EXP || statementType (last s) /= IFSTA = expression (last s)
      | b == PAR = error "getLastNested not implemented for PAR"
      | b == CON && null (alt (statementUni (last s))) == True = getLastNestedExpression(b, con(statementUni (last (s))))
      | b == CON = getLastNestedExpression(b, alt(statementUni (last s)))
      | b == ALT && closedCon (statementUni (last s)) == True = getLastNestedExpression(b, alt(statementUni (last (s))))
      | b == ALT = getLastNestedExpression(b, con(statementUni (last s)))
      | b == CON = error "con"
      | b == ALT = error "alt"
      | otherwise = error "get last nested expression"

addEmptyToLastParam :: Statement -> Statement 
addEmptyToLastParam s = sta 
  where 
    sta 
      | statementType s == FUNCSTA = Statement{
        statementType = FUNCSTA,
        statementUni = FuncStatement{
            body = [],
            params = params (statementUni s)++ [Expression{expressionType = EMPTYEXP}]
          },
        expression = expression s
      }
      | statementType s /= IFSTA = Statement{statementType = statementType s, statementUni = statementUni s, expression = addEmptyToLastParamExp(expression s)} 
      | otherwise = error "addEmptyToLastParam" 
addEmptyToLastParamExp :: Expression -> Expression 
addEmptyToLastParamExp e = exp 
  where 
    exp
      | expressionType e == CALLEXP = CallExpression{expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = callParams e ++ [Expression{expressionType = EMPTYEXP}]} 
      | expressionType e == OPERATOREXP = OperatorExpression{expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = addEmptyToLastParamExp(rightOperator e)}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addEmptyToLastParamExp(rightBool e)}
      | expressionType e == INFIXEXP = InfixExpression{expressionType = INFIXEXP, infixOperator = infixOperator e, infixExpression = addEmptyToLastParamExp(infixExpression e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addEmptyToLastParamExp(e)} 
      | otherwise =error "addEmptyToLastParamExp"

addIfToLastBlock:: (BlockType, [Statement]) -> [Statement]
addIfToLastBlock(b,s) = sta 
  where   
    sta 
      | b == CON && statementType (last s) == IFSTA && null (alt (statementUni (last s)))= pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = False,
              con = con (statementUni (last s)) ++ [Statement{
                statementType = IFSTA,
                expression = Expression{expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = False, 
                  con = [], 
                  alt = [], 
                  closedAlt = False
                  }
                }
              ],
              alt = []
            },
          expression = expression (last s)
          }
        ] 
      | otherwise = s--error "addIfToLastStatement"

findLastOpen:: ([Statement]) -> Bool
findLastOpen(s) = bool
  where 
    bool
      | null s == True = False
      | statementType (last s) == IFSTA && closedCon (statementUni (last s)) == False = True 
      | statementType (last s) == IFSTA && closedAlt (statementUni (last s)) == False = True 
      | otherwise = False
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
          closedCon = False,
          closedAlt = False,
          con = closeLastOpen(b, con(statementUni (last s))),
          alt = alt (statementUni (last s))}}]
      | statementType (last s) == IFSTA && closedAlt(statementUni (last s)) == False && findLastOpen(alt(statementUni(last s))) == False = 
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
      | statementType (last s) == IFSTA && b == ALT= pop s ++ [Statement{
        statementType = statementType (last s),
        expression = expression (last s),
        statementUni = IfStatement {
          closedCon = True,
          closedAlt = False,
          con = con (statementUni (last s)),
          alt = closeLastOpen(b, alt(statementUni (last s)))}}]
      | otherwise = error "closeLastIf"

