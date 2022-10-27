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
addIntToLastExp(b, t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = IntegerLiteralExpression{expressionType = INTEXP, integerLiteral = t}
      | expressionType e == PREFIXEXP = PrefixExpression {expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = t}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIntToLastExp(b, t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIntToLastExp(b, t, rightBool e)}
      | expressionType e == GROUPEDEXP && expressionType (groupedExpression e) == EMPTYEXP= GroupedExpression {expressionType = GROUPEDEXP, closed= False, groupedExpression = IntegerLiteralExpression {expressionType = INTEXP, integerLiteral = t}}
      | expressionType e == GROUPEDEXP = GroupedExpression {expressionType = GROUPEDEXP, closed= False, groupedExpression = addIntToLastExp (b, t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIntToLastExp(b, t, assignExpression e)}
      | expressionType e == CALLEXP = CallExpression {expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addIntToLastExp(b, t, last(callParams e))]}
      | expressionType e == IDENTEXP && b == PAR= CallExpression {
            expressionType = CALLEXP,
            callParams = [IntegerLiteralExpression{expressionType = INTEXP, integerLiteral = t}],  
            callIdent= e,
            closedCall = False 
          }
      | otherwise = error ("addIntToLastExp" ++ expressionToString(e))

addBoolToLastExp :: (Token, Expression) -> Expression
addBoolToLastExp (t, e) = ex
  where
    ex 
      | expressionType e == IDENTEXP || expressionType e == CALLEXP || expressionType e == INTEXP || expressionType e == PREFIXEXP ||expressionType e == OPERATOREXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addBoolToLastExp (t, rightBool e)} 
      | expressionType e == GROUPEDEXP && closed e == False = GroupedExpression {expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addBoolToLastExp (t, groupedExpression e)} 
      | expressionType e == GROUPEDEXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addBoolToLastExp(t, assignExpression e)}
      | otherwise = error "addBoolToLastExp"


addOperatorToLastExp:: (Token, Expression) -> Expression 
addOperatorToLastExp(t, e) = ex
  where
    ex
      | expressionType e == IDENTEXP || expressionType e == INTEXP || expressionType e == PREFIXEXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression {expressionType = EMPTYEXP}}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addOperatorToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP && closed e == True = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e,operator = t, rightOperator = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addOperatorToLastExp(t, groupedExpression e)}
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) == GROUPEDEXP && closed (rightOperator e) == True && checkPrecedence(t, e) == False = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression {expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) /= INTEXP && expressionType (rightOperator e) /= PREFIXEXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addOperatorToLastExp(t, rightOperator e))}
      | expressionType e == OPERATOREXP && checkPrecedence (t, e) == True = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = OperatorExpression {expressionType = OPERATOREXP, leftOperator = rightOperator e, operator = t, rightOperator = Expression {expressionType = EMPTYEXP}}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = e,operator = t,rightOperator = Expression {expressionType = EMPTYEXP}}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addOperatorToLastExp(t, assignExpression e)}
      | expressionType e == EMPTYEXP = PrefixExpression {expressionType = PREFIXEXP, prefixOperator = t, prefixExpression = Expression {expressionType = EMPTYEXP}}
      | otherwise = error "addOperatorToLastExpression" 

addPrefixToLastExp:: (Token, Expression) ->  Expression 
addPrefixToLastExp(t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = PrefixExpression {expressionType = PREFIXEXP, prefixOperator = t, prefixExpression = Expression {expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addPrefixToLastExp(t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addPrefixToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addPrefixToLastExp(t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addPrefixToLastExp(t, assignExpression e)}
      | otherwise = error "addPrefixToLastRightOperator"

addTFToLastBool :: (Token, Expression) -> Expression 
addTFToLastBool (t, e) = ex
  where
    ex
      | expressionType e == BOOLEXP && expressionType (rightBool e) == EMPTYEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = TFExpression {expressionType = TFEXP, bool = typ t}} 
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addTFToLastBool(t, rightBool e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addTFToLastBool(t, assignExpression e)}
      | expressionType e == EMPTYEXP = TFExpression{expressionType = TFEXP, bool = typ t} 
      | otherwise = error "addTFToLastBool"

isTF :: Token -> Bool
isTF t = typ t == FALSE || typ t == TRUE

addGroupToLastExp :: Expression -> Expression
addGroupToLastExp e = ex
  where 
    ex 
      | expressionType e == EMPTYEXP = GroupedExpression {expressionType = GROUPEDEXP, closed=False, groupedExpression = Expression{expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = addGroupToLastExp (rightOperator e)}
      | expressionType e == BOOLEXP =  BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addGroupToLastExp( rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addGroupToLastExp (groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addGroupToLastExp(assignExpression e)}
      | expressionType e == IDENTEXP = CallExpression {expressionType = CALLEXP, callIdent = e, callParams = [], closedCall = False}  
      | otherwise = error "error AddingGroupToLastExp"


closeLastGrouped :: Expression -> Expression
closeLastGrouped e = ex 
  where 
    ex 
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
      | expressionType e == CALLEXP = CallExpression {expressionType = CALLEXP, callParams = callParams e, callIdent = callIdent e, closedCall= True}
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
    CON -> pop s ++ addToLastCon(s, sta)
    ALT -> pop s ++ addToLastAlt(s, sta)
    BOD ->  pop s ++ [
      Statement {
        statementType = statementType (last s), 
        statementUni = FuncStatement{
          params = getParams(s),
          body = getBody(s) ++ [sta] 
        },
        expression = expression (last s) 
        }] 
    EXP -> s ++ [sta] 
    _ -> error "noPopAddToStatement"

addToLastCon :: ([Statement], Statement) -> [Statement]
addToLastCon (s, sta) = statement 
  where 
    statement 
      | null s = [sta]
      | statementType (last s) == IFSTA && null (getAlt(s)) == True= pop s ++ [
        Statement{
            statementType = IFSTA, 
            statementUni = IfStatement{
              closedCon = False,
              closedAlt = False,
              alt = getAlt(s), 
              con = addToLastCon(getCon(s), sta) 
            },
            expression = expression (last s)
          }]
      | statementType (last s) == IFSTA && getClosedCon(s) == True = pop s ++ [
        Statement{
            statementType = IFSTA, 
            statementUni = IfStatement{
              closedCon = True,
              closedAlt = False,
              alt = addToLastCon(getAlt(s), sta), 
              con = getCon(s) 
            },
            expression = expression (last s)
          }
      ]
      | statementType (last s) == FUNCSTA = pop s ++ [Statement{
          statementType = statementType (last s),
          statementUni = FuncStatement{
              params = getParams(s),
              body = addToLastCon(getBody(s), sta)
            },
          expression = expression (last s)
        }]
      | otherwise = s ++ [sta] 


addToLastAlt :: ([Statement], Statement) -> [Statement]
addToLastAlt (s, sta) = statement 
  where 
    statement 
      | null s = [sta]
      | statementType (last s) == IFSTA &&  getClosedCon(s)== True= pop s ++ [ 
          Statement{
              statementType = IFSTA, 
              statementUni = IfStatement{
                closedCon = True,
                closedAlt = False,
                alt = addToLastAlt(getAlt(s), sta),
                con = getCon(s) 
              },
              expression = expression (last s)
            }]
      | statementType (last s) == IFSTA = pop s ++ [
          Statement{
              statementType = IFSTA, 
              statementUni = IfStatement{
                closedCon = False,
                closedAlt = False,
                alt = getAlt(s),
                con = addToLastAlt(getCon(s), sta) 
              },
              expression = expression (last s)
            }]
      | statementType (last s) == FUNCSTA = pop s ++ [Statement{
          statementType = statementType (last s),
          statementUni = FuncStatement{
              params = getParams(s),
              body = addToLastAlt(getBody(s), sta)
            },
          expression = expression (last s)
        }]
      | otherwise = s ++ [sta]

addToStatement :: (BlockType, [Statement], Statement) -> [Statement]
addToStatement (b, s, sta) = 
  case b of 
    CON -> pop s ++ [sta] 
    ALT -> pop s ++ [sta] 
    PAR -> pop s ++ [sta]
    BOD -> pop s ++ [
      Statement {
          statementType = statementType(last s),
          statementUni = FuncStatement{
              params = getParams(s),
              body = pop (getBody(s)) ++ [last (body (statementUni(sta)))]
            },
          expression = expression (last s)
        }
      ]
    EXP -> pop s ++ [sta] 

addToLastStatement :: (BlockType, Token, ExpressionType, [Statement]) ->Statement 
addToLastStatement (b, t, e, s) = sta 
  where 
    sta 
      | b == PAR  && statementType (last s) == FUNCSTA = Statement{
        statementType = FUNCSTA,
        statementUni = FuncStatement{
          params = pop (getParams(s)) ++ [addXToExp(b, t, e, getLastParam(getParams(s)))],
          body = []
          },
        expression = expression (last s)
        } 
      | (b == CON || b == ALT ) && statementType (last s) == FUNCSTA = Statement{statementType =FUNCSTA, expression = expression (last s), statementUni = FuncStatement{
          params = getParams(s),
          body = pop (getBody(s)) ++ [addToLastStatement(b, t, e, getBody(s))]
        }}
      | b == BOD = Statement {
          statementType = statementType (last s), 
          statementUni = FuncStatement{
            params = getParams(s),
            body = pop (getBody(s)) ++ [addToLastStatement(EXP, t, e, getBody(s))]}, 
          expression = expression (last s)
          }
      --Adding in con 
      |  (b == CON || b == ALT ) && statementType (last s) == IFSTA && null (getCon(s)) == False && getClosedCon(s) == False = Statement{
          statementType = IFSTA,
          statementUni = IfStatement{
            closedCon = False,
            closedAlt = False,
            con = pop (getCon(s)) ++ [addToLastStatement(b,t,e, getCon(s))],
            alt = []
            },
          expression = expression (last s)
        }
      -- Adding in Alt 
      | (b == CON || b == ALT ) && statementType (last s) == IFSTA &&  getClosedCon(s) == True = Statement {
          statementType = IFSTA,
          statementUni = IfStatement{
              closedCon = True,
              closedAlt = False,
              con = getCon(s), 
              alt = pop (getAlt(s)) ++ [addToLastStatement(b,t,e, getAlt(s))]
            },
          expression = expression (last s)
        }
      --Found last statement
      | otherwise = Statement{
          statementType = statementType(last s),
          statementUni = statementUni(last s),
          expression = addXToExp(b, t, e, expression (last s))
        } 


getLastParam :: [Expression] -> Expression 
getLastParam e = 
  case e of 
    [] -> Expression{expressionType = EMPTYEXP}
    _ -> last e



addXToExp :: (BlockType, Token, ExpressionType, Expression) -> Expression 
addXToExp (b, t,et, e) = ex 
  where 
    ex 
      | typ t == INT = addIntToLastExp(b, t, e)
      | typ t == MINUS && et == PREFIXEXP =  addPrefixToLastExp(t, e)
      | typ t == MINUS = addOperatorToLastExp(t,e)
      | typ t == BANG = addPrefixToLastExp(t, e)
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
addAssignToLastExp e = ex 
  where
    ex
      | expressionType e == IDENTEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = e, assignExpression = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == EMPTYEXP = Expression {expressionType = EMPTYEXP}
      | otherwise = error "parseAssignToLastExp"

parseIdentifierToLet :: [Token] -> Statement
parseIdentifierToLet t = s
  where
    s  
      | typ (head t) == IDENT = Statement {statementType = LETSTA, statementUni= LetStatement {identifier = literal (head t)}, expression = Expression {expressionType = EMPTYEXP}}
      | otherwise = error ("Error parsing identifier to let statement on line: " ++ (show(line(head t))))

addIdentifierToLastExp:: (BlockType, Token, Expression) ->  Expression 
addIdentifierToLastExp(b, t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = IdentExpression{expressionType = IDENTEXP,  ident = t}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIdentifierToLastExp(b, t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIdentifierToLastExp(b, t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addIdentifierToLastExp(b, t, groupedExpression e)}
      | expressionType e == PREFIXEXP = PrefixExpression {expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = addIdentifierToLastExp(b, t, prefixExpression e)}
      | expressionType e == CALLEXP = CallExpression{expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addIdentifierToLastExp(b, t, getLastParam(callParams e))]}
      | expressionType e == IDENTEXP && b == PAR = CallExpression{expressionType = CALLEXP, closedCall = False, callIdent = e, callParams = [IdentExpression{expressionType = IDENTEXP,  ident = t}]}
      | expressionType e == ASSIGNEXP = AssignExpression {expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIdentifierToLastExp(b,t,assignExpression e)}
      | otherwise = error ("Error adding identifier to last exp on line: " ++ (show (line(t)))) 



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



hasValidParams :: [Statement] -> Bool
hasValidParams s = b 
  where 
    b 
      | null (getParams(s)) == True = True
      | (False`elem` [expressionType x == IDENTEXP | x <- getParams(s)]) == False = True
      | otherwise = False

hasValidCondition :: (BlockType, [Statement]) -> Bool
hasValidCondition (b, s) = boo 
  where
    boo 
      |getLastIfConditionType(b,s)  == GROUPEDEXP = True
      --TODO Implement check for boolexp inside if
      | otherwise = error (expressionToString(getLastNestedExpression(b, s)))

changeFuncToCall :: [Statement] -> [Statement] 
changeFuncToCall s = sta
  where
  sta   
      | statementType (last s) == FUNCSTA && null (getBody(s))  = pop s ++ [Statement{
          statementUni=CallStatement{},
          statementType=CALLSTA, 
          expression = CallExpression {
            expressionType = CALLEXP,
            callParams = getParams(s),
            callIdent= expression  (last s), 
            closedCall = False
            }}]
      | otherwise = pop s ++ [changeLastFuncToCall(last s)]

changeLastFuncToCall :: Statement -> Statement 
changeLastFuncToCall s = sta 
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
changeLastFuncExp e = ex
  where   
    ex
      | expressionType e == IDENTEXP = CallExpression {
            expressionType = CALLEXP,
            callParams = [],  
            callIdent= e,
            closedCall = False 
          }
      | expressionType e == PREFIXEXP = PrefixExpression {expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = changeLastFuncExp(prefixExpression e)}
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
      | statementType (last s) == FUNCSTA && getLastExpressionType(b, s) == GROUPEDEXP &&  findGrouped(last (getParams(s))) == True = True 
      | otherwise = False 



checkFuncClosed :: Expression -> Bool 
checkFuncClosed e = b 
  where 
    b 
      | expressionType e == CALLEXP && closedCall e == False = False 
      | expressionType e == BOOLEXP = checkFuncClosed(rightBool e)
      | expressionType e == OPERATOREXP = checkFuncClosed(rightOperator e)
      | expressionType e == PREFIXEXP = checkFuncClosed(prefixExpression e)
      | expressionType e == GROUPEDEXP = checkFuncClosed(groupedExpression e)
      | otherwise = False 

getLastNestedExpression:: (BlockType, [Statement]) -> Expression
getLastNestedExpression(b, s) = e 
  where   
    e 
      | statementType (last s) == FUNCSTA = last(getParams(s))
      | b == EXP || statementType (last s) /= IFSTA = expression (last s)
      | b == PAR = error "getLastNested not implemented for PAR"
      | b == CON && null (getAlt(s)) == True = getLastNestedExpression(b, getCon(s))
      | b == CON = getLastNestedExpression(b, getAlt(s))
      | b == ALT && getClosedCon(s) == True = getLastNestedExpression(b, getAlt(s))
      | b == ALT = getLastNestedExpression(b, getCon(s))
      | b == CON = error "con"
      | b == ALT = error "alt"
      | otherwise = error "get last nested expression"

getLastIfConditionType :: (BlockType, [Statement]) -> ExpressionType 
getLastIfConditionType (b, s) = e
  where
    e 
      | b == EXP || (statementType (last s) == IFSTA && null(getCon(s)) == True) = expressionType (expression (last s))
      | statementType(last s) == FUNCSTA = getLastIfConditionType(b, getBody(s))
      | b == CON && null (getAlt(s)) == True = getLastIfConditionType(b, getCon(s))
      | b == CON = getLastIfConditionType(b, getAlt(s))
      | b == ALT && getClosedCon(s) == True = getLastIfConditionType(b, getAlt(s))
      | b == ALT = getLastIfConditionType(b, getCon(s))
      | b == CON = error "con"
      | b == ALT = error "alt"
      | otherwise = error "getLastIfConditionType"



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
addEmptyToLastParamExp e = ex 
  where 
    ex
      | expressionType e == CALLEXP = CallExpression{expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = callParams e ++ [Expression{expressionType = EMPTYEXP}]} 
      | expressionType e == OPERATOREXP = OperatorExpression{expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = addEmptyToLastParamExp(rightOperator e)}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addEmptyToLastParamExp(rightBool e)}
      | expressionType e == PREFIXEXP = PrefixExpression{expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = addEmptyToLastParamExp(prefixExpression e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addEmptyToLastParamExp(e)} 
      | otherwise =error "addEmptyToLastParamExp"

addIfToLastBlock:: (BlockType, [Statement]) -> [Statement]
addIfToLastBlock(b,s) = sta 
  where   
    sta 
      -- Add CON  
      | b == CON && statementType (last s) == IFSTA && getClosedCon(s) == False &&  (null(getCon(s)) == True || statementType (last (getCon(s))) /= IFSTA)= pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = False,
              con = getCon(s) ++ [Statement{
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
      --Search in con 
      | b == CON && statementType (last s) == IFSTA && getClosedCon(s) == False = pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = False,
              con = getCon(s) ++ [Statement{
                statementType = IFSTA,
                expression = Expression{expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = False, 
                  con = addIfToLastBlock(CON, con(statementUni(last s))), 
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
      --Search in alt 
      | b == CON = pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = False,
              con = getCon(s),
              alt =  getAlt(s) ++ [Statement{
                statementType = IFSTA,
                expression = Expression{expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = False, 
                  con = getCon(getCon(s)), 
                  alt = addIfToLastBlock(CON, getAlt(s)),  
                  closedAlt = False
                  }
                }
              ]
            },
          expression = expression (last s)
          }
      ] 
      -- Add Alt
      | b == ALT && statementType (last s) == IFSTA && getClosedCon(s) == True && (null (getAlt(s)) == True || statementType (last (getAlt(s))) /= IFSTA) = pop s ++ [
          Statement{
              statementType = IFSTA, 
              statementUni = IfStatement{
                  closedCon = False,
                  con = [],
                  closedAlt = False,
                  alt = pop (getAlt(s)) ++ [Statement{
                statementType = IFSTA,
                expression = Expression{expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = False, 
                  con = [], 
                  alt = [], 
                  closedAlt = False
                  }
                }
              ] 
              },
              expression = expression (last s)
            }
      ] 
      --Search in alt 
      | b == ALT && statementType (last s) == IFSTA && getClosedCon(s) == True = pop s ++ [
          Statement{
              statementType = IFSTA, 
              statementUni = IfStatement{
                  closedCon = True,
                  con = getCon(s),
                  closedAlt = False,
                  alt = addIfToLastBlock(ALT, getAlt(s))
              },
              expression = expression (last s)
            }
      ] 
      -- Search in CON
      | b == ALT && statementType (last s) == IFSTA && getClosedCon(s) == False = pop s ++ [
          Statement{
              statementType = IFSTA, 
              statementUni = IfStatement{
                  closedCon = False,
                  con = addIfToLastBlock(ALT, getCon(s)),
                  closedAlt = False,
                  alt = [] 
              },
              expression = expression (last s)
            }
      ] 
      --Is FUNCSTA
      | b == BOD && statementType (last s) == FUNCSTA = pop s ++ [
          Statement{
              statementType = FUNCSTA, 
              statementUni = FuncStatement{
                  params = getParams(s),
                  body = getBody(s) ++ [Statement{
                    statementType = IFSTA,
                    expression = Expression{expressionType = EMPTYEXP},
                    statementUni = IfStatement{
                      closedCon = False, 
                      con = [], 
                      alt = [], 
                      closedAlt = False
                      }
                    }
                ]
              },
              expression = expression (last s)
            }
        ]
      | otherwise = error "addIfToLastBlock"

findLastOpen:: ([Statement]) -> Bool
findLastOpen(s) = b
  where 
    b
      | null s == True = False
      | statementType (last s) == IFSTA && getClosedAlt(s)== False && null(getAlt(s)) == False= True 
      | statementType (last s) == IFSTA && getClosedCon(s) == False  = True 
      | otherwise = False

closeLastOpen:: (BlockType, [Statement]) -> [Statement]
closeLastOpen(b, s) = sta 
  where   
    sta
      --Close con
      | statementType (last s) == IFSTA && getClosedCon(s) == False && findLastOpen(getCon(s)) == False = 
        pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
            closedCon = True,
            closedAlt = False,
            con = getCon(s),
            alt = getAlt(s)},
            expression = expression (last s)}
          ] 
      -- Search in con
      | statementType (last s) == IFSTA && getClosedCon(s) == False && null(getAlt(s)) == True = pop s ++ [Statement{
        statementType = statementType (last s),
        expression = expression (last s),
        statementUni = IfStatement {
          closedCon = False,
          closedAlt = False,
          con = closeLastOpen(b, getCon(s)),
          alt = getAlt(s)}}]
      -- Close Alt
      | statementType (last s) == IFSTA && closedAlt(statementUni (last s)) == False && findLastOpen(getAlt(s)) == False = 
        pop s ++ [
        Statement{
          statementType = statementType (last s),
          statementUni = IfStatement{
            closedCon = True,
            closedAlt = True,
            con = getCon(s),
            alt = getAlt(s)},
            expression = expression (last s)}
          ] 
      --Search in alt
      | statementType (last s) == IFSTA = pop s ++ [Statement{
        statementType = statementType (last s),
        expression = expression (last s),
        statementUni = IfStatement {
          closedCon = True,
          closedAlt = False,
          con = getCon(s),
          alt = closeLastOpen(b, getAlt(s))}}]
      -- s is funcsta
      | statementType (last s) == FUNCSTA && b /= BOD = pop s ++ [Statement {
          statementType = FUNCSTA,
          statementUni = FuncStatement{
              params = getParams(s),
              body = closeLastOpen(b, getBody(s))
            },
          expression = expression (last s)
        }] 
      | otherwise = error "closeLastOpen"

findLastBlockType :: (BlockType, Statement) -> BlockType 
findLastBlockType (b, s) = block
  where   
    block 
      | statementType s == FUNCSTA = findLastBlockType(b, last (body(statementUni s)))
      | closedCon (statementUni s) == False = findLastBlockType(CON, (last (con(statementUni s)))) 
      | null (alt(statementUni s)) == False && statementType (last (alt (statementUni s))) == IFSTA = findLastBlockType(ALT, (last (alt(statementUni s))))
      | otherwise = b


getCon :: [Statement] -> [Statement]
getCon s = con (statementUni (last s))

getAlt:: [Statement] -> [Statement]
getAlt s = alt (statementUni (last s))

getClosedCon :: [Statement] -> Bool
getClosedCon s = closedCon(statementUni (last s))

getClosedAlt :: [Statement] -> Bool
getClosedAlt s = closedAlt(statementUni (last s))

getParams :: [Statement] -> [Expression] 
getParams s = params (statementUni(last s))

getBody :: [Statement] -> [Statement]
getBody s = body(statementUni(last s))
