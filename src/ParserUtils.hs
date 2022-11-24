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
      | expressionType e == EMPTYEXP = IntegerLiteralExpression{expLine = expLine e, expressionType = INTEXP, integerLiteral = t}
      | expressionType e == MAPEXP && null (fst (mapMap e)) = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) ++ [addIntToLastExp(b,t, Expression{expLine = expLine e, expressionType=EMPTYEXP})], snd (mapMap e))}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [addIntToLastExp(b, t, last (getVal e))])} 
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (pop (fst (mapMap e)) ++ [addIntToLastExp(b,t, last (fst (mapMap e)))], snd (mapMap e))}
      | expressionType e == MAPEXP && checkValidVal(e)= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [addIntToLastExp(b,t, last (snd (mapMap e)))])}
      | expressionType e == ARRAYEXP && null (array e) =  ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array =[IntegerLiteralExpression{expressionType = INTEXP, expLine = expLine e, integerLiteral = t}]} 
      | expressionType e == ARRAYEXP && expressionType (last (array e)) == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addIntToLastExp(b,t,last (array e))]}
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addIntToLastExp(b, t, last (array e))]} 
      | expressionType e == PREFIXEXP = PrefixExpression {expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = IntegerLiteralExpression {expLine = expLine e,expressionType = INTEXP, integerLiteral = t}}
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIntToLastExp(b, t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIntToLastExp(b, t, rightBool e)}
      | expressionType e == GROUPEDEXP && expressionType (groupedExpression e) == EMPTYEXP= GroupedExpression {expLine = expLine e,expressionType = GROUPEDEXP, closed= False, groupedExpression = IntegerLiteralExpression {expLine= expLine e, expressionType = INTEXP, integerLiteral = t}}
      | expressionType e == GROUPEDEXP = GroupedExpression {expLine = expLine e, expressionType = GROUPEDEXP, closed= False, groupedExpression = addIntToLastExp (b, t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIntToLastExp(b, t, assignExpression e)}
      | expressionType e == CALLEXP = CallExpression {expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addIntToLastExp(b, t, last(callParams e))]}
      | expressionType e == INDEXEXP = IndexExpression {closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [addIntToLastExp(b, t, last (arrayIndex e))]}
      | expressionType e == IDENTEXP && b == PAR= CallExpression {
            expLine = expLine e,
            expressionType = CALLEXP,
            callParams = [IntegerLiteralExpression{expLine = expLine e,expressionType = INTEXP, integerLiteral = t}],  
            callIdent= e,
            closedCall = False 
          }
      | otherwise = error ("addIntToLastExp on line: " ++ (show (expLine e)) ++ " " ++ expressionToString(e))

addBoolToLastExp :: (Token, Expression) -> Expression
addBoolToLastExp (t, e) = ex
  where
    ex 
      | expressionType e == TFEXP || expressionType e == EMPTYEXP || expressionType e == INDEXEXP || expressionType e == IDENTEXP || expressionType e == CALLEXP || expressionType e == INTEXP || expressionType e == PREFIXEXP ||expressionType e == OPERATOREXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expLine = expLine e, expressionType = EMPTYEXP}} 
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addBoolToLastExp (t, rightBool e)} 
      | expressionType e == GROUPEDEXP && closed e == False = GroupedExpression {expLine = expLine e, expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addBoolToLastExp (t, groupedExpression e)} 
      | expressionType e == GROUPEDEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expLine = expLine e, expressionType = EMPTYEXP}} 
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addBoolToLastExp(t, assignExpression e)}
      | expressionType e == ARRAYEXP = ArrayExpression{expLine = expLine e, expressionType = ARRAYEXP, closedArr = False, array = pop (array e) ++ [addBoolToLastExp(t, last (array e))]}
      | otherwise = error ("addBoolToLastExp on line: " ++ (show (expLine e)) ++ " " ++ expressionToString(e))


addOperatorToLastExp:: (Token, Expression) -> Expression 
addOperatorToLastExp(t, e) = ex
  where
    ex
      | expressionType e == CALLEXP || expressionType e == IDENTEXP || expressionType e == STRINGEXP || expressionType e == INTEXP || expressionType e == PREFIXEXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression {expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == MAPEXP && null (fst (mapMap e)) = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) ++ [addOperatorToLastExp(t, Expression{expLine = expLine e, expressionType=EMPTYEXP})], snd (mapMap e))}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [addOperatorToLastExp(t, last (getVal e))])} 
      | expressionType e == MAPEXP && nextItem e == KEY && checkValidKey(e) = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (pop (fst (mapMap e)) ++ [addOperatorToLastExp(t, last (fst (mapMap e)))], snd (mapMap e))}
      | expressionType e == MAPEXP && checkValidVal(e)= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [addOperatorToLastExp(t, last (snd (mapMap e)))])}
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addOperatorToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP && closed e == True = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = e,operator = t, rightOperator = Expression {expLine = expLine e, expressionType = EMPTYEXP}} 
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addOperatorToLastExp(t, groupedExpression e)}
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) == GROUPEDEXP && closed (rightOperator e) == True && checkPrecedence(t, e) == False = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression {expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP && expressionType (rightOperator e) /= INTEXP && expressionType (rightOperator e) /= PREFIXEXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addOperatorToLastExp(t, rightOperator e))}
      | expressionType e == OPERATOREXP && checkPrecedence (t, e) == True = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = OperatorExpression {expLine = expLine e,expressionType = OPERATOREXP, leftOperator = rightOperator e, operator = t, rightOperator = Expression {expLine = expLine e, expressionType = EMPTYEXP}}}
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = e,operator = t,rightOperator = Expression {expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == INDEXEXP && closedIndex e == True = OperatorExpression {
          expLine = expLine e, 
          expressionType = OPERATOREXP, 
          leftOperator = IndexExpression{
            closedIndex = True, 
            expLine = expLine e, 
            expressionType = INDEXEXP, 
            arrayIdent = arrayIdent e, 
            arrayIndex = arrayIndex e},
          operator = t,
          rightOperator = Expression {expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == INDEXEXP = IndexExpression{closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [addOperatorToLastExp(t, last (arrayIndex e))]}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addOperatorToLastExp(t, assignExpression e)}
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = [PrefixExpression {expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = t, prefixExpression = Expression {expLine = expLine e, expressionType = EMPTYEXP}}]} 
      | expressionType e == ARRAYEXP && expressionType (last (array e)) == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addOperatorToLastExp(t,last (array e))]}
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addOperatorToLastExp(t, last (array e))]} 
      | expressionType e == EMPTYEXP = PrefixExpression {expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = t, prefixExpression = Expression {expLine = expLine e, expressionType = EMPTYEXP}}
      | otherwise = error ("addOperatorToLastExpression on line: " ++ (show (expressionType e)) ++ " " ++ expressionToString(e)) 

addPrefixToLastExp:: (Token, Expression) ->  Expression 
addPrefixToLastExp(t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = PrefixExpression {expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = t, prefixExpression = Expression {expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addPrefixToLastExp(t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addPrefixToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addPrefixToLastExp(t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addPrefixToLastExp(t, assignExpression e)}
      | otherwise = error ("addPrefixToLastRightOperator on line: " ++ (show (expLine e)) ++ " " ++ expressionToString(e)) 

addTFToLastExp:: (Token, Expression) -> Expression 
addTFToLastExp(t, e) = ex
  where
    ex
      | expressionType e == BOOLEXP && expressionType (rightBool e) == EMPTYEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = TFExpression {expLine = expLine e, expressionType = TFEXP, bool = typ t}} 
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addTFToLastExp(t, rightBool e)}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [addTFToLastExp(t, last (getVal e))])} 
      | expressionType e == MAPEXP && checkValidVal(e)= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [addTFToLastExp(t, last (snd (mapMap e)))])}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addTFToLastExp(t, assignExpression e)}
      | expressionType e == EMPTYEXP = TFExpression{expLine = expLine e, expressionType = TFEXP, bool = typ t} 
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = [TFExpression{expLine = expLine e, expressionType = TFEXP, bool = typ t} ]} 
      | expressionType e == ARRAYEXP && expressionType (last (array e)) == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addTFToLastExp(t,last (array e))]}
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addTFToLastExp(t, last (array e))]} 
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addTFToLastExp(t, groupedExpression e)}
      | expressionType e == PREFIXEXP = PrefixExpression{expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = TFExpression{expLine = expLine e, expressionType = TFEXP, bool = typ t}}
      | otherwise = error ("addTFToLastExp on line: " ++ (show (expLine e)) ++ " " ++ expressionToString(e)) 

isTF :: Token -> Bool
isTF t = typ t == FALSE || typ t == TRUE

addGroupToLastExp :: Expression -> Expression
addGroupToLastExp e = ex
  where 
    ex 
      | expressionType e == EMPTYEXP = GroupedExpression {expLine = expLine e, expressionType = GROUPEDEXP, closed=False, groupedExpression = Expression{expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == MAPEXP && null (fst (mapMap e)) = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) ++ [addGroupToLastExp(Expression{expLine = expLine e, expressionType=EMPTYEXP})], snd (mapMap e))}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [addGroupToLastExp(last (getVal e))])} 
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (pop (fst (mapMap e)) ++ [addGroupToLastExp(last (fst (mapMap e)))], snd (mapMap e))}
      | expressionType e == MAPEXP && checkValidVal(e)= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [addGroupToLastExp(last (snd (mapMap e)))])}
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = addGroupToLastExp (rightOperator e)}
      | expressionType e == BOOLEXP =  BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addGroupToLastExp( rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {expLine = expLine e, expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addGroupToLastExp (groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addGroupToLastExp(assignExpression e)}
      | expressionType e == IDENTEXP = CallExpression {expLine = expLine e, expressionType = CALLEXP, callIdent = e, callParams = [], closedCall = False}  
      | expressionType e == INDEXEXP = IndexExpression{closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [addGroupToLastExp(last (arrayIndex e))]}
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = [GroupedExpression {expLine = expLine e, expressionType = GROUPEDEXP, closed=False, groupedExpression = Expression{expLine = expLine e, expressionType = EMPTYEXP}}]} 
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addGroupToLastExp(last (array e))]} 
      | otherwise = error ("addGroupToLastExp on line: " ++ (show (expLine e)) ++ " " ++ expressionToString(e)) 


closeLastGrouped :: Expression -> Expression
closeLastGrouped e = ex 
  where 
    ex 
      -- Found last grouped  
      | expressionType e == GROUPEDEXP && findGrouped (groupedExpression e) == False = GroupedExpression {expLine = expLine e, expressionType = GROUPEDEXP, closed = True, groupedExpression = groupedExpression e}  
      -- Found grouped nested 
      | expressionType e == GROUPEDEXP = GroupedExpression {expLine = expLine e, closed = closed e, expressionType = GROUPEDEXP, groupedExpression = closeLastGrouped (groupedExpression e)}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [closeLastGrouped(last (getVal e))])} 
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (pop (fst (mapMap e)) ++ [closeLastGrouped(last (fst( mapMap e)))], snd(mapMap e))}
      | expressionType e == MAPEXP && nextItem e == VAL = MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd(mapMap e))++ [closeLastGrouped(last (fst( mapMap e)))])}

      --Exists deeper inside left
      | expressionType e == OPERATOREXP && findGrouped (leftOperator e) == True = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, operator = operator e, leftOperator = closeLastGrouped (leftOperator e), rightOperator = rightOperator e}   
      | expressionType e == BOOLEXP && findGrouped (leftBool e) == True = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = closeLastGrouped (leftBool e), boolOperator = boolOperator e, rightBool = rightBool e} 
      -- Found grouped in right 
      | expressionType e == OPERATOREXP && findGrouped (rightOperator e) == True= OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, operator = operator e, rightOperator= closeLastGrouped (rightOperator e), leftOperator = leftOperator e} 
      | expressionType e == BOOLEXP && findGrouped (rightBool e) ==True = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = closeLastGrouped (rightBool e)}
      -- didn't find 
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = closeLastGrouped(assignExpression e)}
      | expressionType e == CALLEXP = CallExpression {expLine = expLine e, expressionType = CALLEXP, callParams = callParams e, callIdent = callIdent e, closedCall= True}
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [closeLastGrouped(last (array e))]} 
      | expressionType e == INDEXEXP = IndexExpression{closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [closeLastGrouped(last (arrayIndex e))]}
      | otherwise = error ("couldn't close last grouped on line: " ++ (show (expLine e)) ++ " " ++ expressionToString(e)) 


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


closeLastSta :: [Statement] -> [Statement]
closeLastSta s = sta 
  where   
    sta 
      | null s = s 
      | statementType (last s) == FUNCSTA && null (getBody s) == False && statementType (last (getBody s)) == FUNCSTA && closedSta (last (getBody s)) == False = pop s ++ [Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          statementUni = FuncStatement{
              params = params (statementUni (last s)),
              body = closeLastSta (getBody s)
            },
          expression = expression (last s)
        }] 
      | statementType (last s) == FUNCSTA = pop s ++ [Statement{
          closedSta = True, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          statementUni = statementUni (last s),
          expression = expression (last s)
        }]
      | otherwise = s


noPopAddToStatement:: (BlockType, [Statement], Statement) -> [Statement]
noPopAddToStatement(b, s, sta) = st 
  where 
    st 
      | b == CON = addToLastCon(s, sta)
      | b == ALT = addToLastAlt(s, sta)
      | b == EXP = s ++ [sta]
      | b == BOD && statementType (last s) == FUNCSTA && null (getBody s) == False && statementType (last (getBody s)) == FUNCSTA && closedSta (last (getBody s)) == False= pop s ++ [
      Statement {
        closedSta = False, 
        staLine = staLine (last s),
        statementType = statementType (last s), 
        statementUni = FuncStatement{
          params = getParams(s),
          body = noPopAddToStatement(b, getBody s, sta) 
        },
        expression = expression (last s) 
        }
      ]
      | b == BOD && statementType (last s) == FUNCSTA = pop s ++ [
      Statement {
        closedSta = False, 
        staLine = staLine (last s),
        statementType = statementType (last s), 
        statementUni = FuncStatement{
          params = getParams(s),
          body = getBody(s) ++ [sta] 
        },
        expression = expression (last s) 
        }]
      | b == BOD && statementType (last s) == FORSTA = pop s ++ [
        Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s), 
          statementUni = ForStatement{
            inc = inc (statementUni (last s)),
            start = start (statementUni (last s)),
            stop = stop (statementUni (last s)),
            forBody = forBody (statementUni (last s)) ++ [sta] 
          },
          expression = expression (last s) 
        }
      ]

addToLastCon :: ([Statement], Statement) -> [Statement]
addToLastCon (s, sta) = statement 
  where 
    statement 
      | null s = [sta]
      | statementType (last s) == IFSTA && getClosedCon s == False = pop s ++ [
        Statement{
            closedSta = False, 
            staLine = staLine (last s),
            statementType = IFSTA, 
            statementUni = IfStatement{
              closedCon = False,
              closedAlt = True,
              alt = [], 
              con = addToLastCon(getCon(s), sta) 
            },
            expression = expression (last s)
          }]
      | statementType (last s) == IFSTA && null (getAlt s) == False = pop s ++ [
        Statement{
            closedSta = False, 
            staLine = staLine (last s),
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
          closedSta = False, 
          staLine = staLine (last s),
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
      | statementType (last s) == IFSTA && getClosedCon s == False = pop s ++ [
          Statement{
              closedSta = False, 
              staLine = staLine (last s),
              statementType = IFSTA, 
              statementUni = IfStatement{
                closedCon = False,
                closedAlt = True,
                alt = [],
                con = addToLastAlt(getCon(s), sta) 
              },
              expression = expression (last s)
            }]
      | statementType (last s) == IFSTA && getClosedAlt s == False = pop s ++ [ 
          Statement{
              closedSta = False, 
              staLine = staLine (last s),
              statementType = IFSTA, 
              statementUni = IfStatement{
                closedCon = True,
                closedAlt = False,
                alt = addToLastAlt(getAlt(s), sta),
                con = getCon(s) 
              },
              expression = expression (last s)
            }]
      | statementType (last s) == FUNCSTA = pop s ++ [Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          statementUni = FuncStatement{
              params = getParams(s),
              body = addToLastAlt(getBody(s), sta)
            },
          expression = expression (last s)
        }]
      | otherwise = s ++ [sta]

addToStatement :: (BlockType, [Statement], Statement) -> [Statement]
addToStatement (b, s, sta) = st 
  where 
    st 
      | b == CON = pop s ++ [sta]
      | b == ALT = pop s ++ [sta]
      | b == PAR = pop s ++ [sta]
      | b == EXP = pop s ++ [sta]
      | b == START = pop s ++ [sta]
      | b == STOP = pop s ++ [sta]
      | b == INC = pop s ++ [sta]
      | b == BOD && statementType (last s) == FUNCSTA = pop s ++ [
      Statement {
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType(last s),
          statementUni = FuncStatement{
              params = getParams(s),
              body = pop (getBody(s)) ++ [last (body (statementUni(sta)))]
            },
          expression = expression (last s)
        }
      ]
      | b == BOD && statementType (last s) == FORSTA = pop s ++ [
        Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s), 
          statementUni = ForStatement{
            inc = inc (statementUni (last s)),
            start = start (statementUni (last s)),
            stop = stop (statementUni (last s)),
            forBody = pop (forBody (statementUni (last s)))  ++ [last (forBody (statementUni sta))] 
          },
          expression = expression (last s) 
        }
      ]


addToLastStatement :: (BlockType, Token, ExpressionType, [Statement]) ->Statement 
addToLastStatement (b, t, e, s) = sta 
  where 
    sta 
      -- REMOVE THIS AFTER TESTING COMPILER
      | null s = Statement{
          closedSta = False,
          staLine = 0,
          statementType = NOSTA,
          statementUni = NoStatement{},
          expression = addXToExp(b,t, e, Expression{expLine = 0, expressionType = EMPTYEXP})
        }
      | b == START && statementType (last s) == FORSTA = Statement{
          closedSta = False,
        staLine = staLine (last s),
        statementType = FORSTA,
        statementUni = ForStatement{
          start = addXToExp(b,t,e,start (statementUni (last s))),
          stop = stop (statementUni (last s)),
          inc = inc (statementUni (last s)),
          forBody = []
          },
        expression = expression (last s)
        } 
      | b == STOP && statementType (last s) == FORSTA = Statement{
          closedSta = False,
        staLine = staLine (last s),
        statementType = FORSTA,
        statementUni = ForStatement{
          start = start (statementUni (last s)),
          stop = addXToExp(b,t,e,stop(statementUni (last s))),
          inc = inc (statementUni (last s)),
          forBody = []
          },
        expression = expression (last s)
        } 
      | b == INC && statementType (last s) == FORSTA = Statement{
          closedSta = False,
        staLine = staLine (last s),
        statementType = FORSTA,
        statementUni = ForStatement{
          start = start (statementUni (last s)),
          stop = stop (statementUni (last s)),
          inc = addXToExp(b,t,e,inc(statementUni (last s))),
          forBody = []
          },
        expression = expression (last s)
        } 
      | b == PAR  && statementType (last s) == FUNCSTA && null (getBody s) == True = Statement{
        closedSta = False, 
        staLine = staLine (last s),
        statementType = FUNCSTA,
        statementUni = FuncStatement{
          params = pop (getParams(s)) ++ [addXToExp(b, t, e, getLastParam(getParams(s)))],
          body = []
          },
        expression = expression (last s)
        } 
      | b == PAR  && statementType (last s) == FUNCSTA = Statement{
        closedSta = False, 
        staLine = staLine (last s),
        statementType = FUNCSTA,
        statementUni = FuncStatement{
          params = params (statementUni (last s)),
          body = pop (getBody(s)) ++ [addToLastStatement(PAR, t, e, getBody(s))]  
          },
        expression = expression (last s)
        } 
      | (b == CON || b == ALT ) && statementType (last s) == FUNCSTA = Statement{closedSta = False, staLine = staLine (last s),statementType =FUNCSTA, expression = expression (last s), statementUni = FuncStatement{
          params = getParams(s),
          body = pop (getBody(s)) ++ [addToLastStatement(b, t, e, getBody(s))]
        }}
      | b == EXP && statementType (last s) == FUNCSTA && expressionType (expression (last s)) /= EMPTYEXP =  Statement {
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s), 
          statementUni = FuncStatement{
            params = getParams (s),
            body = pop (getBody(s)) ++ [addToLastStatement(EXP, t, e, getBody(s))]},  
          expression = expression (last s)
          }
      | b == BOD && statementType (last s) == FUNCSTA = Statement {
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s), 
          statementUni = FuncStatement{
            params = getParams(s),
            body = pop (getBody(s)) ++ [addToLastStatement(EXP, t, e, getBody(s))]}, 
          expression = expression (last s)
          }
      | b == BOD && statementType (last s) == FORSTA = Statement {
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s), 
          statementUni = ForStatement{
            start = start (statementUni (last s)),
            stop = stop (statementUni (last s)),
            inc = inc (statementUni (last s)),
            forBody = pop (forBody (statementUni (last s))) ++ [addToLastStatement(EXP, t, e, forBody (statementUni (last s)))]}, 
          expression = expression (last s)
          }
      --Adding in con 
      |  (b == CON || b == ALT ) && statementType (last s) == IFSTA && null (getCon(s)) == False && getClosedCon(s) == False = Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = IFSTA,
          statementUni = IfStatement{
            closedCon = False,
            closedAlt = True,
            con = pop (getCon(s)) ++ [addToLastStatement(b,t,e, getCon(s))],
            alt = []
            },
          expression = expression (last s)
        }
      -- Adding in Alt 
      | (b == CON || b == ALT ) && statementType (last s) == IFSTA &&  getClosedCon(s) == True = Statement {
          closedSta = False, 
          staLine = staLine (last s),
          statementType = IFSTA,
          statementUni = IfStatement{
              closedCon = True,
              closedAlt = False,
              con = getCon(s), 
              alt = pop (getAlt(s)) ++ [addToLastStatement(b,t,e, getAlt(s))]
            },
          expression = expression (last s)
        }
      | b == CON && statementType (last s) == IFSTA && (null (getCon s)) == True && (expressionType (expression (last s)) == GROUPEDEXP && closed (expression (last s)) == True)  = Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = IFSTA,
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = True,
              con = [Statement{statementType = NOSTA, staLine = -1, closedSta = False, statementUni = NoStatement{}, expression = addXToExp(b,t,e, Expression{expressionType = EMPTYEXP, expLine = -1})}], 
              alt = [] 
            },
          expression = expression (last s)
        } 
      --Found last statement
      | otherwise = Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType(last s),
          statementUni = statementUni(last s),
          expression = addXToExp(b, t, e, expression (last s))
        } 


getLastParam :: [Expression] -> Expression 
getLastParam e = 
  case e of 
    [] -> Expression{expLine = 0,  expressionType = EMPTYEXP}
    _ -> last e



addXToExp :: (BlockType, Token, ExpressionType, Expression) -> Expression 
addXToExp (b, t,et, e) = ex 
  where 
    ex 
      | typ t == INT = addIntToLastExp(b, t, e)
      | typ t == MINUS && et == PREFIXEXP =  addPrefixToLastExp(t, e)
      | typ t == MINUS = addOperatorToLastExp(t,e)
      | typ t == BANG = addPrefixToLastExp(t, e)
      | typ t == TRUE =  addTFToLastExp(t, e)
      | typ t == FALSE = addTFToLastExp(t, e)
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
      | typ t == STRING= addStringToLastExp(b,t,e)
      | typ t == LBRACKET = addArrayToLastExp(b,t,e)
      | typ t == COMMA = addArrayToLastExp(b,t,e)
      | typ t == LBRACE || typ t == COLON = addMapEmptyToLastExp(b,t,e)
      | otherwise = error (literal t)

addAssignToLastExp:: Expression -> Expression 
addAssignToLastExp e = ex 
  where
    ex
      | expressionType e == IDENTEXP || expressionType e == INDEXEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = e, assignExpression = Expression {expLine = expLine e,expressionType = EMPTYEXP}} 
      | expressionType e == EMPTYEXP = Expression {expLine = expLine e,expressionType = EMPTYEXP}
      | otherwise = error ("addAssignToLastExp on line: " ++ (show(expLine e)) ++" " ++(expressionToString e))

parseIdentifierToLet :: [Token] -> Statement
parseIdentifierToLet t = s
  where
    s  
      | typ (head t) == IDENT = Statement {closedSta = False, staLine = line (head t), statementType = LETSTA, statementUni= LetStatement {identifier = literal (head t)}, expression = Expression {expLine = line (head t), expressionType = EMPTYEXP}}
      | otherwise = error ("Error parsing identifier to let statement on line: " ++ (show(line(head t))))


addMapEmptyToLastExp:: (BlockType, Token, Expression) ->  Expression 
addMapEmptyToLastExp(b, t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = MapExpression{closedMap = False, nextItem = KEY, expLine = line t, expressionType = MAPEXP, mapMap= ([], [])}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addMapEmptyToLastExp(b,t,assignExpression e)}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP = MapExpression{nextItem = nextItem e, expLine = expLine e, closedMap = False, expressionType = MAPEXP, mapMap = (getKey e, pop (getVal e) ++ [addMapEmptyToLastExp(b,t,last (getVal e))])}
      | expressionType e == MAPEXP && nextItem e == KEY && typ t == COLON && length (fst (mapMap e)) - 1 == length (snd (mapMap e))= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) , snd (mapMap e) ++ [Expression{expLine = 0, expressionType = EMPTYEXP}])}
      | expressionType e == MAPEXP && nextItem e == VAL && typ t == COLON && length (fst (mapMap e)) == length (snd (mapMap e))= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) , pop (snd (mapMap e)) ++ [addMapEmptyToLastExp(b, t, (last (getVal e)))])}
      | expressionType e == MAPEXP && nextItem e == KEY && null (snd (mapMap e)) == False &&  typ t == COLON && length (fst (mapMap e)) - 1 == length (snd (mapMap e))= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) , snd (mapMap e) ++ [Expression{expLine = 0, expressionType = EMPTYEXP}])}
      | expressionType e == MAPEXP && nextItem e == VAL && typ t == LBRACE = MapExpression{closedMap = False, nextItem = VAL, expLine = line t, expressionType = MAPEXP, mapMap= (fst (mapMap e), pop (snd (mapMap e)) ++ [addMapEmptyToLastExp(b,t,last (snd (mapMap e)))])}
      | expressionType e == ARRAYEXP && null (array e) == True = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = [MapExpression{closedMap = False, nextItem = KEY, expLine = line t, expressionType = MAPEXP, mapMap= ([], [])}]} 
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addMapEmptyToLastExp(b, t, last (array e))]} 
      | otherwise = error ("Error adding map to last exp on line: " ++ (literal t)++ " " ++ (show (nextItem e)))


addIdentifierToLastExp:: (BlockType, Token, Expression) ->  Expression 
addIdentifierToLastExp(b, t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = IdentExpression{expLine = line t, expressionType = IDENTEXP,  ident = t}
      | expressionType e == MAPEXP && null (fst (mapMap e)) = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) ++ [addIdentifierToLastExp(b,t, Expression{expLine = expLine e, expressionType=EMPTYEXP})], snd (mapMap e))}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [addIdentifierToLastExp(b, t, last (getVal e))])} 
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (pop (fst (mapMap e)) ++ [addIdentifierToLastExp(b,t,last (fst (mapMap e)))], snd (mapMap e))}
      | expressionType e == MAPEXP && checkValidVal(e)= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [addIdentifierToLastExp(b,t, last (snd (mapMap e)))])}
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = [IdentExpression{expLine = line t, expressionType = IDENTEXP,  ident = t}]} 
      | expressionType e == ARRAYEXP && expressionType (last (array e)) == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addIdentifierToLastExp(b,t,last (array e))]}
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addIdentifierToLastExp(b, t, last (array e))]} 
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addIdentifierToLastExp(b, t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e,expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIdentifierToLastExp(b, t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addIdentifierToLastExp(b, t, groupedExpression e)}
      | expressionType e == PREFIXEXP = PrefixExpression {expLine = expLine e,expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = addIdentifierToLastExp(b, t, prefixExpression e)}
      | expressionType e == CALLEXP = CallExpression{expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addIdentifierToLastExp(b, t, getLastParam(callParams e))]}
      | expressionType e == IDENTEXP && b == PAR = CallExpression{expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = e, callParams = [IdentExpression{expLine = expLine e, expressionType = IDENTEXP,  ident = t}]}
      | expressionType e == INDEXEXP = IndexExpression {closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [addIdentifierToLastExp(b, t, last (arrayIndex e))]}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIdentifierToLastExp(b,t,assignExpression e)}
      | otherwise = error ("Error adding identifier to last exp on line: " ++ (show (line(t))) ++ literal t ++ " " ++ expressionToString e)

addArrayToLastExp:: (BlockType, Token, Expression) ->  Expression 
addArrayToLastExp(b, t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = ArrayExpression{closedArr = False, expLine = line t, expressionType = ARRAYEXP,  array = []}
      | expressionType e == ARRAYEXP && null(array e) == False && expressionType (last (array e)) == IDENTEXP =  ArrayExpression{closedArr = False, expLine = line t, expressionType = ARRAYEXP,  array = pop (array e) ++ [IndexExpression{closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = (last (array e)), arrayIndex = [Expression{expLine = -1, expressionType = EMPTYEXP}]}]}
      | expressionType e == ARRAYEXP && null(array e) == True && typ t == LBRACKET= ArrayExpression{closedArr = False, expLine = line t, expressionType = ARRAYEXP,  array = [addArrayToLastExp(b, t, Expression{expLine = 0, expressionType = EMPTYEXP})]}
      | expressionType e == ARRAYEXP && typ t == LBRACKET = ArrayExpression{closedArr = False, expLine = line t, expressionType = ARRAYEXP,  array = pop (array e) ++ [addArrayToLastExp(b, t, last (array e))]}
      | expressionType e == ARRAYEXP && typ t == COMMA && (expressionType (last (array e)) == ARRAYEXP && closedArr (last (array e)) == False || expressionType (last (array e)) == MAPEXP && closedMap (last (array e)) == False)= ArrayExpression{expLine = expLine e, expressionType = ARRAYEXP, closedArr = False, array = pop (array e) ++ [addArrayToLastExp(b,t, (last (array e)))]}
      | expressionType e == ARRAYEXP= ArrayExpression{closedArr = False, expLine = line t, expressionType = ARRAYEXP,  array = array e ++ [Expression{expLine = 0, expressionType = EMPTYEXP}]}
      | expressionType e == MAPEXP && nextItem e == VAL && typ t == COMMA && (length (fst (mapMap e))) == length (snd (mapMap e)) && (expressionType (last (snd (mapMap e))) /= ARRAYEXP || closedArr (last (snd (mapMap e))) == True)= MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) ++ [Expression{expLine = 0, expressionType = EMPTYEXP}], snd (mapMap e))}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [addArrayToLastExp(b, t, last (getVal e))])} 
      | expressionType e == MAPEXP && nextItem e == VAL = MapExpression{expLine = expLine e, expressionType = MAPEXP, nextItem = VAL, closedMap = False, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [addArrayToLastExp(b,t,last (snd (mapMap e)))])} 
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{expLine = expLine e, expressionType = MAPEXP, nextItem = KEY, closedMap = False, mapMap = (pop (fst (mapMap e)) ++ [addArrayToLastExp(b,t,(last (fst (mapMap e))))], snd (mapMap e) )} 
      | expressionType e == CALLEXP && typ t /= COMMA = CallExpression{expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addArrayToLastExp(b, t, getLastParam(callParams e))]}
      | expressionType e == CALLEXP = CallExpression{expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = (callParams e) ++ [Expression{expLine = -1, expressionType = EMPTYEXP}]}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addArrayToLastExp(b,t,assignExpression e)}
      | expressionType e == IDENTEXP = IndexExpression{closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = e, arrayIndex = [Expression{expLine = -1, expressionType = EMPTYEXP}]}
      | expressionType e == INDEXEXP && closedIndex e == False = IndexExpression {closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [addArrayToLastExp(b, t, last (arrayIndex e))]}
      | expressionType e == INDEXEXP = IndexExpression {closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = arrayIndex e ++ [Expression{expLine = expLine e, expressionType = EMPTYEXP}]}
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addArrayToLastExp(b, t, rightOperator e))}
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addArrayToLastExp(b, t, groupedExpression e)}
      | expressionType e == BOOLEXP = BoolExpression{expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addArrayToLastExp(b, t, rightBool e)}
      | otherwise = error ("Error adding array to last exp on line: "++ (show t) ++ " " ++ (expressionToString e)) 

addStringToLastExp:: (BlockType, Token, Expression) ->  Expression 
addStringToLastExp(b, t, e) = ex 
  where
    ex 
      | expressionType e == EMPTYEXP = StringExpression{expLine = line t, expressionType = STRINGEXP,  stringLiteral = t}
      | expressionType e == MAPEXP && null (fst (mapMap e)) = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e) ++ [addStringToLastExp(b,t, Expression{expLine = expLine e, expressionType=EMPTYEXP})], snd (mapMap e))}
      | expressionType e == MAPEXP && null (getVal e) == False && expressionType (last (getVal e)) == MAPEXP && closedMap (last (getVal e)) == False = MapExpression{nextItem = KEY, closedMap = False, expressionType = MAPEXP, expLine = expLine e, mapMap = (getKey e, pop (getVal e) ++ [addStringToLastExp(b, t, last (getVal e))])} 
      | expressionType e == MAPEXP && nextItem e == KEY && checkValidKey(e) = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (pop (fst (mapMap e)) ++ [addStringToLastExp(b,t, last (fst (mapMap e)))], snd (mapMap e))}
      | expressionType e == MAPEXP && checkValidVal(e)= MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [addStringToLastExp(b,t, last (snd (mapMap e)))])}
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = [StringExpression{expLine = line t, expressionType = STRINGEXP,  stringLiteral = t}]} 
      | expressionType e == ARRAYEXP && expressionType (last (array e)) == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addStringToLastExp(b,t,last (array e))]}
      | expressionType e == ARRAYEXP = ArrayExpression{closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addStringToLastExp(b, t, last (array e))]} 
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addStringToLastExp(b, t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e,expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addStringToLastExp(b, t,rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addStringToLastExp(b, t, groupedExpression e)}
      | expressionType e == PREFIXEXP = PrefixExpression {expLine = expLine e,expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = addStringToLastExp(b, t, prefixExpression e)}
      | expressionType e == CALLEXP = CallExpression{expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = pop (callParams e) ++ [addStringToLastExp(b, t, getLastParam(callParams e))]}
      | expressionType e == IDENTEXP && b == PAR = CallExpression{expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = e, callParams = [StringExpression{expLine = expLine e, expressionType = STRINGEXP,  stringLiteral = t}]}
      | expressionType e == INDEXEXP = IndexExpression {closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [addStringToLastExp(b,t, last(arrayIndex e))]}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addStringToLastExp(b,t,assignExpression e)}
      | otherwise = error ("Error adding string to last exp on line: " ++ (show (line(t))) ++ (show e) ++ " " ++ (show t)) 


changeSta :: (BlockType, Token, [Statement]) -> [Statement]
changeSta (b, t,s) = sta 
  where   
    sta 
      | statementType (last s) == NOSTA && typ t == ASSIGN = pop s ++ [Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = ASSIGNSTA,
          statementUni = AssignStatement{}, 
          expression = expression (last s)
        }]
      | statementType (last s) == NOSTA && typ t == LPAREN = pop s ++ [Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = FUNCSTA,
          statementUni = FuncStatement{params = [], body = []},
          expression = expression (last s) 
        }]
      | statementType (last s)== FUNCSTA && b == BOD = pop s ++ [Statement{
          closedSta = False,
          staLine = staLine (last s),
          statementType = FUNCSTA,
          statementUni = FuncStatement{params = getParams s, body = changeSta(b, t, getBody s)},
          expression = expression (last s) 
        }
      ]
      | statementType (last s)== FORSTA && b == BOD = pop s ++ [Statement{
          closedSta = False,
          staLine = staLine (last s),
          statementType = FORSTA,
          statementUni = ForStatement{
            start = start (statementUni (last s)), 
            stop = stop (statementUni (last s)), 
            inc = inc (statementUni (last s)), 
            forBody = changeSta(b, t, forBody (statementUni (last s)))},
          expression = expression (last s) 
        }
      ]
      | (b == CON || b == ALT) && statementType (last s) == IFSTA && getClosedCon s == False = pop s ++ [Statement{
          closedSta = False,
          staLine = staLine (last s),
          statementType = IFSTA,
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = False,
              con = changeSta(b,t,getCon s), 
              alt = []
            },
          expression = expression (last s)
        }] 
      | (b == CON || b == ALT) && statementType (last s) == IFSTA = pop s ++ [Statement{
          closedSta = False,
          staLine = staLine (last s),
          statementType = IFSTA,
          statementUni = IfStatement{
              closedCon = True,
              closedAlt = False,
              con = getCon s, 
              alt = changeSta(b,t,getAlt s) 
            },
          expression = expression (last s)
        }] 
      | (b == CON || b == ALT) && statementType (last s) == FUNCSTA = pop s ++ [Statement{
          closedSta = False,
          staLine = staLine (last s),
          statementType = FUNCSTA,
          statementUni = FuncStatement{
              params = getParams s,
              body = changeSta (b,t,getBody s)
            },
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
      | otherwise = error ("hasValidCondition on line: " ++ (statementToString (last s))) 

changeFuncToCall :: [Statement] -> [Statement] 
changeFuncToCall s = sta
  where
  sta   
      | statementType (last s) == FUNCSTA && null (getBody(s))  = pop s ++ [Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementUni=CallStatement{},
          statementType=CALLSTA, 
          expression = CallExpression {
            expLine = staLine (last s),
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
          closedSta = False, 
          staLine = staLine s,
          statementUni=CallStatement{},
          statementType=CALLSTA, 
          expression = CallExpression {
            expLine = staLine s, 
            expressionType = CALLEXP,
            callParams = params (statementUni s),
            callIdent= expression  s,
            closedCall = False 
          }}
      | statementType s == ASSIGNSTA || statementType s == RETSTA || statementType s == LETSTA = Statement {closedSta = False, staLine = staLine s, statementType = statementType s, statementUni = statementUni s, expression = changeLastFuncExp(expression s)} 

      | otherwise = error ("changeLastFuncToCall on line: " ++ (show (staLine s))) 

changeLastFuncExp :: Expression -> Expression 
changeLastFuncExp e = ex
  where   
    ex
      | expressionType e == IDENTEXP = CallExpression {
            expLine = expLine e,
            expressionType = CALLEXP,
            callParams = [],  
            callIdent= e,
            closedCall = False 
          }
      | expressionType e == PREFIXEXP = PrefixExpression {expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = changeLastFuncExp(prefixExpression e)}
      | expressionType e == OPERATOREXP = OperatorExpression {expLine = expLine e,expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = changeLastFuncExp(rightOperator e)}
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e,expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = changeLastFuncExp(rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = changeLastFuncExp(groupedExpression e), closed = closed e}
      | expressionType e == CALLEXP = CallExpression{
          expLine = expLine e,
          expressionType = CALLEXP, 
          callParams = callParams e,
          callIdent = callIdent e,
          closedCall = True
        }
      | expressionType e == ARRAYEXP = ArrayExpression {closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [changeLastFuncExp (last (array e))]}
      | expressionType e == ASSIGNEXP = AssignExpression{expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = changeLastFuncExp (assignExpression e)}
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedMap = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (pop (fst (mapMap e)) ++ [changeLastFuncExp(last (getKey e))], snd (mapMap e))}
      | expressionType e == MAPEXP = MapExpression{closedMap = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [changeLastFuncExp(last (getVal e))])}
      | expressionType e == INDEXEXP = IndexExpression{closedIndex = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = pop (arrayIndex e) ++ [changeLastFuncExp(last (arrayIndex e))]}
      | otherwise = error ("changeLastFuncExp on line: " ++ (show (expressionType e))) 

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
      | b == CON && null (getAlt(s)) == True = getLastNestedExpression(b, getCon(s))
      | b == CON = getLastNestedExpression(b, getAlt(s))
      | b == ALT && getClosedCon(s) == True = getLastNestedExpression(b, getAlt(s))
      | b == ALT = getLastNestedExpression(b, getCon(s))
      | otherwise = error ("getLastNestedExpression on line: " ++ (show (staLine (last s)))) 

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
      | otherwise = error ("getLastIfConditionType on line: " ++ (show (staLine (last s)))) 



addEmptyToLastParam :: Statement -> Statement 
addEmptyToLastParam s = sta 
  where 
    sta 
      | statementType s == FUNCSTA && null (body (statementUni s))= Statement{
        closedSta = False, 
        staLine = staLine s,
        statementType = FUNCSTA,
        statementUni = FuncStatement{
            body = [],
            params = params (statementUni s)++ [Expression{expLine = staLine s, expressionType = EMPTYEXP}]
          },
        expression = expression s
      }
      | statementType s == FUNCSTA = Statement{
        closedSta = False, 
        staLine = staLine s,
        statementType = FUNCSTA,
        statementUni = FuncStatement{
            body = pop (getBody [s]) ++ [addEmptyToLastParam(last (getBody [s]))],
            params = params (statementUni s) 
          },
        expression = expression s
      }
      | statementType s /= IFSTA = Statement{closedSta = False, staLine = staLine s, statementType = statementType s, statementUni = statementUni s, expression = addEmptyToLastParamExp(expression s)} 
      | otherwise = error ("addEmptyToLastParam on line: " ++ (show (staLine s))) 

addEmptyToLastParamExp :: Expression -> Expression 
addEmptyToLastParamExp e = ex 
  where 
    ex
      | expressionType e == CALLEXP = CallExpression{expLine = expLine e,expressionType = CALLEXP, closedCall = False, callIdent = callIdent e, callParams = callParams e ++ [Expression{expLine = expLine e, expressionType = EMPTYEXP}]} 
      | expressionType e == OPERATOREXP = OperatorExpression{expLine = expLine e,expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = addEmptyToLastParamExp(rightOperator e)}
      | expressionType e == BOOLEXP = BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addEmptyToLastParamExp(rightBool e)}
      | expressionType e == PREFIXEXP = PrefixExpression{expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = addEmptyToLastParamExp(prefixExpression e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addEmptyToLastParamExp(e)} 
      | expressionType e == ARRAYEXP = ArrayExpression {closedArr = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addEmptyToLastParamExp (last (array e))]}
      | expressionType e == ASSIGNEXP = AssignExpression {expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addEmptyToLastParamExp(assignExpression e)}
      | otherwise = error ("addEmptyToLastParamExp on line: " ++ (show (expLine e))) 

addIfToLastBlock:: (BlockType, [Statement]) -> [Statement]
addIfToLastBlock(b,s) = sta 
  where   
    sta 
      -- Add CON  
      | b == CON && statementType (last s) == IFSTA && getClosedCon(s) == False &&  (null(getCon(s)) == True || statementType (last (getCon(s))) /= IFSTA)= pop s ++ [
        Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = True,
              con = getCon(s) ++ [Statement{
                closedSta = False, 
                staLine = staLine (last s),
                statementType = IFSTA,
                expression = Expression{expLine = staLine (last s), expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = False, 
                  con = [], 
                  alt = [], 
                  closedAlt = True 
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
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = True,
              con = getCon(s) ++ [Statement{
                closedSta = False,
                staLine = staLine (last s),
                statementType = IFSTA,
                expression = Expression{expLine = staLine (last s), expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = False, 
                  con = addIfToLastBlock(CON, con(statementUni(last s))), 
                  alt = [], 
                  closedAlt = True 
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
          closedSta = False,
          staLine = staLine (last s),
          statementType = statementType (last s),
          statementUni = IfStatement{
              closedCon = True,
              closedAlt = False,
              con = getCon(s),
              alt =  getAlt(s) ++ [Statement{
                closedSta = False, 
                staLine = staLine(last s),
                statementType = IFSTA,
                expression = Expression{expLine = staLine (last s), expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = True, 
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
      | b == ALT && statementType (last s) == IFSTA && getClosedCon(s) == True && getClosedAlt s == False && (null (getAlt(s)) == True || statementType (last (getAlt(s))) /= IFSTA) = pop s ++ [
          Statement{
              closedSta = False, 
              staLine = staLine (last s),
              statementType = IFSTA, 
              statementUni = IfStatement{
                  closedCon = True,
                  con = getCon s,
                  closedAlt = False,
                  alt = pop (getAlt(s)) ++ [Statement{
                closedSta = False, 
                staLine = staLine (last s),
                statementType = IFSTA,
                expression = Expression{expLine = staLine (last s),expressionType = EMPTYEXP},
                statementUni = IfStatement{
                  closedCon = False, 
                  con = [], 
                  alt = [], 
                  closedAlt = True 
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
              closedSta = False, 
              staLine = staLine (last s),
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
              closedSta = False, 
              staLine = staLine (last s),
              statementType = IFSTA, 
              statementUni = IfStatement{
                  closedCon = False,
                  con = addIfToLastBlock(ALT, getCon(s)),
                  closedAlt = True,
                  alt = [] 
              },
              expression = expression (last s)
            }
      ] 
      --Is FUNCSTA
      | b == BOD && statementType (last s) == FUNCSTA = pop s ++ [
          Statement{
              closedSta = False, 
              staLine = staLine (last s),
              statementType = FUNCSTA, 
              statementUni = FuncStatement{
                  params = getParams(s),
                  body = getBody(s) ++ [Statement{
                    closedSta = False, 
                    staLine = staLine (last s),
                    statementType = IFSTA,
                    expression = Expression{expLine = staLine (last s), expressionType = EMPTYEXP},
                    statementUni = IfStatement{
                      closedCon = False, 
                      con = [], 
                      alt = [], 
                      closedAlt = True 
                      }
                    }
                ]
              },
              expression = expression (last s)
            }
        ]
      | otherwise = error ("addIfToLastBlock on line: " ++ (show (staLine (last s)))) 

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
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          statementUni = IfStatement{
            closedCon = True,
            closedAlt = True,
            con = getCon(s),
            alt = []},
            expression = expression (last s)}
          ] 
      -- Search in con
      | statementType (last s) == IFSTA && getClosedCon(s) == False && null(getAlt(s)) == True = pop s ++ [Statement{
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          expression = expression (last s),
          statementUni = IfStatement {
            closedCon = False,
            closedAlt = True,
            con = closeLastOpen(b, getCon(s)),
            alt = []}}]
      -- Close Alt
      | statementType (last s) == IFSTA && closedAlt(statementUni (last s)) == False && findLastOpen(getAlt(s)) == False = 
        pop s ++ [
        Statement{
          closedSta = False,
          staLine = staLine (last s),
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
          closedSta = False, 
          staLine = staLine (last s),
          statementType = statementType (last s),
          expression = expression (last s),
          statementUni = IfStatement {
            closedCon = True,
            closedAlt = False,
            con = getCon(s),
            alt = closeLastOpen(b, getAlt(s))}}]
      -- s is funcsta
      | statementType (last s) == FUNCSTA && b == ALT && statementType (last (getBody s)) == IFSTA && getClosedCon(getBody s)&& getClosedAlt(getBody s) = s 
      | statementType (last s) == FUNCSTA && b /= BOD = pop s ++ [Statement {
          closedSta = False, 
          staLine = staLine (last s),
          statementType = FUNCSTA,
          statementUni = FuncStatement{
              params = getParams(s),
              body = closeLastOpen(b, getBody(s))
            },
          expression = expression (last s)
        }] 
      | otherwise = error ("closeLastOpen on line: " ++ (statementsToString s))

findLastBlockType :: (BlockType, Statement) -> BlockType 
findLastBlockType (b, s) = block
  where   
    block 
      | statementType s == FUNCSTA && null (getBody [s]) == False && statementType (last (getBody [s])) == FUNCSTA = BOD
      | statementType s == FUNCSTA && null (getBody [s]) = b
      | statementType s == FUNCSTA = findLastBlockType(b, last (body(statementUni s)))
      | statementType s == IFSTA && closedCon (statementUni s) == False = findLastBlockType(CON, (last (con(statementUni s)))) 
      | statementType s == IFSTA && closedAlt (statementUni s) == False = findLastBlockType(ALT, (last (alt(statementUni s))))
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

isListExpression :: (BlockType, Statement) -> Bool
isListExpression (b,s) = bo 
  where 
    bo 
      | statementType s == IFSTA && closedCon (statementUni s) == False && null (getCon [s]) == True = False 
      | (statementType s == LETSTA || statementType s == ASSIGNSTA) && (expressionType (expression s) == EMPTYEXP || expressionType (expression s) == MAPEXP || expressionType (expression s) == ARRAYEXP)= True  
      | statementType s == IFSTA && closedCon (statementUni s)== False = isListExpression(b, last (getCon [s]))
      | statementType s == IFSTA = isListExpression(b, last (getAlt [s])) 
      | statementType s == FUNCSTA && null (getBody([s])) == False = isListExpression(b, last (getBody[s]))
      | otherwise = False 

isValidMap :: ([Statement]) -> Bool 
isValidMap (s) = bo 
  where 
    bo 
      | null s = False
      | expressionType (expression (last s)) == MAPEXP || expressionType (expression (last s))== ARRAYEXP = isValidMapArray(expression (last s))
      | statementType (last s) == IFSTA && closedCon (statementUni (last s)) == False = isValidMap(getCon(s)) 
      | statementType (last s) == IFSTA = isValidMap(getAlt(s)) 
      | statementType (last s) == FUNCSTA = isValidMap(getBody(s)) 
      | otherwise = error ("map is not valid" ++ (statementToString (last s)))

isValidMapArray :: Expression -> Bool 
isValidMapArray e = b 
  where 
    b
      | expressionType e == ARRAYEXP = isValidMapArray(last (array e))
      | expressionType e == MAPEXP && expressionType (last (getVal e)) == ARRAYEXP && closedArr (last (getVal e)) == False = isValidMapArray((last (getVal e))) 
      | expressionType e == MAPEXP && length (fst (mapMap e)) == length (snd(mapMap e)) && expressionType (last (snd(mapMap e))) /= EMPTYEXP = True
      | otherwise = False

checkValidVal:: Expression -> Bool 
checkValidVal e = length (fst (mapMap e)) == length (snd (mapMap e)) 

checkValidKey:: Expression -> Bool 
checkValidKey e = length (fst (mapMap e)) - 1== length (snd (mapMap e)) 

closeLastIndex :: (BlockType, Statement) -> Statement 
closeLastIndex (b, s) = sta 
  where 
    sta 
      | statementType s == IFSTA && getClosedCon([s]) == False = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = IFSTA,
          statementUni = IfStatement{
              closedCon = False,
              con = pop (getCon [s]) ++ [closeLastIndex(b, last (getCon( [s])))],
              alt = [],
              closedAlt = True 
            },
          expression = expression s
        } 
      | statementType s == IFSTA = Statement{
          closedSta = False,
          staLine = staLine s,
          statementType = IFSTA,
          statementUni = IfStatement{
              closedCon = True,
              con = con (statementUni s),
              alt = pop (getAlt [s]) ++ [closeLastIndex(b, last (getAlt ( [s])))],
              closedAlt = False
            },
          expression = expression s
        }
      | statementType s /= FUNCSTA = Statement{
          closedSta = False,
          staLine = staLine s,
          statementType = statementType s,
          statementUni = statementUni s,
          expression = closeLastIndexExp (expression s)
        }
      | statementType s == FUNCSTA && b == PAR = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s,
          statementUni = FuncStatement{
              params = pop (getParams([s])) ++ [closeLastIndexExp(last (getParams [s]))],
              body = []
            },
          expression = expression s
        }
      | statementType s == FUNCSTA = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s,
          statementUni = FuncStatement{
              params = params (statementUni s), 
              body = pop (getBody [s]) ++ [closeLastIndex(b, last (getBody [s]))] 
            },
          expression = expression s
        }
      | otherwise = error "closeLastIndex"

closeLastIndexExp :: Expression -> Expression 
closeLastIndexExp e =
  case expressionType e of 
    OPERATOREXP -> OperatorExpression{expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = closeLastIndexExp(rightOperator e)}
    BOOLEXP -> BoolExpression {expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = closeLastIndexExp(rightBool e)}
    INDEXEXP -> IndexExpression{closedIndex = True, expLine = expLine e, expressionType = INDEXEXP,  arrayIdent = arrayIdent e, arrayIndex = arrayIndex e}
    ARRAYEXP -> closeLastIndexArray e
    GROUPEDEXP -> GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = closeLastIndexExp (groupedExpression e), closed = False}
    ASSIGNEXP -> closeLastIndexAssign e 
    MAPEXP ->closeLastIndexMap e 
    _ -> error ("closeLastIndexExp" ++ expressionToString e)

closeLastIndexMap :: Expression -> Expression
closeLastIndexMap e = 
  case nextItem e of 
    VAL -> MapExpression{
        expLine = expLine e,
        expressionType = MAPEXP, 
        nextItem = nextItem e,
        closedMap = False,
        mapMap = (
          fst(mapMap e),
          pop (snd (mapMap e)) ++ [closeLastIndexExp(last (snd (mapMap e)))]
        )
      }
    KEY -> MapExpression{
        expLine = expLine e,
        nextItem = nextItem e,
        expressionType = MAPEXP, 
        closedMap = False,
        mapMap = (
          pop (fst (mapMap e)) ++ [closeLastIndexExp(last (fst (mapMap e)))],
          snd (mapMap e)
        )
      }
    

closeLastIndexAssign :: Expression -> Expression
closeLastIndexAssign e = ex 
  where 
    ex 
      | expressionType (assignExpression e) == EMPTYEXP = AssignExpression{
          expLine = expLine e,
          expressionType = ASSIGNEXP,
          assignExpression = assignExpression e,
          assignIdent = closeLastIndexExp (assignIdent e)
        }
      | otherwise = AssignExpression{
          expLine = expLine e,
          expressionType = ASSIGNEXP,
          assignIdent = assignIdent e,
          assignExpression = closeLastIndexExp(assignExpression e)
        } 


closeLastIndexArray :: Expression -> Expression 
closeLastIndexArray e = ex 
  where 
    ex 
      | null (array e) = ArrayExpression{
          closedArr = True, 
          expLine = expLine e,
          expressionType = ARRAYEXP,
          array = array e
        } 
      | expressionType (last (array e)) == INDEXEXP = ArrayExpression{
        closedArr = False,
        expLine = expLine e,
        expressionType = ARRAYEXP, 
        array = pop (array e) ++ [IndexExpression{expressionType = INDEXEXP, closedIndex = True, expLine = expLine (last (array e)), arrayIndex = arrayIndex (last (array e)), arrayIdent= arrayIdent (last (array e))}]
      }
      | expressionType (last (array e)) == ARRAYEXP = ArrayExpression{
          closedArr = False,
          expLine = expLine e,
          expressionType = ARRAYEXP, 
          array = pop (array e) ++ [closeLastIndexExp(last (array e))]
        } 
      | expressionType (last (array e)) == MAPEXP && closedMap (last (array e)) == False = ArrayExpression{
          closedArr = False,
          expLine = expLine e,
          expressionType = ARRAYEXP,
          array = pop (array e) ++ [closeLastIndexExp(last (array e))]
        }
      | otherwise = ArrayExpression{
          closedArr = True, 
          expLine = expLine e,
          expressionType = ARRAYEXP,
          array = array e
        } 
getVal:: Expression -> [Expression]
getVal e = snd (mapMap e)

getKey:: Expression -> [Expression]
getKey e = fst (mapMap e)

openLastAlt :: [Statement] -> [Statement]
openLastAlt s = sta 
  where 
    sta 
      | null s = error "open last alt null"
      | statementType (last s) == IFSTA && getClosedCon s == False = pop s ++ [Statement{
          closedSta = False, 
          statementType = IFSTA,
          staLine = staLine (last s),
          statementUni = IfStatement{
              closedCon = False,
              con = openLastAlt(getCon s), 
              alt = [],
              closedAlt = True 
            },
          expression = expression (last s)
        }]
      | statementType (last s) == IFSTA && getClosedAlt s == True = pop s ++ [Statement{
          closedSta = False, 
          statementType = IFSTA,
          staLine = staLine (last s),
          statementUni = IfStatement{
              closedCon = True,
              con = getCon s, 
              alt = [],
              closedAlt = False
            },
          expression = expression (last s)
        }]
      | statementType (last s) == IFSTA = pop s ++ [Statement{
          closedSta = False, 
          statementType = IFSTA,
          staLine = staLine (last s),
          statementUni = IfStatement{
              closedCon = True,
              con = getCon s, 
              alt = openLastAlt (getAlt s),
              closedAlt = False
            },
          expression = expression (last s)
        }]
      | statementType (last s) == FUNCSTA = pop s ++ [Statement{
          closedSta = False, 
          statementType = FUNCSTA, 
          staLine = staLine (last s),
          statementUni = FuncStatement{
              params = getParams s,
              body = openLastAlt(getBody s)  
            },
          expression = expression (last s)
        }]
      | otherwise = error (statementsToString s) 
checkValidSemicolon :: (BlockType, Statement)-> Bool 
checkValidSemicolon (b, s) = bo 
  where 
    bo
      | statementType s /= IFSTA = checkValidSemicolonExp (expression s)
      | statementType s == FUNCSTA && b == PAR = error ("error while parsing semicolon, not a valid statement on line: " ++ (show (staLine s))) 
      | otherwise = error "not valid statement"
checkValidSemicolonExp :: Expression -> Bool
checkValidSemicolonExp e = True

closeLastMap :: Statement -> Statement 
closeLastMap s = sta 
  where 
    sta 
      | statementType s == IFSTA && getClosedCon [s] == False = Statement{
          closedSta = False, 
          statementType = IFSTA,
          staLine = staLine s,
          statementUni = IfStatement{
              closedCon = False,
              con = pop (getCon [s])++ [closeLastMap(last (getCon [s]))], 
              alt = [],
              closedAlt = True 
            },
          expression = expression s
        }
      | statementType s == IFSTA = Statement{
          closedSta = False, 
          statementType = IFSTA,
          staLine = staLine s,
          statementUni = IfStatement{
              closedCon = True,
              con = getCon [s], 
              alt = pop (getAlt [s]) ++ [closeLastMap(last (getAlt [s]))],
              closedAlt = False
            },
          expression = expression s
        }
      | statementType s == FUNCSTA = Statement{
          closedSta = False, 
          statementType = FUNCSTA, 
          staLine = staLine s,
          statementUni = FuncStatement{
              params = getParams [s],
              body = pop (getBody [s]) ++ [closeLastMap(last (getBody [s]))]
            },
          expression = expression s
        }
      --FORSTA?
      | otherwise = Statement{
        closedSta = False,
        statementType = statementType s,
        staLine =staLine s,
        statementUni = statementUni s,
        expression = closeLastMapExp(expression s) 
      }


closeLastMapExp :: Expression -> Expression 
closeLastMapExp e = ex 
  where 
    ex 
      | expressionType e == MAPEXP && expressionType (last (snd (mapMap e))) == ARRAYEXP && closedArr (last (snd (mapMap e))) == False =MapExpression{
          nextItem = VAL, 
          closedMap = False, 
          expLine = expLine e, 
          expressionType = MAPEXP, 
          mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [closeLastMapExp (last (snd (mapMap e)))])
        } 
      | expressionType e == MAPEXP && expressionType (last (snd (mapMap e))) == MAPEXP && closedMap (last (snd (mapMap e))) == False = MapExpression{
          nextItem = VAL, 
          closedMap = False, 
          expLine = expLine e, 
          expressionType = MAPEXP, 
          mapMap = (fst (mapMap e), pop (snd (mapMap e)) ++ [closeLastMapExp (last (snd (mapMap e)))])
        }
      | expressionType e == ARRAYEXP = ArrayExpression{
          expLine = expLine e, 
          expressionType = ARRAYEXP, 
          array = pop (array e) ++ [closeLastMapExp(last (array e))], 
          closedArr = False 
        }
      | expressionType e == MAPEXP = MapExpression{
          nextItem = VAL, 
          closedMap = True, 
          expLine = expLine e, 
          expressionType = MAPEXP, 
          mapMap = mapMap e  
        } 
      | otherwise = error ("closeLastMapExp :"++expressionToString e)
