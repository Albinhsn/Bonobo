module ParserUtils where 

import Ast 
import Token 
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
      | expressionType e == INTEXP = error "shouldn't call this with int"
      | otherwise = error "addToLastRightOperator"

addBoolToLastExp :: (Token, Expression) -> Expression
addBoolToLastExp (t, e) = exp
  where
    exp 
      | expressionType e == INTEXP || expressionType e == INFIXEXP ||expressionType e == OPERATOREXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addBoolToLastExp (t, rightBool e)} 
      | expressionType e == GROUPEDEXP && closed e == False = GroupedExpression {expressionType = GROUPEDEXP, closed = closed e, groupedExpression = addBoolToLastExp (t, groupedExpression e)} 
      | expressionType e == GROUPEDEXP = BoolExpression {expressionType = BOOLEXP, leftBool = e, boolOperator = t, rightBool = Expression {expressionType = EMPTYEXP}} 
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
      | otherwise = error "addOperatorToLastExpression" 

addInfixToLastExp:: (Token, Expression) ->  Expression 
addInfixToLastExp(t, e) = exp 
  where
    exp 
      | expressionType e == EMPTYEXP = InfixExpression {expressionType = INFIXEXP, infixOperator = t, infixExpression = Expression {expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = (addInfixToLastExp(t, rightOperator e))}
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addInfixToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expressionType = GROUPEDEXP, closed=closed e, groupedExpression = addInfixToLastExp(t, groupedExpression e)}
      | otherwise = error "addInfixToLastRightOperator"

addTFToLastBool :: (Token, Expression) -> Expression 
addTFToLastBool (t, e) = exp
  where
    exp
      | expressionType e == BOOLEXP && expressionType (rightBool e) == EMPTYEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = TFExpression {expressionType = TFEXP, bool = typ t}} 
      | expressionType e == BOOLEXP = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addTFToLastBool(t, rightBool e)}
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
      | otherwise = error "error adding group to lat exp"

closeLastExpression :: Expression -> Expression 
closeLastExpression e = exp 
  where 
    exp 
      | expressionType e == GROUPEDEXP && closed e == False = GroupedExpression {expressionType = GROUPEDEXP, closed = True, groupedExpression = groupedExpression e}
      | expressionType e == OPERATOREXP && findGrouped (leftOperator e) == True = OperatorExpression {expressionType = OPERATOREXP, leftOperator = closeLastGrouped (leftOperator e), operator = operator e, rightOperator = rightOperator e}
      | expressionType e == OPERATOREXP && findGrouped (rightOperator e) == True = OperatorExpression {expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = closeLastGrouped (rightOperator e)}
      | expressionType e == BOOLEXP && findGrouped (leftBool e) == True = BoolExpression {expressionType = BOOLEXP, leftBool = closeLastGrouped (leftBool e), boolOperator = boolOperator e, rightBool = rightBool e}
      | expressionType e == BOOLEXP && findGrouped (rightBool e) == True = BoolExpression {expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = closeLastGrouped (rightBool e)} 
      | expressionType e == GROUPEDEXP = GroupedExpression {expressionType = GROUPEDEXP, closed=closed e, groupedExpression = closeLastExpression (groupedExpression e)}
      | otherwise = error "error closing expression"



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
