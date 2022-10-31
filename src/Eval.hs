module Eval where 

import Object
import Ast 
import Lexer 
import Token 
import Utils 
import ParserUtils


evaluateFunc :: (Expression, [Variable], [Function])-> Object
evaluateFunc (e, v, f) = o 
  where 
    o
      | expressionType e /= CALLEXP = error "not call expression"
      | identIsFunc(literal (ident (callIdent e)), f) && haveValidParams(
          literal(ident(callIdent e)),
          callParams e,
          getFuncParams(literal(ident(callIdent e)), f)
        )
        = evaluateBody(getFuncBody(literal(ident(callIdent e)), f), (
          v ++ paramsToVars(
            v, 
            callParams e,
            getFuncParams(literal(ident(callIdent e)), f),
            f
            ), 
          f)
        )
      | otherwise = error (literal (ident(callIdent e)) ++ " is not a function")

evaluateBody :: ([Statement], ([Variable], [Function])) -> Object
evaluateBody (s, (v, f)) = o 
  where 
    o 
      | null s = NullObject{objectType = NULL_OBJ}
      | statementType (head s) == RETSTA = evaluateExpression(expression (head s), v, f)
      | statementType (head s) == IFSTA && boolValue (evaluateExpression(expression (head s), v, f)) == True = evaluateBody((removeFirst s) ++ getCon(s), (v,f)) 
      | statementType (head s) == IFSTA  = evaluateBody((removeFirst s) ++ getAlt(s), (v,f)) 
      | otherwise = evaluateBody(removeFirst s, evaluateStatement (head s, (v, f)))


haveValidParams :: (String, [Expression], [Expression]) -> Bool 
haveValidParams (s, e,ex) = b 
  where 
    b 
      | null e && null ex /= True = error ("function " ++ s ++ " doesn't have required params: " ++ paramToString(ex))
      | length e < length ex = error ("function " ++ s ++ " misses " ++ (show ((length ex) - (length e))) ++ " params")
      | length e > length ex = error ("function " ++ s ++ " has " ++ (show ((length e) - (length ex))) ++ " too many params")
      | otherwise = True 

paramsToVars :: ([Variable], [Expression], [Expression], [Function]) -> [Variable] 
paramsToVars (v, [], [], f) = []
paramsToVars (v, (x: xs), (y: ys), f) = Variable{varIdent = literal(ident y), varValue = evaluateExpression(x, v, f)} : paramsToVars (v, xs, ys, f)



evaluateProgram :: ([Statement],([Variable], [Function]))-> ([Statement],([Variable], [Function])) 
evaluateProgram (s,(v, f)) = (st,(va, fu))
  where 
    (st, (va, fu))
      | null s = (s, (v, f))
      | otherwise = evaluateProgram(removeFirst s, evaluateStatement(head s, (v,f)))

evaluateStatement :: (Statement, ([Variable], [Function]))-> ([Variable],[Function]) 
evaluateStatement (s,(v, f))= (va, fu) 
  where 
    (va, fu) 
      | statementType s == LETSTA = (addVar(Variable{varIdent = identifier (statementUni s), varValue = evaluateExpression(expression s,v,f)}, v), f)
      | statementType s == FUNCSTA = (v, f ++ [Function{funcIdent = literal (ident (expression s)), funcParams = params (statementUni s), funcBody = body(statementUni s)}])
      | statementType s == CALLSTA = (addVar(Variable{varIdent = "EMPTY", varValue = evaluateFunc(expression s,v, f)},v), f)
      | statementType s == ASSIGNSTA = (evaluateAssign(assignIdent (expression s), v, evaluateExpression(assignExpression (expression s),v, f)), f)
      | statementType s == IFSTA = (evaluateIf(s, (v, f)), f)
      | otherwise = error  "evaluateStatement"


evaluateAssign :: (Expression, [Variable], Object) -> [Variable]
evaluateAssign (e,v,o) = va
  where 
    va
      | expressionType e /= INDEXEXP = replaceVar(Variable{varIdent = getLiteralFromAssign(e), varValue = o}, v)
      | checkListExists(getLiteralFromAssignIndex e,v)== False = error "can't assign to non existing array/map"
      | checkListType(getLiteralFromAssignIndex e, v) == ARRAY_OBJ =pop v ++ [Variable{varIdent = getLiteralFromAssignIndex e, varValue = ArrayObject{objectType = ARRAY_OBJ, arrValue = replaceArrayIndex(arrayIndex e, arrValue (varValue (head [x | x <- v, varIdent x == getLiteralFromAssignIndex e])), o)}}]
      | checkListType(getLiteralFromAssignIndex e, v) == MAP_OBJ = pop v ++ [Variable{varIdent = getLiteralFromAssignIndex e, varValue = MapObject{objectType = MAP_OBJ, mapValue = replaceMapKey(arrayIndex e, mapValue (varValue (head [x | x <- v, varIdent x == getLiteralFromAssignIndex e])), o)}}]


evaluateIf :: (Statement, ([Variable], [Function]))-> [Variable] 
evaluateIf (s, (v, f))= va 
  where 
    va 
      | boolValue (evaluateExpression(expression s, v, f)) == True = fst(snd(evaluateProgram(getCon([s]), (v, f))))
      | null (alt(statementUni s)) == False = fst(snd(evaluateProgram(getAlt([s]), (v, f))))
      | otherwise = [] 

evaluateExpression :: (Expression,[Variable], [Function])-> Object  
evaluateExpression (e, v, f)= o 
  where 
    o
      | expressionType e == INTEXP = IntObject{objectType = INT_OBJ, intValue = parseIntFromToken(integerLiteral e)}
      | expressionType e == STRINGEXP = StringObject{objectType = STRING_OBJ, stringValue = literal (stringLiteral e)}
      | expressionType e == TFEXP = BoolObject{objectType = BOOL_OBJ, boolValue = parseBoolFromTokenType(bool e)}
      | expressionType e == PREFIXEXP = parsePrefixObject(e,v, f) 
      | expressionType e == OPERATOREXP = evalOperation(operator e, evaluateExpression(leftOperator e, v, f), evaluateExpression(rightOperator e, v, f))
      | expressionType e == BOOLEXP = parseBoolObject(e,v, f)
      | expressionType e == GROUPEDEXP = evaluateExpression(groupedExpression e, v, f)
      | expressionType e == IDENTEXP = getVarValue(literal (ident (e)), v) 
      | expressionType e == ASSIGNEXP = evaluateExpression(assignExpression e, v, f)
      | expressionType e == CALLEXP = evaluateFunc(e, v, f) 
      | expressionType e == ARRAYEXP = ArrayObject{objectType = ARRAY_OBJ, arrValue = [evaluateExpression (x, v,f) | x <- array e]}
      | expressionType e == INDEXEXP = evaluateIndex(literal (arrayIdent (e)),evaluateExpression(arrayIndex e,v,f), v)
      | expressionType e == MAPEXP = MapObject{objectType = MAP_OBJ, mapValue = ([evaluateExpression(x, v, f) | x <- fst(mapMap e)],[evaluateExpression(x,v,f) | x <- snd(mapMap e)])}
      | otherwise = error ("error evaluating expression" ++ expressionToString(e))


evaluateIndex :: (String, Object, [Variable]) -> Object 
evaluateIndex (s, o, v) = ob 
  where 
    ob 
      | checkListExists(s,v)== False = error "array/map doesn't exist"
      | checkListType(s,v) == ARRAY_OBJ  = getArrayIndex(o,head [varValue x | x <- v, s == varIdent x])
      | checkListType(s,v) == MAP_OBJ = getMapIndex(o, head [varValue x | x <- v, s == varIdent x]) 
      | otherwise = error "array/map doesn't exist"  

checkListType :: (String, [Variable]) -> ObjectType
checkListType (s, v) = objectType (head [varValue x | x <- v, varIdent x == s]) 

getMapIndex :: (Object, Object) -> Object 
getMapIndex (key, o ) = ob
  where 
    ob 
      | objectType key /= INT_OBJ && objectType key /= STRING_OBJ = error ("can't acess map with anything other then int/string" ++ inspectObject (key) ++ " " ++ inspectObject (o))
      | checkKeyExists(key, o) == False = error ("key doesn't exist in map" ++ inspectObject key ++ " " ++ inspectObject o)
      | objectType key == INT_OBJ = head [y | (x,y) <- zip (fst (mapValue o)) (snd (mapValue o)), intValue key == intValue x]
      | objectType key == STRING_OBJ = head [y | (x,y) <- zip (fst (mapValue o)) (snd (mapValue o)), objectType x == STRING_OBJ && stringValue key == stringValue x] 
      | otherwise = error ("getMapIndex with key: " ++ inspectObject(key) ++ " map: " ++ inspectObject(o)) 



getArrayIndex :: (Object, Object) -> Object 
getArrayIndex (key,o ) = ob
  where 
    ob 
      | objectType key /= INT_OBJ = error "can't access array with other type then int" 
      | length (arrValue o)-1 >= intValue key && intValue key >= 0 = (arrValue o)!! intValue key 
      | length (arrValue o)-1 < intValue key = error "trying to access out of bounds in array"
      | otherwise = error "trying to access with negative index"

checkListExists :: (String, [Variable]) -> Bool
checkListExists (s,v) = null ([x | x <- v, varIdent x == s]) == False 


concatContext :: ([Variable],[Function]) -> String 
concatContext (v, f) =  pop (concat [inspectFunction x ++ " "| x <- f]++ "- " ++ concat [inspectVariable x ++ " "| x <- v])

parseIntFromToken :: Token -> Int 
parseIntFromToken t = read (literal t)

parseBoolFromTokenType :: TokenType -> Bool 
parseBoolFromTokenType t = 
  case t of 
    TRUE -> True 
    FALSE -> False 
    _ -> error "parseBoolFromTokenType"

parsePrefixObject :: (Expression, [Variable], [Function])-> Object 
parsePrefixObject (e,v,f) = o 
  where 
    o 
      | typ (prefixOperator e) == BANG = BoolObject{objectType = BOOL_OBJ, boolValue = parseBangValue(evaluateExpression(prefixExpression e, v, f))} 
      | typ (prefixOperator e) == MINUS = IntObject{objectType = INT_OBJ, intValue = parseMinusValue(evaluateExpression(prefixExpression e, v, f))}
      | otherwise = error "parsePrefixObject"


parseBangValue :: Object -> Bool 
parseBangValue o = b 
  where 
    b 
      | objectType o == BOOL_OBJ && boolValue o == True = False
      | objectType o == BOOL_OBJ && boolValue o == False = True 
      | otherwise = error "can't have bang prefix without bool expression" 

parseMinusValue :: Object -> Int
parseMinusValue o = i
  where 
    i 
      | objectType o == INT_OBJ =  -1 * (intValue o)
      | otherwise = error "can't have minus before non int"


evalOperation :: (Token, Object, Object) -> Object 
evalOperation (t, o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ literal t ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ = evalIntOp(t,intValue o1,intValue o2)
      | objectType o1 == STRING_OBJ = evalStringOp(t,stringValue o1,stringValue o2)
      | otherwise = error ("can't do operation with types: " ++ inspectObject(o1) ++ " " ++ literal t ++ " " ++ inspectObject(o2))

evalIntOp :: (Token, Int, Int) -> Object 
evalIntOp (t, o1, o2) = o
  where
    o 
      | typ t == PLUS = IntObject{objectType = INT_OBJ, intValue = o1 + o2 } 
      | typ t == MINUS = IntObject{objectType = INT_OBJ, intValue = o1 - o2} 
      | typ t == ASTERISK = IntObject{objectType = INT_OBJ, intValue = o1 * o2} 
      | typ t == SLASH = IntObject{objectType = INT_OBJ, intValue = o1 `div` o2} 
      | otherwise = error ("evalIntOp" ++ (literal t))


evalStringOp :: (Token, String, String) -> Object 
evalStringOp (t, s1, s2) = o 
  where 
    o 
      | typ t == PLUS = StringObject{objectType = STRING_OBJ, stringValue = s1 ++ s2}
      | otherwise = error ("can't do string operation with" ++ (literal t)) 


parseBoolObject :: (Expression, [Variable], [Function])-> Object 
parseBoolObject (e,v,f ) = o 
  where 
    o 
      | typ (boolOperator e) == EQUALS = BoolObject{objectType = BOOL_OBJ, boolValue = (evaluateExpression(leftBool e, v, f) == evaluateExpression(rightBool e, v,f ))}
      | typ (boolOperator e) == NOT_EQUALS=BoolObject{objectType = BOOL_OBJ, boolValue = (evaluateExpression(leftBool e, v,f ) /= evaluateExpression(rightBool e, v,f ))}
      | typ (boolOperator e) == GREATER_T= BoolObject{objectType = BOOL_OBJ, boolValue = evaluateGT(evaluateExpression(leftBool e, v,f ), evaluateExpression(rightBool e, v,f))} 
      | typ (boolOperator e) == LESS_T= BoolObject{objectType = BOOL_OBJ, boolValue = not (evaluateGT(evaluateExpression(leftBool e, v,f ), evaluateExpression(rightBool e, v,f)))} 
      | otherwise = error "parseBoolObject"

evaluateGT :: (Object, Object) -> Bool 
evaluateGT (o1, o2) = b 
  where   
    b 
      | objectType o1 == INT_OBJ && objectType o2 == INT_OBJ && intValue o1 > intValue o2 = True 
      | objectType o1 == INT_OBJ && objectType o2 == INT_OBJ = False 
      | otherwise = error "can't evaluate since both are not ints"

evaluateLT :: (Object, Object) -> Bool 
evaluateLT (o1, o2) = True

getLiteralFromAssign :: Expression-> String 
getLiteralFromAssign e = literal (ident e) 

