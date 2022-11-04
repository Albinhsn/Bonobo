module Eval where 

import Object
import Ast 
import Lexer 
import Token 
import Utils 
import ParserUtils

evaluateFunc :: (Expression, [Variable], [Function])-> (ReturnType,Object)
evaluateFunc (e,v, f) = o 
  where 
    o
      | expressionType e /= CALLEXP = error "not call expression"
      | identIsFunc(getCallLiteral e, f) && haveValidParams(
          getCallLiteral e ,
          callParams e,
          getFuncParams(getCallLiteral e, f)
        )
        = evaluateBody(getFuncBody(getCallLiteral e, f), (
          paramsToVars(
            v, 
            callParams e,
            getFuncParams(getCallLiteral e, f),
            f
            ), 
          f)
        )
      | otherwise = error (literal (ident(callIdent e)) ++ " is not a function")

evaluateBody :: ([Statement], ([Variable], [Function])) -> (ReturnType, Object)
evaluateBody (s, (v, f)) = o 
  where 
    o 
      | null s = (NONE, NullObject{objectType = NULL_OBJ})
      | statementType (head s) == RETSTA = (SMTH, evaluateExpression(expression (head s), v, f))
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
      | statementType s == LETSTA = (addVar(Variable{varIdent = identifier (statementUni s), varValue = evaluateExpression(expression s,v,f)}, v,f), f)
      | statementType s == FUNCSTA = addFunc(s, (v,f)) 
      | statementType s == CALLSTA = evaluateReturn(evaluateFunc(expression s,v, f),v,f)
      | statementType s == ASSIGNSTA = (evaluateAssign(assignIdent (expression s), v, evaluateExpression(assignExpression (expression s),v, f),f), f)
      | statementType s == IFSTA = (evaluateIf(s, (v, f)), f)
      | otherwise = error  ("evaluateStatement " ++ (show (statementType s)) ++ ": "++ statementToString s)

evaluateReturn :: ((ReturnType, Object), [Variable], [Function]) -> ([Variable], [Function])
evaluateReturn ((r,o), v,f) = (va, fu)
  where
    (va, fu)
      | r == NONE = (v,f)
      | otherwise = (v,f)


addFunc :: (Statement, ([Variable], [Function])) -> ([Variable], [Function])
addFunc (s, (v, f)) = (va, fu)
  where
    (va, fu)
      | null [x | x <- f, literal (ident (expression s)) == funcIdent x] == False = error ("redeclaration of func: " ++ (literal(ident (expression s)))) 
      | null [x | x <- v, literal (ident (expression s)) == varIdent x] == False = error ("redeclaration of variable: " ++ (literal(ident (expression s)))) 
      | otherwise = (v, f ++ [Function{funcIdent = literal (ident (expression s)), funcParams = params (statementUni s), funcBody = body(statementUni s)}])


evaluateAssign :: (Expression, [Variable], Object, [Function]) -> [Variable]
evaluateAssign (e,v,o,f) = va
  where 
    va
      | expressionType e /= INDEXEXP = replaceVar(Variable{varIdent = getLiteralFromAssign(e), varValue = o}, v)
      | checkListExists(getLAI e,v) == False = error "can't assign to non existing map/array" 
      | checkListType(getLAI e, v) == MAP_OBJ || checkListType (getLAI e, v) == ARRAY_OBJ = replaceVariable(
        getLAI e,
        v, 
        Variable{
          varIdent = getLAI e,
          varValue = newFunc(
            [evaluateExpression(x,v,f) | x <- arrayIndex e], 
            head [varValue x | x <- v, getLAI e == varIdent x],
            o
          )
        }
      )
      | otherwise = error ("evaluateAssign " ++ (expressionToString e) ++ " " ++ (inspectObject o))


replaceVariable :: (String, [Variable], Variable) -> [Variable]
replaceVariable (s, vars, newVar) = newVars
  where
    newVars 
      | otherwise = newVar:[x | x <- vars, varIdent x /= s] 


newFunc :: ([Object], Object, Object) -> Object 
newFunc (idxs, list, newVal) = o 
  where 
    o
      | length idxs == 1 && objectType list == ARRAY_OBJ = ArrayObject{
        objectType = ARRAY_OBJ,
        arrValue = replaceArrayIndex(
          head idxs, 
          arrValue list, 
          newVal
        )
      } 
      | objectType list == ARRAY_OBJ = ArrayObject{
          objectType = ARRAY_OBJ,
          arrValue = replaceArrayIndex(head idxs, arrValue list, newFunc(
            removeFirst idxs, 
            arrValue list!!(intValue (head idxs)),
            newVal
          ))
        } 
      | length idxs == 1 && objectType list == MAP_OBJ = MapObject{
        objectType = MAP_OBJ,
        mapValue = replaceMapKey(
          head idxs,
          mapValue list, 
          newVal
        ) 
      }
      | length idxs > 1 && objectType list == MAP_OBJ && checkKeyExists(head idxs, list) == False = error ("key doesn't exist and trying to access deeper, key: " ++ inspectObject(head idxs)) 
      | length idxs > 1 && objectType list == MAP_OBJ && checkKeyExists(head idxs, list )== True = MapObject{
          objectType = MAP_OBJ,
          mapValue = replaceMapKey(
            head idxs,
            mapValue list,
            newFunc(
              removeFirst idxs, 
              getMap(head idxs, mapValue list),
              newVal
            ) 
          )
        }
      | otherwise = error ("can't access index type: " ++ (show (objectType list)))


replaceArrayIndex :: (Object, [Object], Object) -> [Object]
replaceArrayIndex (idx, arr, newVal) = o 
  where 
    o
      | objectType idx /= INT_OBJ = error "can't use index that are isn't an int"
      | otherwise = replaceNth (intValue idx) newVal arr

replaceMapKey:: (Object, ([Object], [Object]), Object) -> ([Object], [Object]) 
replaceMapKey(key,(k,v),o2) = va 
  where 
    va 
      | objectType key /= INT_OBJ && objectType key /= STRING_OBJ = error "can't assign with keys that are non string/int"
      --If key exists replace it 
      | objectType key == INT_OBJ && checkMapExists(key, (k,v))= (k, replaceNth (getIndexOfIntKey(intValue key, k)) o2 v) 
      --If key doesn't exist, add it
      | objectType key == INT_OBJ =  (k ++ [IntObject{objectType = INT_OBJ, intValue = intValue key}], o2:v)
      --If key exists replace it 
      | objectType key == STRING_OBJ && checkMapExists (key, (k,v))= (k, replaceNth (getIndexOfStrKey(stringValue key, k)) o2 v)       
      | objectType key == STRING_OBJ = (k ++ [StringObject{objectType = STRING_OBJ, stringValue = stringValue key}], v ++ [o2])
      | otherwise = error "replaceMapKey"


getLAI :: Expression -> String 
getLAI e = literal (ident (arrayIdent e))


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
      | expressionType e == CALLEXP && isPrebuilt(getCallLiteral e) = evaluatePrebuilt(getCallLiteral e, [evaluateExpression(x,v,f) |  x <- callParams e]) 
      | expressionType e == CALLEXP = snd (evaluateFunc(e,v, f))
      | expressionType e == INDEXEXP = evaluateIndexExp(e,v,f)
      | expressionType e == ARRAYEXP = ArrayObject{objectType = ARRAY_OBJ, arrValue = [evaluateExpression (x, v,f) | x <- array e]}
      | expressionType e == MAPEXP = MapObject{objectType = MAP_OBJ, mapValue = ([evaluateExpression(x, v, f) | x <- fst(mapMap e)],[evaluateExpression(x,v,f) | x <- snd(mapMap e)])}
      | otherwise = error ("error evaluating expression" ++ expressionToString(e))

evaluateIndexExp :: (Expression, [Variable], [Function]) -> Object 
evaluateIndexExp (e,v,f )= o 
  where
    o 
      | expressionType (arrayIdent e) == IDENTEXP && checkListExists(literal(ident (arrayIdent e)), v) = evaluateIndex(arrayIndex e, getVarValue(literal(ident (arrayIdent e)), v),v,f) 
      | otherwise = error ("evaluateIndexExp " ++ (show (arrayIdent e))) 


evaluateIndex :: ([Expression], Object, [Variable], [Function]) -> Object 
evaluateIndex (e, o, v, f) = ob 
  where 
    ob
      | null e = o 
      | objectType o == MAP_OBJ = evaluateIndex(removeFirst e, evaluateMapIndex(evaluateExpression(head e, v, f),o), v, f)
      | objectType o == ARRAY_OBJ = evaluateIndex(removeFirst e, evaluateArrayIndex(evaluateExpression(head e, v, f),o),v,f)
      | otherwise = error "valuateIndex" 

evaluateArrayIndex :: (Object, Object ) -> Object 
evaluateArrayIndex (idx, arr) = o
  where 
    o
      | objectType idx /= INT_OBJ = error ("can't get array index with type: " ++ (show (objectType o)))
      | length (arrValue arr) < intValue idx = error "can't access out of bounds in array"
      | intValue idx < 0 = error "can't access with negative array"
      | otherwise = arrValue arr!!intValue idx


evaluateMapIndex :: (Object, Object ) -> Object 
evaluateMapIndex (key, val) = o
  where 
    o
      | checkKeyExists(key,val) == False = error "key doesn't exist"
      | objectType key == INT_OBJ = (snd (mapValue val))!!getIndexOfIntKey(intValue key, fst(mapValue val))
      | objectType key == STRING_OBJ =(snd (mapValue val))!!getIndexOfStrKey(stringValue key, fst(mapValue val))

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


getLength :: Object -> Int 
getLength o = i 
  where 
    i 
      | objectType o == ARRAY_OBJ = length (arrValue o) 
      | objectType o == MAP_OBJ = length (fst(mapValue o))
      | objectType o == STRING_OBJ = length (stringValue o)
      | otherwise = error ("can't get length of type: " ++ (show (objectType o)))

appendArr :: (Object, Object) -> Object 
appendArr (o1, o2) = ob 
  where 
    ob 
      | objectType  o1 /= ARRAY_OBJ = error ("can't append to type: " ++ show(objectType o1))
      | objectType o2 == ARRAY_OBJ || objectType o2 == MAP_OBJ = error "havn't implemented nested map/array"
      | otherwise = ArrayObject{objectType = ARRAY_OBJ, arrValue = arrValue o1 ++ [o2]}


isPrebuilt :: String -> Bool 
isPrebuilt s = s == "len" || s == "append"


evaluatePrebuilt :: (String,[Object]) -> Object 
evaluatePrebuilt (s, o) = ob 
  where
    ob
      | s == "len" && length o /= 1  = error ("len requires 1 param, can't be called with: " ++ show(length o)) 
      | s == "len" = IntObject{objectType = INT_OBJ, intValue = getLength (head o)}
      | s == "append" && length o /= 2 = error ("append takes 2 params not: " ++ (show(length o)))
      | s == "append" = appendArr(o!!0, o!!1)
      | otherwise = error "evaluatePrebuilt"

getCallLiteral :: Expression -> String 
getCallLiteral e = literal (ident (callIdent e))

