module Object where 

import Ast 
import Token
import Utils

data ObjectType = MAP_OBJ | ARRAY_OBJ | NULL_OBJ | INT_OBJ | BOOL_OBJ | STRING_OBJ deriving (Eq, Show) 

data Function = Function{funcIdent :: !String, funcParams :: ![Expression], funcBody :: ![Statement]} deriving (Eq, Show)

data Variable = Variable{varIdent :: !String, varValue :: Object} deriving (Eq, Show)

data Object 
  = NullObject {objectType :: !ObjectType}
  | IntObject {objectType :: !ObjectType, intValue :: !Int}
  | StringObject {objectType :: !ObjectType, stringValue :: !String}
  | BoolObject {objectType :: !ObjectType, boolValue :: !Bool}
  | ArrayObject {objectType :: !ObjectType, arrValue :: ![Object]}
  | MapObject {objectType :: !ObjectType, mapValue :: !([Object], [Object])}
  deriving(Eq, Show)



getFuncParams :: (String, [Function]) -> [Expression]
getFuncParams (s, f) = head [funcParams x | x <- f, funcIdent x == s]

-- TODO error handle this
getVarValue :: (String, [Variable]) -> Object 
getVarValue (s, v) = head [varValue x | x <- v, varIdent x == s] 


--TODO error handle this 
getFuncBody :: (String, [Function]) -> [Statement]
getFuncBody (s, f) = head [funcBody x | x <- f, funcIdent x == s] 

identIsFunc :: (String, [Function]) -> Bool 
identIsFunc (s, f) = (null [x | x <- f, funcIdent x == s]) == False

isVar :: (String, [Variable]) -> Bool 
isVar (s, v) = (null [x | x <- v, varIdent x== s]) == False

getFunc :: (String, [Function])-> Function 
getFunc (s, f) = head [x | x <- f, funcIdent x == s]

replaceVar :: (Variable, [Variable]) -> [Variable]
replaceVar (v, va) = removeVar(varIdent v, va) ++ [v]

addVar :: (Variable, [Variable]) -> [Variable]
addVar (v, va) = var 
  where 
    var 
      | isVar(varIdent v, va) == True = replaceVar(v, va)
      | otherwise = va ++ [v]

inspectVariable :: Variable -> String 
inspectVariable v = (varIdent v) ++ " = " ++ (inspectObject (varValue v))

inspectObject :: Object -> String 
inspectObject o = 
  case objectType o of 
    NULL_OBJ -> "null"
    INT_OBJ -> show (intValue o)
    BOOL_OBJ -> show(boolValue o) 
    STRING_OBJ -> "'" ++ stringValue o ++ "'"
    ARRAY_OBJ -> "["++ concat [inspectObject x ++ ", " | x <- arrValue o] ++ "]"
    MAP_OBJ -> "{" ++ (concat [inspectObject i ++ ":" ++ inspectObject x ++ ", " | (i, x) <- zip (fst(mapValue o)) (snd(mapValue o))]) ++ "}"


inspectFunction :: Function -> String
inspectFunction f = "fn " ++ (funcIdent f) ++ paramToString(funcParams f) ++ "{" ++ statementsToString(funcBody f) ++ "};"

removeVar :: (String, [Variable]) -> [Variable]
removeVar (s, v) =  va 
  where   
    va 
      | null (checkVar(s, v)) = error ("can't assign to non existing variable " ++ s ++ (concat [inspectVariable x | x <- v]))
      | otherwise = [x | x <- v, varIdent x /= s]

checkVar :: (String, [Variable]) -> [Variable] 
checkVar (s, v) = b 
  where 
    b   
      | null v = [] 
      | otherwise = [x | x <- v, varIdent x == s]

replaceArrayIndex :: (Expression, [Object], Object) -> [Object] 
replaceArrayIndex (e, o,o2) = o3
  where 
    o3  
      | expressionType e /= INTEXP = error "can't assign for non int index in array" 
      | length o-1 >= readIntFromString e && readIntFromString e >= 0 = replaceNth (readIntFromString e) o2 o  
      | length o-1 < readIntFromString e = error "trying to access out of bounds in array"
      | otherwise = error "trying to access with negative index"

readIntFromString :: Expression -> Int 
readIntFromString e = read (literal (integerLiteral e))

checkKeyExists :: (Object, Object) -> Bool 
checkKeyExists (key, o) = b
  where
    b 
      | objectType key == INT_OBJ && null [x | x <- fst(mapValue o), objectType x == INT_OBJ && intValue key == intValue x] == False  = True
      | objectType key == STRING_OBJ && null [x | x <- fst(mapValue o), objectType x == STRING_OBJ && stringValue key == stringValue x] == False = True
      | otherwise = False 

checkMapExists :: (Expression, ([Object], [Object])) -> Bool 
checkMapExists (e, (keys, vals)) = b
  where 
    b
      | expressionType e == INTEXP && null [x | x <- keys, objectType x == INT_OBJ && readIntFromString e  == intValue x] == False = True 
      | expressionType e == STRINGEXP && null [x | x <- keys, objectType x == STRING_OBJ && literal (stringLiteral e)  == stringValue x] == False = True  
      | otherwise = False

getIndexOfStrKey :: (String, [Object]) -> Int 
getIndexOfStrKey (key,o) = head [i | (i, x) <- zip [0..] o, objectType x == STRING_OBJ && key == stringValue x]

getIndexOfIntKey :: (Int, [Object]) -> Int 
getIndexOfIntKey (key,o) = head [i | (i, x) <- zip [0..] o, objectType x == INT_OBJ && key == intValue x]

replaceMapKey:: (Expression, ([Object], [Object]), Object) -> ([Object], [Object]) 
replaceMapKey(e,(k,v),o2) = va 
  where 
    va 
      | expressionType e /= INTEXP && expressionType e /= STRINGEXP = error "can't assign with keys that are non string/int"
      --If key exists replace it 
      | expressionType e == INTEXP && checkMapExists(e, (k,v))= (k, replaceNth (getIndexOfIntKey(readIntFromString e, k)) o2 v) 
      --If key doesn't exist, add it
      | expressionType e == INTEXP =  (k ++ [IntObject{objectType = INT_OBJ, intValue = readIntFromString e}], v ++ [o2])
      --If key exists replace it 
      | expressionType e == STRINGEXP && checkMapExists (e, (k,v))= (k, replaceNth (getIndexOfStrKey(literal (stringLiteral e), k)) o2 v)       --If key doesn't exist, add it
      | expressionType e == STRINGEXP = (k ++ [StringObject{objectType = STRING_OBJ, stringValue = literal (stringLiteral e)}], v ++ [o2])
      | otherwise = error "replaceMapKey"

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

getLiteralFromAssignIndex :: Expression -> String 
getLiteralFromAssignIndex e = literal (arrayIdent e) 
