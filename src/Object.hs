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
getVarValue (s, v) = o 
  where 
    o
      | null v = error "null vars"
      | otherwise = head [varValue x | x <- v, varIdent x == s] 


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


readIntFromString :: Expression -> Int 
readIntFromString e = i 
  where 
    i
      | expressionType e/= INTEXP = error (expressionToString e)
      | otherwise = read (literal (integerLiteral e))


checkKeyExists :: (Object, Object) -> Bool 
checkKeyExists (key, o) = b
  where
    b 
      | objectType key /= INT_OBJ && objectType key /= STRING_OBJ = error ("can't access map with key of type " ++ (show (objectType key)))
      | objectType key == INT_OBJ && null [x | x <- fst(mapValue o), objectType x == INT_OBJ && intValue key == intValue x] == False  = True
      | objectType key == STRING_OBJ && null [x | x <- fst(mapValue o), objectType x == STRING_OBJ && stringValue key == stringValue x] == False = True
      | otherwise = False 

checkMapExists :: (Object, ([Object], [Object])) -> Bool 
checkMapExists (key, (keys, vals)) = b
  where 
    b
      | objectType key == INT_OBJ && null [x | x <- keys, objectType x == INT_OBJ &&  key == x] == False = True 
      | objectType key == STRING_OBJ && null [x | x <- keys, objectType x == STRING_OBJ && key == x] == False = True  
      | otherwise = False


getMap :: (Object, ([Object],[Object])) -> Object 
getMap (key, (k,v)) = head [x | (i,x) <- zip k v, i == key]


getIndexOfStrKey :: (String, [Object]) -> Int 
getIndexOfStrKey (key,o) = head [i | (i, x) <- zip [0..] o, objectType x == STRING_OBJ && key == stringValue x]

getIndexOfIntKey :: (Int, [Object]) -> Int 
getIndexOfIntKey (key,o) = head [i | (i, x) <- zip [0..] o, objectType x == INT_OBJ && key == intValue x]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
 | n == 0 = newVal:xs
 | otherwise = x:replaceNth (n-1) newVal xs

getLiteralFromAssignIndex :: Expression -> String 
getLiteralFromAssignIndex e = s
  where
    s
      | expressionType e == IDENTEXP = literal (ident e) 
      | expressionType e == INDEXEXP = getLiteralFromAssignIndex(arrayIdent e)
