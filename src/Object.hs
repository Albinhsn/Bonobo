module Object where 

import Ast 
import Token
import Utils
import Data.ByteString as BS


data ObjectType = FUNC_OBJ | MAP_OBJ | ARRAY_OBJ | NULL_OBJ | INT_OBJ | BOOL_OBJ | STRING_OBJ deriving (Eq, Show, Ord) 

data ReturnType = NONE | SMTH deriving (Eq, Show) 

data Function = Function{funcIdent :: !String, funcParams :: ![Expression], funcBody :: ![Statement]} deriving (Eq, Show)

data Variable = Variable{varIdent :: !String, varValue :: Object} deriving (Eq, Show)

data Object 
  = NullObject {objectType :: !ObjectType}
  | IntObject {objectType :: !ObjectType, intValue :: !Int}
  | StringObject {objectType :: !ObjectType, stringValue :: !String}
  | BoolObject {objectType :: !ObjectType, boolValue :: !Bool}
  | ArrayObject {objectType :: !ObjectType, arrValue :: ![Object]}
  | MapObject {objectType :: !ObjectType, mapValue :: ![(Object, Object)]}
  | FuncObject{objectType :: !ObjectType, funcValue :: ByteString}
  deriving(Eq, Show, Ord)



getFuncParams :: (String, [Function]) -> [Expression]
getFuncParams (s, f) = Prelude.head [funcParams x | x <- f, funcIdent x == s]

-- TODO error handle this
getVarValue :: (String, [Variable]) -> Object 
getVarValue (s, v) = o 
  where 
    o
      | Prelude.null v = error "null vars"
      | otherwise = Prelude.head [varValue x | x <- v, varIdent x == s] 



inspectFunction :: Function -> String
inspectFunction f = "fn " ++ (funcIdent f) ++ paramToString(funcParams f) ++ "{" ++ statementsToString(funcBody f) ++ "};"

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
      | objectType key == INT_OBJ && Prelude.null [x | x <- mapValue o, objectType (fst x) == INT_OBJ && intValue key == intValue (fst x)] == False  = True
      | objectType key == STRING_OBJ && Prelude.null [x | x <- mapValue o, objectType (fst x) == STRING_OBJ && stringValue key == stringValue (fst x)] == False = True
      | otherwise = False 

checkMapExists :: (Object, [(Object, Object)]) -> Bool 
checkMapExists (key, mp) = b
  where 
    b
      | objectType key == INT_OBJ && Prelude.null [fst x | x <- mp, objectType (fst x) == INT_OBJ &&  key == fst x] == False = True 
      | objectType key == STRING_OBJ && Prelude.null [fst x | x <- mp, objectType (fst x) == STRING_OBJ && key == fst x] == False = True  
      | otherwise = False


getMap :: (Object, [(Object, Object)]) -> Object 
getMap (key, mp) = Prelude.head [x | (i,x) <- mp, i == key]


getIndexOfStrKey :: (String, [Object]) -> Int 
getIndexOfStrKey (key,o) = Prelude.head [i | (i, x) <- Prelude.zip [0..] o, objectType x == STRING_OBJ && key == stringValue x]

getIndexOfIntKey :: (Int, [Object]) -> Int 
getIndexOfIntKey (key,o) = Prelude.head [i | (i, x) <- Prelude.zip [0..] o, objectType x == INT_OBJ && key == intValue x]

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

inspectObject :: Object -> String 
inspectObject o = 
  case objectType o of 
    NULL_OBJ -> "null"
    INT_OBJ -> show (intValue o)
    BOOL_OBJ -> show(boolValue o) 
    STRING_OBJ -> "'" ++ stringValue o ++ "'"
    ARRAY_OBJ -> "["++ Prelude.concat [inspectObject x ++ ", " | x <- arrValue o] ++ "]"
    MAP_OBJ -> "{" ++ Prelude.concat [inspectObject i ++ ":" ++ inspectObject x ++ ", " | (i, x) <- mapValue o] ++ "}"
    -- TODO FIX THIS
    FUNC_OBJ -> "fn " ++ "FUNC" ++ ";" 

inspectVariable :: Variable -> String 
inspectVariable v = (varIdent v) ++ " = " ++ (inspectObject (varValue v))
