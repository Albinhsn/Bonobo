module Object where 

import Ast 

data ObjectType = ARRAY_OBJ | NULL_OBJ | INT_OBJ | BOOL_OBJ | STRING_OBJ deriving (Eq, Show) 

data Function = Function{funcIdent :: !String, funcParams :: ![Expression], funcBody :: ![Statement]} deriving (Eq, Show)

data Variable = Variable{varIdent :: !String, varValue :: Object} deriving (Eq, Show)

data Object 
  = NullObject {objectType :: !ObjectType}
  | IntObject {objectType :: !ObjectType, intValue :: !Int}
  | StringObject {objectType :: !ObjectType, stringValue :: !String}
  | BoolObject {objectType :: !ObjectType, boolValue :: !Bool}
  | ArrayObject {objectType :: !ObjectType, arrValue :: ![Object]}
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
removeVar :: (String, [Variable]) -> [Variable]
removeVar (s, v) =  va 
  where   
    va 
      | null (checkVar(s, v)) = error "variable doesn't exist"
      | otherwise = [x | x <- v, varIdent x /= s]

checkVar :: (String, [Variable]) -> [Variable] 
checkVar (s, v) = b 
  where 
    b   
      | null v = [] 
      | otherwise = [x | x <- v, varIdent x == s]


