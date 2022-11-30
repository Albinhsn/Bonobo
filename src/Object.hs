module Object where 

import Ast 
import Token
import Utils
import Data.ByteString as BS
import Debug.Trace

data ObjectType = FUNC_OBJ | MAP_OBJ | ARRAY_OBJ | NULL_OBJ | INT_OBJ | BOOL_OBJ | STRING_OBJ deriving (Eq, Show, Ord) 

data Object 
  = NullObject {objectType :: !ObjectType}
  | IntObject {objectType :: !ObjectType, intValue :: !Int}
  | StringObject {objectType :: !ObjectType, stringValue :: !String}
  | BoolObject {objectType :: !ObjectType, boolValue :: !Bool}
  | ArrayObject {objectType :: !ObjectType, arrValue :: ![Object]}
  | MapObject {objectType :: !ObjectType, mapValue :: ![(Object, Object)]}
  | FuncObject{objectType :: !ObjectType, numArgs :: !Int, numLocals :: !Int, funcValue :: ByteString}
  deriving(Eq, Show, Ord)

inspectObject :: Object -> String 
inspectObject o = 
  case objectType o of 
    NULL_OBJ -> "null"
    INT_OBJ -> show (intValue o)
    BOOL_OBJ -> show(boolValue o) 
    STRING_OBJ -> "'" ++ stringValue o ++ "'"
    ARRAY_OBJ -> "["++ Prelude.concat [inspectObject x ++ ", " | x <- arrValue o] ++ "]"
    MAP_OBJ -> "{" ++ Prelude.concat [inspectObject i ++ ":" ++ inspectObject x ++ ", " | (i, x) <- mapValue o] ++ "}"
    -- FUNC_OBJ -> "fn" 
    FUNC_OBJ -> "fn ( args: " ++show (numArgs o) ++ "){ locals: "++ show(numLocals o)++ " " ++ disassembleFunc("",funcValue o) ++ "};" 


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

disassembleFunc :: (String, ByteString) -> String 
disassembleFunc (s,b)= str 
  where 
    str 
      | BS.null b = s
      | BS.head b == 0 = disassembleFunc(s ++ " CONST " ++ (show (fromIntegral (BS.head (removeFirstInstruction2 b)))), removeFirstInstruction2(removeFirstInstruction2 b))
      | BS.head b == 1 = disassembleFunc(s ++ " POP", removeFirstInstruction2 b)
      | BS.head b == 2 = disassembleFunc(s ++ " ADD", removeFirstInstruction2 b)
      | BS.head b == 3 = disassembleFunc(s ++ " SUB", removeFirstInstruction2 b)
      | BS.head b == 4 = disassembleFunc(s ++ " MUL", removeFirstInstruction2 b)
      | BS.head b == 5 = disassembleFunc(s ++ " DIV", removeFirstInstruction2 b)
      | BS.head b == 6 = disassembleFunc(s ++ " TRUE", removeFirstInstruction2 b)
      | BS.head b == 7 = disassembleFunc(s ++ " FALSE", removeFirstInstruction2 b)
      | BS.head b == 8 = disassembleFunc(s ++ " GT", removeFirstInstruction2 b)
      | BS.head b == 9 = disassembleFunc(s ++ " LT", removeFirstInstruction2 b)
      | BS.head b == 10 = disassembleFunc(s ++ " NEQ", removeFirstInstruction2 b)
      | BS.head b == 11 = disassembleFunc(s ++ " EQ", removeFirstInstruction2 b)
      | BS.head b == 12 = disassembleFunc(s ++ " MINUS", removeFirstInstruction2 b)
      | BS.head b == 13 = disassembleFunc(s ++ " BANG", removeFirstInstruction2 b)
      | BS.head b == 14 = disassembleFunc(s ++ " JUMP", removeFirstInstruction2 b)
      | BS.head b == 15 = disassembleFunc(s ++ " JUMPNT", removeFirstInstruction2 b)
      | BS.head b == 16 = disassembleFunc(s ++ " SETGLOBAL " ++ (show (fromIntegral (BS.head (removeFirstInstruction2 b)))), removeFirstInstruction2(removeFirstInstruction2 b))
      | BS.head b == 17 = disassembleFunc(s ++ " GETGLOBAL " ++ (show (fromIntegral (BS.head (removeFirstInstruction2 b)))), removeFirstInstruction2(removeFirstInstruction2 b))
      | BS.head b == 18 = disassembleFunc(s ++ " ARRAY " ++(show (fromIntegral (BS.head (removeFirstInstruction2 b)))) , removeFirstInstruction2(removeFirstInstruction2 b))
      | BS.head b == 20 = disassembleFunc(s ++ " HASH "++(show (fromIntegral (BS.head (removeFirstInstruction2 b)))) , removeFirstInstruction2(removeFirstInstruction2 b))
      | BS.head b == 22 = disassembleFunc(s ++ " INDEX", removeFirstInstruction2 b)
      | BS.head b == 23 = disassembleFunc(s ++ " SETINDEX", removeFirstInstruction2 b)
      | BS.head b == 24 = disassembleFunc(s ++ " OPCALL " ++ (show (fromIntegral (BS.head (removeFirstInstruction2 b)))), removeFirstInstruction2(removeFirstInstruction2 b))
      | BS.head b == 25 = disassembleFunc(s ++ " RETURNVALUE", removeFirstInstruction2 b)
      | BS.head b == 26 = disassembleFunc(s ++ " OPRETURN", removeFirstInstruction2 b)
      | BS.head b == 27 = disassembleFunc(s ++ " SETLOCAL "++ (show (fromIntegral (BS.head (removeFirstInstruction2 b))))++" " ++ (show (BS.index (b) 2)) , removeFirstInstruction2(removeFirstInstruction2(removeFirstInstruction2 b)))
      | BS.head b == 28 = disassembleFunc(s ++ " GETLOCAL "++ (show (fromIntegral (BS.head (removeFirstInstruction2 b))))++ " " ++ (show (BS.index (b) 2)), removeFirstInstruction2(removeFirstInstruction2(removeFirstInstruction2 b)))
      | BS.head b == 29 = disassembleFunc(s ++ " GETPREBUILT "++ (show (fromIntegral (BS.head (removeFirstInstruction2 b))))++ " " ++ (show (BS.index (b) 2)), removeFirstInstruction2(removeFirstInstruction2(removeFirstInstruction2 b)))
      | otherwise = error ("disassembleFunc " ++ (show (BS.head b)))

removeFirstInstruction2:: ByteString -> ByteString 
removeFirstInstruction2 b = 
  case BS.length b of 
    0 -> error "can't remove instruction of length 0?"
    1 -> BS.empty :: ByteString 
    _ -> pack(removeFirst(unpack b))

isInt :: Object -> Bool
isInt o =
  case objectType o of
    INT_OBJ -> True
    _ -> error "can't access array with non int"


isWithinBounds :: (Int, Int) -> Bool
isWithinBounds (i, l) = b
  where 
    b
      | i < 0 = error "can't access array with negative index"
      | i >= l = error "trying to access array outside of bounds"
      | otherwise = True

getFuncs :: [Object] -> [Object] 
getFuncs o = [x | x <- o, objectType x == FUNC_OBJ] 

getObjectLen :: Object -> Int 
getObjectLen o= i 
  where 
    i 
      | objectType o == STRING_OBJ = Prelude.length (stringValue o)
      | objectType o == MAP_OBJ = Prelude.length (mapValue o)
      | objectType o == ARRAY_OBJ = Prelude.length (arrValue o)
      | otherwise = error "can't get len of this type"
