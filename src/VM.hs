module VM where 


import Object
import Code 
import Utils

import Debug.Trace
import Data.ByteString as BS

run :: ((ByteString, [Object]), [Object]) -> [Object]
run ((instructions,constants), stack) = ob 
  where 
    ob 
      | BS.null instructions = stack
      --Constant
      | BS.head instructions == 0 = run(pushToStack(removeFirstInstruction instructions, constants, stack))
      --Pop
      | BS.head instructions == 1 =
        -- trace ("removing " ++ (inspectObject (Prelude.head stack))) $ 
        run((removeFirstInstruction instructions, constants),removeFirst stack)
      --Add
      | BS.head instructions == 2 = 
        -- trace ("eval add op: " ++ Prelude.concat[inspectObject x ++ ", "| x <- stack]) $ 
        run((removeFirstInstruction instructions, constants), addOp stack)
      --Sub
      | BS.head instructions == 3 = 
        -- trace ("eval sub op: " ++ Prelude.concat[inspectObject x ++ ", "| x <- stack]) $ 
        run((removeFirstInstruction instructions, constants), subOp stack) 
      --Mul
      | BS.head instructions == 4 = 
        -- trace ("eval mul op: " ++ Prelude.concat[inspectObject x ++ ", "| x <- stack]) $ 
        run((removeFirstInstruction instructions, constants), mulOp stack) 
      --Div
      | BS.head instructions == 5 = 
        -- trace ("eval div op: " ++ Prelude.concat[inspectObject x ++ ", "| x <- stack]) $ 
        run((removeFirstInstruction instructions, constants), divOp stack) 
      --True
      | BS.head instructions == 6 = run((removeFirstInstruction instructions, constants),BoolObject{objectType = BOOL_OBJ, boolValue = True}:stack)
      --False 
      | BS.head instructions == 7 = run((removeFirstInstruction instructions, constants),BoolObject{objectType = BOOL_OBJ, boolValue = False}:stack)
      --GT 
      | BS.head instructions == 8 = run((removeFirstInstruction instructions, constants), gtOp stack) 
      --NEQ 
      | BS.head instructions == 10 = run((removeFirstInstruction instructions, constants), neqOp stack)   
      --EQ
      | BS.head instructions == 11 = run((removeFirstInstruction instructions, constants), eqOp stack)   
      --MINUS
      | BS.head instructions == 12 = run((removeFirstInstruction instructions, constants), minusOp stack)   
      --BANG
      | BS.head instructions == 13 = run((removeFirstInstruction instructions, constants), bangOp stack)   
      | otherwise = error "run" 


bangOp :: [Object] -> [Object]
bangOp stack = o 
  where 
    o
      | objectType (Prelude.head stack) /= BOOL_OBJ = error ("can't have bang prefix on non bool: " ++ (inspectObject (Prelude.head stack )))
      | otherwise = BoolObject{objectType = BOOL_OBJ, boolValue = not (boolValue (Prelude.head stack))}:(removeFirst stack)

minusOp :: [Object] -> [Object]
minusOp stack = o 
  where 
    o
      | objectType (Prelude.head stack) /= INT_OBJ = error ("can't have minus prefix on non integer: " ++ (inspectObject (Prelude.head stack )))
      | otherwise = IntObject{objectType = INT_OBJ, intValue = -1 * intValue (Prelude.head stack)}:(removeFirst stack)

eqOp :: [Object] -> [Object]
eqOp o = BoolObject{objectType = BOOL_OBJ, boolValue = o!!0 == o!!1}:(removeFirst(removeFirst o))

neqOp :: [Object] -> [Object]
neqOp o = BoolObject{objectType = BOOL_OBJ, boolValue = o!!0 /= o!!1}:(removeFirst(removeFirst o))


gtOp :: [Object] -> [Object]
gtOp o = evalGTOp(o!!0, o!!1):(removeFirst(removeFirst o))

evalGTOp :: (Object, Object) -> Object 
evalGTOp (o1, o2) = o
  where 
    o
      | objectType o1 /= INT_OBJ || objectType o2 /= INT_OBJ = error ("can't do greater then operation on non ints: " ++ inspectObject(o1)++ " " ++ inspectObject(o2))
      | otherwise = BoolObject{objectType = BOOL_OBJ, boolValue = intValue o1 < intValue o2}

addOp :: [Object] -> [Object]
addOp o =evalAddOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalAddOp:: (Object, Object) -> Object 
evalAddOp(o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ = trace ("Adding " ++ show(intValue o2 ) ++ " + " ++ (show (intValue o1))) $ IntObject{objectType = INT_OBJ, intValue = intValue o1 + intValue o2}
      | objectType o1 == STRING_OBJ = StringObject{objectType = STRING_OBJ, stringValue = stringValue o1 ++ stringValue o2}
      | otherwise = error ("can't do operation with types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))

subOp :: [Object] -> [Object]
subOp o =evalSubOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalSubOp :: (Object, Object) -> Object 
evalSubOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 /= INT_OBJ = error ("can't sub with non int type: " ++ (show (objectType o1))) 
      | objectType o1 == INT_OBJ = trace ("Subtracting " ++ show(intValue o1 ) ++ " - " ++ (show (intValue o2))) $ IntObject{objectType = INT_OBJ, intValue = intValue o2 - intValue o1}

divOp :: [Object] -> [Object]
divOp o = evalDivOp(o!!0, o!!1):(removeFirst (removeFirst o))

evalDivOp :: (Object, Object) -> Object 
evalDivOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 /= INT_OBJ = error ("can't div with non int type: " ++ (show (objectType o1))) 
      | objectType o1 == INT_OBJ = trace ("Dividing " ++ show(intValue o1 ) ++ " / " ++ (show (intValue o2))) $ IntObject{objectType = INT_OBJ, intValue = intValue o2 `div` intValue o1}

mulOp :: [Object] -> [Object]
mulOp o =evalMulOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalMulOp :: (Object, Object) -> Object 
evalMulOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 /= INT_OBJ = error ("can't mul with non int type: " ++ (show (objectType o1))) 
      | objectType o1 == INT_OBJ = trace ("Multiplying " ++ show(intValue o2 ) ++ " * " ++ (show (intValue o1))) $ IntObject{objectType = INT_OBJ, intValue = intValue o1 * intValue o2}

pushToStack :: (ByteString, [Object], [Object]) -> ((ByteString, [Object]), [Object]) 
pushToStack (instructions,constants, stack) = ob 
  where 
    ob 
      | otherwise = 
        trace ("pushing to stack: " ++ show (inspectObject (constants !! (fromIntegral (BS.head instructions))))) $ 
        ((removeFirstInstruction instructions, constants), constants !! (fromIntegral (BS.head instructions)):stack)


removeFirstInstruction :: ByteString -> ByteString 
removeFirstInstruction b = 
  case BS.length b of 
    0 -> error "can't remove instruction of length 0?"
    1 -> BS.empty :: ByteString 
    _ -> pack(removeFirst(unpack b))
