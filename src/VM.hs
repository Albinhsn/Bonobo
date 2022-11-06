module VM where 


import Object
import Code 
import Utils

import Debug.Trace
import Data.ByteString as DB



run :: ((ByteString, [Object]), [Object]) -> [Object]
run ((instructions,constants), stack) = ob 
  where 
    ob 
      | DB.null instructions = stack
      | DB.head instructions == 0 = run(pushToStack(removeFirstInstruction instructions, constants, stack))
      | DB.head instructions == 1 = run((removeFirstInstruction instructions, constants), addOp stack)
      | DB.head instructions == 2 =trace ("removing " ++ (inspectObject (Prelude.head stack))) $ run((removeFirstInstruction instructions, constants),removeFirst stack)
      | DB.head instructions == 3 = run((removeFirstInstruction instructions, constants), subOp stack) 
      | DB.head instructions == 4 = run((removeFirstInstruction instructions, constants), mulOp stack) 
      | DB.head instructions == 5 = run((removeFirstInstruction instructions, constants), divOp stack) 
      | otherwise = error "run" 

addOp :: [Object] -> [Object]
addOp o = evalAddOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalAddOp:: (Object, Object) -> Object 
evalAddOp(o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ = trace ("Adding " ++ show(intValue o2 ) ++ " to: " ++ (show (intValue o1))) $ IntObject{objectType = INT_OBJ, intValue = intValue o1 + intValue o2}
      | objectType o1 == STRING_OBJ = StringObject{objectType = STRING_OBJ, stringValue = stringValue o1 ++ stringValue o2}
      | otherwise = error ("can't do operation with types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))

subOp :: [Object] -> [Object]
subOp o = evalSubOp(o!!0, o!!1):(removeFirst (removeFirst o))

evalSubOp :: (Object, Object) -> Object 
evalSubOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 /= INT_OBJ = error ("can't sub with non int type: " ++ (show (objectType o1))) 
      | objectType o1 == INT_OBJ = trace ("Subtracting " ++ show(intValue o2 ) ++ " to: " ++ (show (intValue o1))) $ IntObject{objectType = INT_OBJ, intValue = intValue o1 - intValue o2}

divOp :: [Object] -> [Object]
divOp o = evalDivOp(o!!0, o!!1):(removeFirst (removeFirst o))

evalDivOp :: (Object, Object) -> Object 
evalDivOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 /= INT_OBJ = error ("can't div with non int type: " ++ (show (objectType o1))) 
      | objectType o1 == INT_OBJ = trace ("Dividing " ++ show(intValue o2 ) ++ " to: " ++ (show (intValue o1))) $ IntObject{objectType = INT_OBJ, intValue = intValue o1 `div` intValue o2}

mulOp :: [Object] -> [Object]
mulOp o = evalMulOp(o!!0, o!!1):(removeFirst (removeFirst o))

evalMulOp :: (Object, Object) -> Object 
evalMulOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 /= INT_OBJ = error ("can't mul with non int type: " ++ (show (objectType o1))) 
      | objectType o1 == INT_OBJ = trace ("Multiplying " ++ show(intValue o2 ) ++ " to: " ++ (show (intValue o1))) $ IntObject{objectType = INT_OBJ, intValue = intValue o1 * intValue o2}

pushToStack :: (ByteString, [Object], [Object]) -> ((ByteString, [Object]), [Object]) 
pushToStack (instructions,constants, stack) = ob 
  where 
    ob 
      | otherwise = trace ("pushing to stack: " ++ show (inspectObject (constants !! (fromIntegral (DB.head instructions))))) $ ((removeFirstInstruction instructions, constants), stack ++ [constants !! (fromIntegral (DB.head instructions))])


removeFirstInstruction :: ByteString -> ByteString 
removeFirstInstruction b = pack(removeFirst(unpack b))
