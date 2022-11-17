module VM (
  run,
  VM(instruct, constVM, global, stack), 
  runVM,
  addIndexToStack,
  replaceIndex,
  isInt,
  replaceNth,
  updateMapKey,
  evalIndex,
  isWithinBounds,
  evalMapIndex,
  evalArrayIndex,
  addEleToMap,
  addEleToArray,
  evalGetGlobal,
  evalSetGlobal,
  evalJump,
  evalJumpNT,
  bangOp,
  minusOp,
  eqOp,
  neqOp,
  gtOp,
  evalGTOp,
  addOp,
  evalAddOp,
  subOp,
  evalSubOp,
  divOp,
  evalDivOp,
  mulOp,
  evalMulOp,
  pushToStack,
  removeNInstructions,
  removeFirstInstruction
)where 


import Object
import Code 
import Utils

import Debug.Trace
import Data.ByteString as BS
import Data.Map


run :: Compiler -> VM 
run c = runVM(
    VM{
        instruct = bytes c,
        constVM = constants c,
        global = [],
        stack = []
      }
  )


data VM = VM{
    instruct :: !ByteString,
    constVM:: ![Object],
    global :: ![(Int, Object)],
    stack :: ![Object] 
  }

runVM :: VM -> VM
runVM v = vm  
  where 
    vm 
      | BS.null (instruct v)= v 
      --Constant
      | BS.head (instruct v)== 0 = runVM(pushToStack(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v, 
          global = global v, 
          stack = stack v
        }))
      --Pop
      | BS.head (instruct v)== 1 =
        runVM(VM{
            instruct = removeFirstInstruction (instruct v),
            constVM = constVM v, 
            global = global v, 
            stack = removeFirst (stack v)
          })
      --Add
      | BS.head (instruct v)== 2 = 
        runVM(VM{
            instruct = removeFirstInstruction (instruct v), 
            constVM = constVM v, 
            global = global v,
            stack =addOp (stack v)
          })
      --Sub
      | BS.head (instruct v)== 3 = 
        runVM(VM{
            instruct = removeFirstInstruction (instruct v), 
            constVM = constVM v, 
            global = global v,
            stack =subOp (stack v)
          })
      --Mul
      | BS.head (instruct v)== 4 = 
        runVM(VM{
            instruct = removeFirstInstruction (instruct v), 
            constVM  = constVM v, 
            global = global v,
            stack =mulOp (stack v)
          })
      --Div
      | BS.head (instruct v)== 5 = 
        runVM(VM{
            instruct = removeFirstInstruction (instruct v), 
            constVM = constVM v, 
            global = global v,
            stack =divOp (stack v)
          })
      --True
      | BS.head (instruct v)== 6 = runVM(VM{
          instruct = removeFirstInstruction (instruct v), 
          constVM = constVM v, 
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = True}:(stack v)
        }
      )
      --False 
      | BS.head (instruct v)== 7 = runVM(VM{
          instruct = removeFirstInstruction (instruct v), 
          constVM = constVM v, 
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = False}:(stack v)
        })
      --GT 
      | BS.head (instruct v)== 8 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v,
          stack = gtOp (stack v)
        }
      ) 
      --NEQ 
      | BS.head (instruct v)== 10 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v, 
          stack = neqOp (stack v)
        }) 
      --EQ
      | BS.head (instruct v)== 11 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v, 
          stack = eqOp (stack v)
        }) 
      --MINUS
      | BS.head (instruct v)== 12 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v, 
          stack = minusOp (stack v)
        }) 
      --BANG
      | BS.head (instruct v)== 13 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v, 
          stack = bangOp (stack v)
        }) 
      -- JUMP
      | BS.head (instruct v)== 14 = runVM(evalJump(v))
      -- JUMPNT
      | BS.head (instruct v)== 15 = runVM(evalJumpNT(v))
      -- SETGLOBAL
      | BS.head (instruct v) == 16 = runVM(evalSetGlobal(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))
      -- GETGLOBAL
      | BS.head (instruct v)== 17 = runVM(evalGetGlobal(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))  
      -- ARRAY
      | BS.head (instruct v)== 18 = 
        trace ("Found array instructions: " ++ Prelude.concat [inspectObject o | o <- stack v])
        runVM(addEleToArray (VM{
            instruct = removeFirstInstruction (instruct v),
            constVM = constVM v,
            global = global v,
            stack = stack v
          }, ArrayObject{objectType = ARRAY_OBJ, arrValue = []}))  
      -- ARRAYEND
      | BS.head (instruct v)== 19 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v, 
          global = global v, 
          stack = NullObject{objectType = NULL_OBJ}:stack v
        })  
      -- HASH 
      | BS.head (instruct v)== 20 = 
        trace ("Found map instructions: " ++ Prelude.concat [inspectObject o | o <- stack v])
        runVM(addEleToMap (VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = global v,
          stack = stack v
        }, MapObject{objectType = MAP_OBJ, mapValue = []})) 
      -- HASHEND
      | BS.head (instruct v)== 21 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v, 
          global = global v, 
          stack = NullObject{objectType = NULL_OBJ}:stack v
        })  
      -- INDEX
      | BS.head (instruct v)== 22 = runVM(addIndexToStack(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v, 
          global = global v,
          stack = stack v
        }))
      -- SETINDEX
      | BS.head (instruct v)== 23 = runVM(VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v, 
          global = global v,
          stack = evalAssignIndex(
            removeFirst(removeFirst(stack v)),
            stack v!!0,
            stack v!!1
          ):[] 
        })
      -- OPCALL 
      | BS.head (instruct v) == 24 = runVM(runFunc(v))
      -- RETURNVALUE 
      | BS.head (instruct v) == 25 = VM{
          instruct = BS.empty :: ByteString,
          constVM = [],
          global = [],
          stack = [Prelude.head (stack v)]
        } 
      -- OPRETURN 
      | BS.head (instruct v) == 26 = VM{
          instruct = BS.empty :: ByteString,
          constVM = [],
          global = [],
          stack = [NullObject{objectType = NULL_OBJ}]
        } 
      | otherwise = error "run" 


runFunc :: VM -> VM 
runFunc v = VM{
    instruct = removeFirstInstruction (instruct v),
    constVM = constVM v, 
    global = global v, 
    stack = checkReturn(v, Prelude.head (stack (runVM(VM{
        instruct = funcValue (Prelude.head (stack v)),
        constVM = constVM v, 
        global = global v,
        stack = []
      }))))
  } 

checkReturn :: (VM, Object) -> [Object]
checkReturn (v,o) = obj 
  where 
    obj   
      | objectType o == NULL_OBJ = error "null return"
      | otherwise = o:(removeFirst (stack v))


evalAssignIndex :: ([Object],Object, Object) -> Object 
evalAssignIndex  (st ,list, newVal) = newList 
  where 
    newList 
      | Prelude.length st == 1 = replaceIndex (list, (Prelude.head st), newVal) 
      | otherwise = replaceIndex(
        list, 
        Prelude.head st, 
        evalAssignIndex(
          removeFirst st, 
          evalIndex (Prelude.head st, list),
          newVal
        )
      ) 

addIndexToStack :: VM -> VM 
addIndexToStack v = VM{
          instruct = instruct v,
          constVM = constVM v, 
          global = global v, 
          stack = (evalIndex (stack v!!0, stack v!!1)):(removeFirst(removeFirst (stack v)))
        } 

replaceIndex :: (Object, Object, Object) -> Object 
replaceIndex (l, idx, newVal) = newL 
  where 
    newL 
      | objectType l == ARRAY_OBJ && isInt idx && isWithinBounds (intValue idx, Prelude.length (arrValue l))= ArrayObject{objectType = ARRAY_OBJ, arrValue = replaceNth (intValue idx) newVal (arrValue l)}
      | objectType l == MAP_OBJ = MapObject{objectType = MAP_OBJ, mapValue = updateMapKey(idx, newVal, mapValue l)} 
      | otherwise = error ("trying to index non array/map" ++ inspectObject l) 

isInt :: Object -> Bool
isInt o =
  case objectType o of
    INT_OBJ -> True
    _ -> error "can't access array with non int"

updateMapKey :: (Object, Object, [(Object, Object)]) -> [(Object, Object)]
updateMapKey (k,v,m) = newMap 
  where 
    newMap 
      | member k (fromList m) == False = toList(insert k v (fromList m))
      | otherwise = toList(insert k v (delete k (fromList m))) 

evalIndex :: (Object, Object) -> Object 
evalIndex (k,l) = val 
  where 
    val 
      | objectType l == MAP_OBJ = evalMapIndex(k, l)
      | objectType l == ARRAY_OBJ = evalArrayIndex(k, l)
      | otherwise = error ("k: "++  inspectObject k ++ " l:" ++ inspectObject l) 

isWithinBounds :: (Int, Int) -> Bool
isWithinBounds (i, l) = b
  where 
    b
      | i < 0 = error "can't access array with negative index"
      | i >= l = error "trying to access array outside of bounds"
      | otherwise = True

evalMapIndex :: (Object, Object) -> Object 
evalMapIndex (key, mp) = val
  where 
    val 
      | checkKeyExists(key, mp) = getMap (key, mapValue mp) 
      | otherwise = error "key doesn't exist"

evalArrayIndex :: (Object, Object) -> Object 
evalArrayIndex (idx, arr) = val
  where 
    val 
      | objectType idx == INT_OBJ && isWithinBounds(intValue idx, Prelude.length (arrValue arr)) = arrValue arr!!intValue idx 
      | otherwise = error "can't access array with non int key"

addEleToMap :: (VM, Object) -> VM 
addEleToMap (v, mp) = vm 
  where 
    vm 
      | Prelude.null (stack v) = error (Prelude.concat [inspectObject (fst o) ++ ":" ++ inspectObject (snd o) | o <- mapValue mp])
      | objectType (Prelude.head (stack v)) == NULL_OBJ = 
        trace ("Finished map: " ++ Prelude.concat [inspectObject o | o <- stack v])
        VM{
          instruct = instruct v,
          constVM = constVM v,
          global = global v,
          stack = mp:(removeFirst (stack v))
        }
      | otherwise = 
        trace ("Found another element: " ++ Prelude.concat [inspectObject o | o <- stack v]) $
        addEleToMap(VM{
          instruct = instruct v,
          constVM = constVM v,
          global = global v, 
          stack = removeFirst(removeFirst(stack v))
        }, MapObject{objectType = MAP_OBJ, mapValue = mapValue mp ++ [(Prelude.head (stack v), stack v!!1)]})

addEleToArray :: (VM, Object) -> VM 
addEleToArray (v, arr) = vm 
  where 
    vm 
      | Prelude.null (stack v) = error (Prelude.concat [inspectObject o | o <- arrValue arr])
      | objectType (Prelude.head (stack v)) == NULL_OBJ = 
        trace ("Finished array: " ++ Prelude.concat [inspectObject o | o <- stack v])
        VM{
          instruct = instruct v,
          constVM = constVM v,
          global = global v,
          stack = arr:(removeFirst (stack v))
        }
      | otherwise = 
        trace ("Found another element: " ++ Prelude.concat [inspectObject o | o <- stack v]) $
        addEleToArray(VM{
          instruct = instruct v,
          constVM = constVM v,
          global = global v, 
          stack = removeFirst(stack v) 
        }, ArrayObject{objectType = ARRAY_OBJ, arrValue = arrValue arr ++ [Prelude.head (stack v)]})

evalGetGlobal :: VM ->  VM 
evalGetGlobal v = VM{
    instruct = removeFirstInstruction (instruct v), 
    constVM = constVM v,
    global = global v, 
    stack = fromList (global v) ! (fromIntegral (BS.head (instruct v))):(stack v) 
  }

evalSetGlobal :: VM ->  VM 
evalSetGlobal v = vm
  where 
    vm 
      | member (fromIntegral (BS.head (instruct v))) (fromList (global v)) == False = VM{
          instruct = removeFirstInstruction (instruct v), 
          constVM = constVM v,
          global = global v ++ [(fromIntegral (BS.head (instruct v)),(Prelude.head (stack v)))], 
          stack = removeFirst (stack v)
        } 
      | otherwise = VM{
          instruct = removeFirstInstruction (instruct v),
          constVM = constVM v,
          global = (fromIntegral (BS.head (instruct v)), Prelude.head (stack v)):[x | x <- global v, fst x /= fromIntegral (BS.head (instruct v))],
          stack = removeFirst (stack v)
        }


evalJump :: VM -> VM 
evalJump v = VM{
    instruct = removeNInstructions (fromIntegral (toInteger (index (instruct v) 1)) :: Int, (instruct v)),
    constVM = constVM v,
    global = global v,
    stack = stack v
  } 


evalJumpNT :: VM -> VM 
evalJumpNT v = vm 
  where 
    vm 
      | objectType (Prelude.head (stack v)) /= BOOL_OBJ = error ("jump not bool"++ Prelude.concat [inspectObject x ++ " "| x <- stack v]) 
      | boolValue (Prelude.head (stack v)) == True = 
        VM{
            instruct = removeNInstructions(2, instruct v),
            constVM = constVM v,
            global = global v,
            stack = removeFirst (stack v)
          }
      -- Should jump 
      | otherwise = 
        trace ("jumpNT") $ 
        VM{
            instruct = removeNInstructions (fromIntegral (index (instruct v) 1) :: Int, instruct v),
            constVM = constVM v,
            global = global v,
            stack = removeFirst (stack v) 
          }


bangOp :: [Object] -> [Object]
bangOp s= o 
  where 
    o
      | objectType (Prelude.head s) /= BOOL_OBJ = error ("can't have bang prefix on non bool: " ++ (inspectObject (Prelude.head s)))
      | otherwise = BoolObject{objectType = BOOL_OBJ, boolValue = not (boolValue (Prelude.head s))}:(removeFirst s)

minusOp :: [Object] -> [Object]
minusOp s= o 
  where 
    o
      | objectType (Prelude.head s) /= INT_OBJ = error ("can't have minus prefix on non integer: " ++ (inspectObject (Prelude.head s)))
      | otherwise = IntObject{objectType = INT_OBJ, intValue = -1 * intValue (Prelude.head s)}:(removeFirst s)

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
      | objectType o1 == INT_OBJ = trace ("Adding " ++ show(intValue o1 ) ++ " + " ++ (show (intValue o2))) $ IntObject{objectType = INT_OBJ, intValue = intValue o2 + intValue o1}
      | objectType o1 == STRING_OBJ = StringObject{objectType = STRING_OBJ, stringValue = stringValue o2 ++ stringValue o1}
      | otherwise = error ("can't do operation with types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))

subOp :: [Object] -> [Object]
subOp o =evalSubOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalSubOp :: (Object, Object) -> Object 
evalSubOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ = trace ("Subtracting " ++ show(intValue o1 ) ++ " - " ++ (show (intValue o2))) $ IntObject{objectType = INT_OBJ, intValue = intValue o2 - intValue o1}
      | otherwise = error ("can't sub with non int type: " ++ (show (objectType o1))) 

divOp :: [Object] -> [Object]
divOp o = evalDivOp(o!!0, o!!1):(removeFirst (removeFirst o))

evalDivOp :: (Object, Object) -> Object 
evalDivOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ = trace ("Dividing " ++ show(intValue o1 ) ++ " / " ++ (show (intValue o2))) $ IntObject{objectType = INT_OBJ, intValue = intValue o2 `div` intValue o1}
      | otherwise = error ("can't div with non int type: " ++ (show (objectType o1)))  

mulOp :: [Object] -> [Object]
mulOp o =evalMulOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalMulOp :: (Object, Object) -> Object 
evalMulOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ = trace ("Multiplying " ++ show(intValue o2 ) ++ " * " ++ (show (intValue o1))) $ IntObject{objectType = INT_OBJ, intValue = intValue o1 * intValue o2}
      | otherwise = error ("can't mul with non int type: " ++ (show (objectType o1))) 

pushToStack :: VM -> VM 
pushToStack v =  
        trace ("pushing to stack: " ++ show (inspectObject ((constVM v)!! (fromIntegral (BS.head (instruct v)))))) $ 
        VM{
            instruct = removeFirstInstruction (instruct v),
            constVM = constVM v,
            global = global v, 
            stack = (constVM v)!!(fromIntegral (BS.head (instruct v))):(stack v)
          }


removeNInstructions :: (Int, ByteString) -> ByteString 
removeNInstructions (i, b) = bs 
  where 
    bs 
      | i == 0 = b 
      | otherwise = removeNInstructions(i-1, removeFirstInstruction b)

removeFirstInstruction :: ByteString -> ByteString 
removeFirstInstruction b = 
  case BS.length b of 
    0 -> error "can't remove instruction of length 0?"
    1 -> BS.empty :: ByteString 
    _ -> pack(removeFirst(unpack b))


disassembleFunc :: (String, ByteString) -> String 
disassembleFunc (s,b)= str 
  where 
    str 
      | BS.null b = s
      | BS.head b == 0 = disassembleFunc(s ++ " CONST " ++ (show (fromIntegral (BS.head (removeFirstInstruction b)))), removeFirstInstruction(removeFirstInstruction b))
      | BS.head b == 1 = disassembleFunc(s ++ " POP", removeFirstInstruction b)
      | BS.head b == 2 = disassembleFunc(s ++ " ADD", removeFirstInstruction b)
      | BS.head b == 3 = disassembleFunc(s ++ " SUB", removeFirstInstruction b)
      | BS.head b == 4 = disassembleFunc(s ++ " MUL", removeFirstInstruction b)
      | BS.head b == 5 = disassembleFunc(s ++ " DIV", removeFirstInstruction b)
      | BS.head b == 6 = disassembleFunc(s ++ " TRUE", removeFirstInstruction b)
      | BS.head b == 7 = disassembleFunc(s ++ " FALSE", removeFirstInstruction b)
      | BS.head b == 8 = disassembleFunc(s ++ " GT", removeFirstInstruction b)
      | BS.head b == 9 = disassembleFunc(s ++ " LT", removeFirstInstruction b)
      | BS.head b == 10 = disassembleFunc(s ++ " NEQ", removeFirstInstruction b)
      | BS.head b == 11 = disassembleFunc(s ++ " EQ", removeFirstInstruction b)
      | BS.head b == 12 = disassembleFunc(s ++ " MINUS", removeFirstInstruction b)
      | BS.head b == 13 = disassembleFunc(s ++ " BANG", removeFirstInstruction b)
      | BS.head b == 14 = disassembleFunc(s ++ " JUMP", removeFirstInstruction b)
      | BS.head b == 15 = disassembleFunc(s ++ " JUMPNT", removeFirstInstruction b)
      | BS.head b == 16 = disassembleFunc(s ++ " SETGLOBAL", removeFirstInstruction b)
      | BS.head b == 17 = disassembleFunc(s ++ " GETGLOBAL", removeFirstInstruction b)
      | BS.head b == 18 = disassembleFunc(s ++ " ARRAY", removeFirstInstruction b)
      | BS.head b == 19 = disassembleFunc(s ++ " ARRAYEND", removeFirstInstruction b)
      | BS.head b == 20 = disassembleFunc(s ++ " HASH", removeFirstInstruction b)
      | BS.head b == 21 = disassembleFunc(s ++ " HASHEND", removeFirstInstruction b)
      | BS.head b == 22 = disassembleFunc(s ++ " INDEX", removeFirstInstruction b)
      | BS.head b == 23 = disassembleFunc(s ++ " SETINDEX", removeFirstInstruction b)
      | BS.head b == 24 = disassembleFunc(s ++ " OPCALL", removeFirstInstruction b)
      | BS.head b == 25 = disassembleFunc(s ++ " RETURNVALUE", removeFirstInstruction b)
      | BS.head b == 26 = disassembleFunc(s ++ " OPRETURN", removeFirstInstruction b)
      | otherwise = error ("disassemble " ++ (show (BS.head b)))
