module VM where 


import Object
import Code 
import Lexer
import Utils
import Compiler
import CompilerUtils

import Control.Lens
import Debug.Trace
import Data.ByteString as BS
import Data.Map


run :: Compiler -> VM 
run c = 
  runVM(
    VM{
        frames = scopes c,
        frameIndex = 0,
        basePointer = 0,
        constVM = constants c,
        global = [],
        stack = []
      }
  )


data VM = VM{
    frames :: ![ByteString],
    frameIndex :: !Int, 
    basePointer :: !Int, 
    constVM:: ![Object],
    global :: ![(Int, Object)],
    stack :: ![Object] 
  } deriving (Show)

runVM :: VM -> VM
runVM v = vm  
  where 
    vm 
      | BS.null (frames v !!frameIndex v)= v 
      --Constant
      | BS.head (frames v !!frameIndex v)== 0 = runVM(pushToStack(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v, 
          stack = stack v
        }))
      --Pop
      | BS.head (frames v !!frameIndex v)== 1 =
        runVM(VM{
            frames = removeFirstInstruction (frames v, frameIndex v),
            frameIndex = frameIndex v,
            basePointer = basePointer v,
            constVM = constVM v, 
            global = global v, 
            stack = removeFirst (stack v)
          })
      --ADD
      | BS.head (frames v !!frameIndex v)== 2 = runVM(VM{
            frames = removeFirstInstruction (frames v, frameIndex v),
            frameIndex = frameIndex v,
            basePointer = basePointer v,
            constVM = constVM v, 
            global = global v,
            stack =addOp (stack v)
          })
      --Sub
      | BS.head (frames v !!frameIndex v)== 3 = 
        runVM(VM{
            frames = removeFirstInstruction (frames v, frameIndex v),
            frameIndex = frameIndex v,
            basePointer = basePointer v,
            constVM = constVM v, 
            global = global v,
            stack =subOp (stack v)
          })
      --Mul
      | BS.head (frames v !!frameIndex v)== 4 = 
        runVM(VM{
            frames = removeFirstInstruction (frames v, frameIndex v),
            frameIndex = frameIndex v,
            basePointer = basePointer v,
            constVM  = constVM v, 
            global = global v,
            stack =mulOp (stack v)
          })
      --Div
      | BS.head (frames v !!frameIndex v)== 5 = 
        runVM(VM{
            frames = removeFirstInstruction (frames v, frameIndex v),
            frameIndex = frameIndex v,
            basePointer = basePointer v,
            constVM = constVM v, 
            global = global v,
            stack =divOp (stack v)
          })
      --True
      | BS.head (frames v !!frameIndex v)== 6 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = True}:(stack v)
        }
      )
      --False 
      | BS.head (frames v !!frameIndex v)== 7 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = False}:(stack v)
        })
      --GT 
      | BS.head (frames v !!frameIndex v)== 8 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = gtOp (stack v)
        }
      ) 
      --NEQ 
      | BS.head (frames v !!frameIndex v)== 10 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v, 
          stack = neqOp (stack v)
        }) 
      --EQ
      | BS.head (frames v !!frameIndex v)== 11 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v, 
          stack = eqOp (stack v)
        }) 
      --MINUS
      | BS.head (frames v !!frameIndex v)== 12 = runVM(VM{
          frames = removeFirstInstruction (frames v,frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v, 
          stack = minusOp (stack v)
        }) 
      --BANG
      | BS.head (frames v !!frameIndex v)== 13 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v, 
          stack = bangOp (stack v)
        }) 
      -- JUMP
      | BS.head (frames v !! frameIndex v)== 14 = runVM(evalJump(v))
      -- JUMPNT
      | BS.head (frames v !! frameIndex v)== 15 = runVM(evalJumpNT(v))
      -- SETGLOBAL
      | BS.head (frames v !! frameIndex v) == 16 = runVM(evalSetGlobal(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))
      -- GETGLOBAL
      | BS.head (frames v !! frameIndex v)== 17 = runVM(evalGetGlobal(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))  
      -- ARRAY
      | BS.head (frames v !! frameIndex v)== 18 = 
        runVM(addEleToArray (VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = stack v
          }, ArrayObject{objectType = ARRAY_OBJ, arrValue = []}))  
      -- ARRAYEND
      | BS.head (frames v !! frameIndex v)== 19 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v, 
          stack = NullObject{objectType = NULL_OBJ}:stack v
        })  
      -- HASH 
      | BS.head (frames v !! frameIndex v)== 20 = 
        runVM(addEleToMap (VM{
          frames = removeFirstInstruction (frames v,frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        }, MapObject{objectType = MAP_OBJ, mapValue = []})) 
      -- HASHEND
      | BS.head (frames v !! frameIndex v)== 21 = runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v, 
          stack = NullObject{objectType = NULL_OBJ}:stack v
        })  
      -- INDEX
      | BS.head (frames v !! frameIndex v)== 22 = runVM(addIndexToStack(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v,
          stack = stack v
        }))
      -- SETINDEX
      | BS.head (frames v !! frameIndex v)== 23 = runVM(VM{
          frames = removeFirstInstruction(frames v, frameIndex v), 
          frameIndex = frameIndex v, 
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v,
          stack = evalAssignIndex(
            removeFirst(removeFirst(stack v)),
            stack v!!0,
            stack v!!1
          ):[] 
        })
      -- OPCALL 
      | BS.head (frames v !! frameIndex v)== 24 = runVM(evalCall(VM{
          frames = removeFirstInstruction(frames v, frameIndex v), 
          frameIndex = frameIndex v, 
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))
      -- OPRETURNVALUE 
      | BS.head (frames v !! frameIndex v)== 25 = 
        trace ("OPRETURNVALUE, Top of stack: " ++ inspectObject (Prelude.head (stack v)))
         VM{
          frames = frames v, 
          frameIndex = frameIndex v, 
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v,
          stack = [Prelude.head (stack v)]
        }
      -- OPRETURN 
      | BS.head (frames v !! frameIndex v)== 26 = VM{
          frames = removeFirstInstruction(frames v,  frameIndex v), 
          frameIndex = frameIndex v, 
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v,
          stack = []
        } 
      -- SETLOCAL
      | BS.head (frames v !! frameIndex v)== 27 = runVM(setLocal(VM{
          frames = removeFirstInstruction(frames v, frameIndex v), 
          frameIndex = frameIndex v, 
          basePointer = basePointer v,
          constVM = constVM v, 
          global = global v,
          stack = stack v 
        })) 
      -- GETLOCAL
      | BS.head (frames v !!frameIndex v)== 28 = runVM(getLocal(VM{
        frames = removeFirstInstruction(frames v, frameIndex v), 
        frameIndex = frameIndex v, 
        basePointer = basePointer v,
        constVM = constVM v, 
        global = global v,
        stack = stack v 
        }))
      | otherwise = error "run" 

getLocal :: VM -> VM 
getLocal v = 
  trace ("getLocal, Stack: " ++ concStack(stack v!!(fromIntegral (BS.head (frames v !! frameIndex v))) :stack v))
  VM{
    frames = removeFirstInstruction (frames v, frameIndex v),
    frameIndex = frameIndex v,
    basePointer = basePointer v,
    constVM = constVM v,
    global = global v,
    stack = stack v!!(fromIntegral (BS.head (frames v !! frameIndex v))) :stack v
  }


concStack :: [Object] -> String 
concStack o = Prelude.concat [inspectObject x ++ " " | x <- o]

setLocal :: VM -> VM 
setLocal v = 
  trace ("setLocal, Stack: " ++ concStack(removeFirst(stack v & element (fromIntegral(BS.head (frames v !! frameIndex v)) + 1) .~ stack v!!0)))
  VM{
    frames = removeFirstInstruction (frames v, frameIndex v),
    frameIndex = frameIndex v,
    basePointer = basePointer v,
    constVM = constVM v,
    global = global v,
    stack = removeFirst(stack v & element (fromIntegral(BS.head (frames v !! frameIndex v)) + 1) .~ stack v!!0)
  }

errorStack :: VM -> VM 
errorStack v = error ("FirstInstruct: " ++ (show (BS.head (frames v !! frameIndex v))) ++  " Stack: " ++ Prelude.concat [inspectObject x ++  " " | x <- stack v])

evalParams :: VM -> VM 
evalParams v = vm 
  where 
    vm 
      | 1 + numArgs (Prelude.head (stack v)) /= Prelude.length (stack v) = error (show (numArgs (Prelude.head (stack v))) ++ " " ++ show (stack v)) 
      | otherwise = 
        trace ("evalParams, BP: " ++ show(basePointer v + numLocals (Prelude.head (stack v)) + numArgs (Prelude.head (stack v))))
        trace ("    Stack: " ++ concStack(stack v ++ [NullObject{objectType = NULL_OBJ} | x <- [1 .. (numArgs(Prelude.head (stack v)) + numLocals (Prelude.head (stack v)))]]))
        trace ("    NumLocals: " ++ show(numLocals (Prelude.head (stack v))))
        trace ("    NumArgs: " ++ show(numArgs(Prelude.head (stack v))))
        VM{ 
          frames = removeFirstInstruction(frames v,  frameIndex v) ++ [funcValue (Prelude.head (stack v))], 
          frameIndex = frameIndex v + 1, 
          basePointer = basePointer v + numLocals (Prelude.head (stack v)) + numArgs (Prelude.head (stack v)),
          constVM = constVM v, 
          global = global v,
          stack = removeFirst(stack v)++ [NullObject{objectType = NULL_OBJ} | x <- [1 .. (numLocals (Prelude.head (stack v)))]] 
        } 


evalCall :: VM -> VM 
evalCall v = evalReturn(runVM(evalParams(v)))

evalReturn :: VM -> VM 
evalReturn v = vm 
  where 
    vm 
      | otherwise =
        VM{
          frames = pop (frames v),
          frameIndex = frameIndex v - 1,
          basePointer = 0, -- SHOULD POINT TO START OF NEXT FRAME 
          constVM = constVM v, 
          global = global v, 
          stack = stack v 
        }



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
          frames = frames v, 
          frameIndex = frameIndex v,
          basePointer = basePointer v,
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
        VM{
          frames = frames v, 
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = mp:(removeFirst (stack v))
        }
      | otherwise = 
        addEleToMap(VM{
          frames = frames v,
          frameIndex = frameIndex v,
          basePointer = basePointer v,
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
        VM{
          frames = frames v,
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v,
          stack = arr:(removeFirst (stack v))
        }
      | otherwise = 
        addEleToArray(VM{
          frames = frames v,
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v, 
          stack = removeFirst(stack v) 
        }, ArrayObject{objectType = ARRAY_OBJ, arrValue = arrValue arr ++ [Prelude.head (stack v)]})

evalGetGlobal :: VM ->  VM 
evalGetGlobal v = 
  VM{
    frames = removeFirstInstruction(frames v, frameIndex v),
    frameIndex = frameIndex v,
    basePointer = basePointer v,
    constVM = constVM v,
    global = global v, 
    stack = fromList (global v) ! (fromIntegral (BS.head (frames v !!frameIndex v))):(stack v) 
  }

evalSetGlobal :: VM ->  VM 
evalSetGlobal v = vm
  where 
    vm 
      | member (fromIntegral (BS.head (frames v !! frameIndex v))) (fromList (global v)) == False = VM{
          frames = removeFirstInstruction (frames v, frameIndex v), 
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = global v ++ [(fromIntegral (BS.head (frames v!! frameIndex v)),(Prelude.head (stack v)))], 
          stack = removeFirst (stack v)
        } 
      | otherwise = VM{
          frames = removeFirstInstruction (frames v, frameIndex v), 
          frameIndex = frameIndex v,
          basePointer = basePointer v,
          constVM = constVM v,
          global = (fromIntegral (BS.head (frames v !!frameIndex v)), Prelude.head (stack v)):[x | x <- global v, fst x /= fromIntegral (BS.head (frames v !! frameIndex v))],
          stack = removeFirst (stack v)
        }


evalJump :: VM -> VM 
evalJump v = VM{
    frames = removeNInstructions (fromIntegral (toInteger (BS.index (frames v !!frameIndex v) 1)) :: Int, frames v, frameIndex v),
    frameIndex = frameIndex v,
    basePointer = basePointer v,
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
            frames = removeNInstructions(2, frames v, frameIndex v),
            frameIndex = frameIndex v,
            basePointer = basePointer v,
            constVM = constVM v,
            global = global v,
            stack = removeFirst (stack v)
          }
      -- Should jump 
      | otherwise = 
        VM{
            frames = removeNInstructions (fromIntegral (BS.index (frames v !! frameIndex v) 1) :: Int, frames v, frameIndex v),
            frameIndex = frameIndex v,
            basePointer = basePointer v,
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
      | objectType o1 == INT_OBJ =  IntObject{objectType = INT_OBJ, intValue = intValue o2 + intValue o1}
      | objectType o1 == STRING_OBJ = StringObject{objectType = STRING_OBJ, stringValue = stringValue o2 ++ stringValue o1}
      | otherwise = error ("can't do operation with types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))

subOp :: [Object] -> [Object]
subOp o =evalSubOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalSubOp :: (Object, Object) -> Object 
evalSubOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ = IntObject{objectType = INT_OBJ, intValue = intValue o2 - intValue o1}
      | otherwise = error ("can't sub with non int type: " ++ (show (objectType o1))) 

divOp :: [Object] -> [Object]
divOp o = evalDivOp(o!!0, o!!1):(removeFirst (removeFirst o))

evalDivOp :: (Object, Object) -> Object 
evalDivOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ =  IntObject{objectType = INT_OBJ, intValue = intValue o2 `div` intValue o1}
      | otherwise = error ("can't div with non int type: " ++ (show (objectType o1)))  

mulOp :: [Object] -> [Object]
mulOp o =evalMulOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

evalMulOp :: (Object, Object) -> Object 
evalMulOp (o1, o2) = o 
  where 
    o
      | objectType o1 /= objectType o2 = error ("can't do operation with different types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))
      | objectType o1 == INT_OBJ =  IntObject{objectType = INT_OBJ, intValue = intValue o1 * intValue o2}
      | otherwise = error ("can't mul with non int type: " ++ (show (objectType o1))) 

pushToStack :: VM -> VM 
pushToStack v =  
        VM{
            frames = removeFirstInstruction (frames v, frameIndex v),
            frameIndex =frameIndex v,
            basePointer = basePointer v,
            constVM = constVM v,
            global = global v, 
            stack = (constVM v)!!(fromIntegral (BS.head (frames v !!frameIndex v))):(stack v)
          }


parseStack ::  VM -> String 
parseStack v = ("Stack: " ++ Prelude.concat [inspectObject x | x <- (stack v)] ++ " " ++ "Globals: " ++ Prelude.concat [inspectGlobal x | x <- (global v)])

inspectGlobal :: (Int, Object) -> String 
inspectGlobal (i, o) = (show i) ++ " = " ++ inspectObject o ++ " "

getGlobal :: (Int, VM) -> Object 
getGlobal (i, v) = (fromList (global v)) ! i 
