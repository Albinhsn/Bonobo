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


run :: ByteString -> [String] 
-- run :: ByteString -> [] 
run b = 
  outputs(runVM(
    VM{
        frames = [(0, b)],
        bpOffset = 0, 
        global = [],
        stack = [],
        outputs =[]
      }
  ))

runTest :: ByteString -> VM 
runTest b = runVM(VM{
        frames = [(0, b)],
        bpOffset = 0, 
        global = [],
        stack = [],
        outputs =[]
      }
    )

data VM = VM{
    frames :: ![(Int,ByteString)],
    bpOffset :: !Int, 
    global :: ![(Int, Object)],
    stack :: ![Object],
    outputs :: [String]
  } deriving (Show)

getFirstInstruction :: (Int, ByteString) -> Int 
getFirstInstruction  f = i 
  where 
    i
      | BS.null (snd (f)) = -1 
      | otherwise = fromIntegral (BS.head (snd f))  

runVM :: VM -> VM
runVM v = 
  case getFirstInstruction(Prelude.last (frames v)) of 
    -- NULL 
    -1 -> 
      v 
    --Constant
    0 -> 
      runVM(pushToStack(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v + 1,
          global = global v, 
          stack = stack v,
          outputs = outputs v 
        }))
    --Pop
    1 -> runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v - 1,
          global = global v, 
          stack = removeFirst (stack v),
          outputs = outputs v
          })
    --ADD
    2 ->  
      runVM(VM{
              frames = removeFirstInstruction (frames v),
              bpOffset = bpOffset v- 1,
              global = global v,
              stack =evalAddOp(Prelude.head (stack v), stack v!!1):removeFirstN(2, stack v),
              outputs = outputs v
            })
    --Sub
    3 ->  
        runVM(VM{
              frames = removeFirstInstruction (frames v),
              bpOffset = bpOffset v- 1,
              global = global v,
              stack =evalSubOp(Prelude.head (stack v), stack v!!1):(removeFirstN(2, stack v)) ,
              outputs = outputs v
            })
    --Mul
    4 -> 
      runVM(VM{
              frames = removeFirstInstruction (frames v),
              bpOffset = bpOffset v- 1,
              global = global v,
              stack =evalMulOp(Prelude.head (stack v), stack v!!1):(removeFirstN(2, stack v)) ,
              outputs = outputs v
            })
    --Div
    5 -> 
      runVM(VM{
              frames = removeFirstInstruction (frames v),
              bpOffset = bpOffset v- 1,
              global = global v,
              stack =evalDivOp(Prelude.head (stack v), stack v!!1):(removeFirstN(2, stack v)),
              outputs = outputs v
            })
    --True
    6 -> runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v+ 1,
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = True}:(stack v),
          outputs = outputs v
        }
      )
    --False 
    7 -> runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v+ 1,
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = False}:(stack v),
          outputs = outputs v
        })
    --GT 
    8 -> runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v- 1,
          global = global v,
          stack = evalGTOp(Prelude.head (stack v), stack v !!1):(removeFirstN(2, stack v)),
          outputs = outputs v
        }
      ) 
    --NEQ 
    10 -> 
      runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v- 1,
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = Prelude.head (stack v) /= stack v !! 1}:(removeFirstN(2, stack v)),
          outputs = outputs v
        }) 
    --EQ
    11 -> runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v- 1,
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = Prelude.head (stack v) == stack v !!1}:(removeFirstN(2, stack v)),
          outputs = outputs v
        }) 
    --MINUS
    12 -> runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v,
          global = global v, 
          stack = minusOp (Prelude.head (stack v)):removeFirst(stack v),
          outputs = outputs v
        }) 
    --BANG
    13 -> runVM(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v,
          global = global v, 
          stack = bangOp (Prelude.head (stack v)):removeFirst(stack v),
          outputs = outputs v
        }) 
    -- JUMP
    -- REWORK THIS 
    14 -> runVM(evalJump(v))
    -- JUMPNT
    15 -> runVM(evalJumpNT(v))
    -- SETGLOBAL
    16 -> 
      runVM(evalSetGlobal(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v,
          outputs = outputs v
        }))
    -- GETGLOBAL
    17 -> 
      runVM(evalGetGlobal(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v,
          outputs = outputs v
        }))  
      -- ARRAY
    18 -> runVM(addArray (VM{
            frames = removeFirstInstruction (frames v),
            bpOffset = bpOffset v,
            global = global v,
            stack = stack v,
            outputs = outputs v
          }))  
    20 -> runVM(addMap (VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v,
          outputs = outputs v
        })) 
    -- INDEX
    22 -> runVM(addIndexToStack(VM{
          frames = removeFirstInstruction (frames v),
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v,
          outputs = outputs v
        }))
    -- SETINDEX
    23 -> runVM(VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v - 2,
          global = global v,
          stack = evalAssignIndex(
            removeFirst(removeFirst(stack v)),
            stack v!!0,
            stack v!!1
          ):[],
          outputs = outputs v
        })
    -- OPCALL 
    24 ->   
        runVM(evalCall(VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v,
          outputs = outputs v
        }))
    -- OPRETURNVALUE 
    25 -> 
         VM{
          frames = frames v, 
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v,
          outputs = outputs v
        }
    -- OPRETURN 
    26 -> VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v,
          global = global v,
          stack = NullObject{objectType = NULL_OBJ}:stack v, 
          outputs = outputs v
        } 
    -- SETLOCAL
    27 -> runVM(setLocal(VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v, 
          outputs = outputs v
        })) 
    -- GETLOCAL
    28 -> runVM(getLocal(VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v, 
          outputs = outputs v
        }))
    29 -> runVM(runPrebuilt(VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v, 
          outputs = outputs v
        }))
    30 -> 
      runVM(runForEval(getStart(VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v, 
          outputs = outputs v
      })))
    _ -> error "run"


getStart :: VM -> VM 
getStart v =   
  runVM(VM{
    -- frames = Utils.append (changeBP(1, frames v)) (0,forStart(Prelude.head (stack v))), 
    frames = Utils.append (changeBP(1, frames v)) (0,forStart(Prelude.head (stack v))), 
    bpOffset = bpOffset v - 1, 
    global = global v, 
    stack = NullObject{objectType = NULL_OBJ}:stack v,
    outputs = outputs v
  })



runForEval :: VM -> VM 
runForEval v = b 
  where 
    b 
      | boolValue (Prelude.head (stack (runVM(VM{
          frames = Utils.append (frames v) (0, forCon (stack v!!1)),
          bpOffset = bpOffset v,
          global = global v,
          stack = stack v,
          outputs = outputs v
        })))) == False = 
          VM{
            frames = changeBP(-1, pop(frames v)),
            bpOffset = bpOffset v,
            global = global v, 
            stack = removeFirstN(2, stack v),
            outputs = outputs v
          }  
      | otherwise =  
        -- trace("forEval: " ++ concStack ([NullObject{objectType = NULL_OBJ} | x <- [1..(forLocals (stack v !!1))]]++ stack v))
        runForEval(runInc(forLocals (stack v !! 1), popLastFrame(runVM(VM{
          frames = Utils.append  (changeBP(forLocals (stack v !! 1), frames v)) (0, forBod(stack v !!1)),
          bpOffset = bpOffset v,
          global = global v,
          stack =[NullObject{objectType = NULL_OBJ} | x <- [1..(forLocals (stack v !!1))]]++ stack v,
          outputs = outputs v
        }))))

runInc :: (Int, VM)-> VM 
runInc (i,v) = 
  -- trace("runInc:    ")
  -- trace("     stack: " ++ concStack (stack v))
  -- trace("     bp" ++ show (changeBP(-i, frames v)))
  popLastFrame(runVM(VM{
    frames = Utils.append (changeBP(-i, frames v)) (0, forInc (stack v !!(i + 1))),
    bpOffset = bpOffset v,
    global = global v,
    stack = removeFirstN(i, stack v),
    outputs = outputs v
  }))

popLastFrame :: VM -> VM 
popLastFrame v = 
  VM{
    frames = pop (frames v),
    bpOffset = bpOffset v, 
    global = global v,
    stack = stack v, 
    outputs = outputs v
  }


runPrebuilt :: VM -> VM 
runPrebuilt v = vm 
  where 
    vm 
    --len
      | getFirstInstruction(Prelude.last (frames v)) == 0 = VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v,
          global = global v,
          stack = IntObject{objectType = INT_OBJ, intValue = getObjectLen(Prelude.head (stack v))}:removeFirst(stack v),
          outputs = outputs v

        } 
      --print
      | getFirstInstruction(Prelude.last (frames v)) == 1 = 
        VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v - 1,
          global = global v,
          stack = removeFirst(stack v),
          outputs = outputs v ++ [inspectObject(Prelude.head (stack v))]
        } 
      -- append
      | getFirstInstruction(Prelude.last (frames v)) == 2 = VM{
          frames = removeFirstInstruction(frames v), 
          bpOffset = bpOffset v - 1,
          global = global v,
          stack = ArrayObject{objectType = ARRAY_OBJ, arrValue = arrValue (stack v !!1)++ [Prelude.head (stack v)]}:removeFirstN(2, stack v),
          outputs = outputs v
        } 
      | otherwise = error ("not implemented prebuilt" ++ show (getFirstInstruction(Prelude.last (frames v))))

getLocal :: VM -> VM 
getLocal v =  
  VM{
    frames = removeFirstInstruction(removeFirstInstruction (frames v)),
    bpOffset = bpOffset v+ 1,
    global = global v,
    stack = stack v!!(getLocalEleIdx v) :stack v,
    outputs = outputs v
  }

concStack :: [Object] -> String 
concStack o = s 
  where 
    s
     | Prelude.null o = ""
     | otherwise =  Prelude.concat [inspectObject x ++ " " | x <- o]

setLocal :: VM -> VM 
setLocal v = 
  -- trace("setLocal: ")
  -- trace("       getLocalEleIdx: " ++ show(getLocalEleIdx v))
  -- trace("       bpIndex" ++ show([fst x | x <- frames v]))
  -- trace("       bpOffset: " ++ show(bpOffset v))
  -- trace("       getNInstruction: " ++ show(getNInstruction(1, frames v)))
  -- trace("       getBPIdx: " ++ show(getBasePointerIdx(getNInstruction(0, frames v ) -1, v)))
  -- trace("       stack prior: " ++ concStack(stack v))
  -- trace("       stack after: " ++ concStack(removeFirst(stack v & element (getLocalEleIdx v) .~ stack v!!0)))
  -- trace("       next 2 instruct: " ++ show(fromIntegral (BS.index (snd (Prelude.last (frames v))) 0 )) ++" "  ++ show (fromIntegral (BS.index (snd (Prelude.last (frames v))) 1))) 
  VM{
    frames = removeFirstInstruction(removeFirstInstruction (frames v)),
    bpOffset = bpOffset v- 1,
    global = global v,
    stack = removeFirst(stack v & element (getLocalEleIdx v) .~ stack v!!0),
    outputs = outputs v
  }

getNInstruction :: (Int, [(Int, ByteString)]) -> Int 
getNInstruction (i, f) = 
  fromIntegral(BS.index(snd(Prelude.last f)) i)

getLocalEleIdx :: VM -> Int 
getLocalEleIdx v = 
  getBasePointerIdx(getNInstruction(0,frames v)-1, v) - 1 + 
  bpOffset v - 
  getNInstruction(1, frames v) 


getBasePointerIdx :: (Int,VM) -> Int 
getBasePointerIdx (i,v) = fst(frames v !!i)

evalParams :: VM -> VM 
evalParams v = vm 
  where 
    vm 
      | otherwise = 
        VM{ 
          frames = removeFirstInstruction(changeBP(numLocals (Prelude.head (stack v)), frames v)) ++ [(0,funcValue (Prelude.head (stack v)))], 
          bpOffset = 0,
          global = global v,
          stack =[NullObject{objectType = NULL_OBJ} | x <- [1 .. (numLocals (Prelude.head (stack v)) - numArgs(Prelude.head (stack v)))]] ++ removeFirst(stack v),
          outputs = outputs v
        } 

changeBP :: (Int, [(Int, ByteString)]) -> [(Int, ByteString)] 
changeBP (i, f) = [(fst x + i, snd x) | x <- f] 

evalCall :: VM -> VM 
evalCall v = vm 
  where 
    vm 
      | otherwise = evalReturn(runVM(evalParams(v)))

concGlobal :: [(Int, Object)] -> String 
concGlobal o = concStack([snd x | x <- o])


evalReturn :: VM -> VM 
evalReturn v = vm 
  where 
    vm 
      | objectType (Prelude.head (stack v)) == NULL_OBJ = 
        VM{
          frames =  changeBP(-1 * getBasePointerIdx(Prelude.length (frames v) -2, v), pop (frames v)),
          bpOffset = 0,
          global = global v, 
          stack = removeFirstN( 1 + getBasePointerIdx(Prelude.length (frames v) -2, v), stack v),
          outputs = outputs v
        }
      | otherwise =
        -- trace("evalReturn: ")
        -- trace("       stack prior: " ++ concStack(stack v))
        -- trace("       stack after: " ++ concStack(stack v !!0:removeFirstN(getBasePointerIdx(Prelude.length (frames v) -2, v) + 1, stack v)))
        -- trace("       globals: " ++ concGlobal(global v))
        VM{
          frames =  changeBP(-1 * getBasePointerIdx(Prelude.length (frames v) -2, v), pop (frames v)),
          bpOffset = 1,
          global = global v, 
          stack =stack v !!0:removeFirstN(getBasePointerIdx(Prelude.length (frames v) -2, v) + 1, stack v), 
          outputs = outputs v
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
addIndexToStack v = 
      VM{
          frames = frames v, 
          bpOffset = bpOffset v - 1,
          global = global v, 
          stack = (evalIndex (stack v!!0, stack v!!1)):(removeFirst(removeFirst (stack v))),
          outputs = outputs v
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
      | otherwise = error ("key doesn't exist: key: " ++ inspectObject key ++ " mp: " ++ inspectObject mp)

evalArrayIndex :: (Object, Object) -> Object 
evalArrayIndex (idx, arr) = val
  where 
    val 
      | objectType idx == INT_OBJ && isWithinBounds(intValue idx, Prelude.length (arrValue arr)) = arrValue arr!!intValue idx 
      | otherwise = error "can't access array with non int key"

addMap :: VM -> VM 
addMap v = VM{
    frames = removeFirstInstruction(frames v), 
    bpOffset = bpOffset v - fromIntegral(BS.head (snd(Prelude.last (frames v)))) + 1,
    global = global v,
    stack = MapObject{objectType = MAP_OBJ, mapValue = generateMap(getFirstN(fromIntegral(BS.head(snd(Prelude.last (frames v)))),stack v, []))}:removeFirstN(fromIntegral (BS.head (snd(Prelude.last (frames v)))), stack v), 
    outputs = outputs v
  } 
generateMap :: [Object] -> [(Object, Object)]
generateMap [] = []
generateMap (k:v:t) = (k, v): generateMap t

addArray :: VM -> VM 
addArray v = VM{
    frames = removeFirstInstruction(frames v),
    bpOffset = bpOffset v - fromIntegral(BS.head (snd(Prelude.last (frames v)))) + 1,
    global = global v, 
    stack = ArrayObject{objectType = ARRAY_OBJ, arrValue = getFirstN(fromIntegral(BS.head (snd(Prelude.last (frames v)))), stack v, [])}:removeFirstN(fromIntegral(BS.head (snd(Prelude.last (frames v)))),stack v),
    outputs = outputs v
  } 

evalGetGlobal :: VM ->  VM 
evalGetGlobal v = 
  VM{
    frames = removeFirstInstruction(frames v),
    bpOffset = bpOffset v + 1,
    global = global v, 
    stack = fromList (global v) ! (getFirstInstruction (Prelude.last (frames v))):(stack v), 
    outputs = outputs v
  }

evalSetGlobal :: VM ->  VM 
evalSetGlobal v = vm
  where 
    vm 
      | Prelude.null (stack v) = error "can't set null"
      | member (getFirstInstruction (Prelude.last (frames v))) (fromList (global v)) == False = VM{
          frames = removeFirstInstruction (frames v), 
          bpOffset = bpOffset v - 1,
          global = global v ++ [(getFirstInstruction (Prelude.last (frames v)),(Prelude.head (stack v)))], 
          stack = removeFirst (stack v),
          outputs = outputs v
        } 
      | otherwise = 
        VM{
          frames = removeFirstInstruction (frames v), 
          bpOffset = bpOffset v - 1,
          global = (getFirstInstruction(Prelude.last (frames v)), Prelude.head (stack v)):[x | x <- global v, fst x /= getFirstInstruction(Prelude.last (frames v))],
          stack = removeFirst (stack v),
          outputs = outputs v
        }


evalJump :: VM -> VM 
evalJump v = VM{
    frames = removeNInstructions (fromIntegral (toInteger (BS.index (snd (Prelude.last (frames v))) 1)) :: Int, frames v),
    bpOffset = bpOffset v,
    global = global v,
    stack = stack v,
    outputs = outputs v
  } 


evalJumpNT :: VM -> VM 
evalJumpNT v = vm 
  where 
    vm 
      | objectType (Prelude.head (stack v)) /= BOOL_OBJ = error ("jump not bool"++ Prelude.concat [inspectObject x ++ " "| x <- stack v]) 
      | boolValue (Prelude.head (stack v)) == True = 
        VM{
            frames = removeNInstructions(2, frames v),
            bpOffset = bpOffset v - 1,
            global = global v,
            stack = removeFirst (stack v),
            outputs = outputs v
          }
      -- Should jump 
      | otherwise = 
        VM{
            frames = removeNInstructions (fromIntegral (BS.index (snd (Prelude.last(frames v))) 1), frames v),
            bpOffset = bpOffset v - 1,
            global = global v,
            stack = removeFirst (stack v),
            outputs = outputs v
          }


bangOp :: Object -> Object
bangOp s= o 
  where 
    o
      | objectType s /= BOOL_OBJ = error ("can't have bang prefix on non bool: " ++ (inspectObject s))
      | otherwise = BoolObject{objectType = BOOL_OBJ, boolValue = not (boolValue s)}

minusOp :: Object -> Object
minusOp s = o 
  where 
    o
      | objectType s /= INT_OBJ = error ("can't have minus prefix on non integer: " ++ (inspectObject s))
      | otherwise = IntObject{objectType = INT_OBJ, intValue = -1 * intValue s}

evalGTOp :: (Object, Object) -> Object 
evalGTOp (o1, o2) = o
  where 
    o
      | objectType o1 /= INT_OBJ || objectType o2 /= INT_OBJ = error ("can't do greater then operation on non ints: " ++ inspectObject(o1)++ " " ++ inspectObject(o2))
      | otherwise = 
        -- trace("o1: " ++ show(intValue o1) ++ " o2:" ++ show(intValue o2))
        BoolObject{objectType = BOOL_OBJ, boolValue = intValue o1 < intValue o2}

evalAddOp:: (Object, Object) -> Object 
evalAddOp(o1, o2) =   
  case (objectType o1, objectType o2) of 
    (INT_OBJ, INT_OBJ) ->IntObject{objectType = INT_OBJ, intValue = intValue o2 + intValue o1} 
    (STRING_OBJ, STRING_OBJ ) -> StringObject{objectType = STRING_OBJ, stringValue = stringValue o2 ++ stringValue o1}
    _ -> error ("can't do operation with types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))

evalSubOp :: (Object, Object) -> Object 
evalSubOp (o1, o2) = o 
  where 
    o
      | objectType o1 == INT_OBJ && objectType o2 == INT_OBJ = 
        -- trace("evalSubOp: ")
        -- trace("     newVal: " ++ show(intValue o2 - intValue o1))
        IntObject{objectType = INT_OBJ, intValue = intValue o2 - intValue o1}
      | otherwise = error ("can't sub with non int type: " ++ (show (objectType o1))) 

evalDivOp :: (Object, Object) -> Object 
evalDivOp (o1, o2) = o 
  where 
    o
      | objectType o1 == INT_OBJ && objectType o2 == INT_OBJ =  IntObject{objectType = INT_OBJ, intValue = intValue o2 `div` intValue o1}
      | otherwise = error ("can't div with non int type: " ++ (show (objectType o1)))  

evalMulOp :: (Object, Object) -> Object 
evalMulOp (o1, o2) = o 
  where 
    o
      | objectType o1 == INT_OBJ && objectType o2 == INT_OBJ =  IntObject{objectType = INT_OBJ, intValue = intValue o1 * intValue o2}
      | otherwise = error ("can't mul with non int type: " ++ (show (objectType o1)) ++ " and " ++ (show (objectType o2))) 

pushToStack :: VM -> VM 
pushToStack v = vm
  where 
    vm 
      | getFirstInstruction(Prelude.last (frames v)) == 1 = 
        VM{
          frames = removeNInstructions(2 + (fromIntegral(BS.index (snd(Prelude.last (frames v))) 1)), frames v),
          bpOffset = bpOffset v, 
          global = global v, 
          stack = parseInt(bsToInt(
          getFirstNInstructions(
              fromIntegral(BS.index (snd(Prelude.last (frames v))) 1),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(2, frames v))!!(Prelude.length (frames v) - 1))))))
          :stack v,
          outputs = outputs v
        } 
      | getFirstInstruction(Prelude.last (frames v)) == 0 = VM{
          frames = removeNInstructions(2 + (fromIntegral(BS.index (snd(Prelude.last (frames v))) 1)), frames v),
          bpOffset = bpOffset v, 
          global = global v, 
          stack = VM.parseString(convertBytesToString(
          getFirstNInstructions(
              fromIntegral(BS.index (snd(Prelude.last (frames v))) 1),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(2, frames v)) !!(Prelude.length (frames v ) - 1))))))
          :stack v,
          outputs = outputs v
        } 
      | getFirstInstruction (Prelude.last (frames v)) == 2 = 
        VM{
          frames = removeNInstructions(
            -- for + start + stop + inc + body 
            6 +  
            -- start
            fromIntegral(BS.index (snd(Prelude.last (frames v))) 1) + 
            -- stop
            fromIntegral(BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))) +  
            -- inc 
            fromIntegral (BS.index (snd(Prelude.last (frames v))) ((3 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))  + fromIntegral (BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))))) + 
            -- body
            fromIntegral (BS.index (snd(Prelude.last (frames v))) (4 + fromIntegral(BS.index (snd(Prelude.last (frames v))) 1) + fromIntegral(BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last(frames v))) 1)))) + (fromIntegral (BS.index (snd(Prelude.last (frames v))) ((3 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))  + fromIntegral (BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))))))))
          , frames v),
          bpOffset = bpOffset v,
          global = global v, 
          stack = ForObject{
            objectType = FOR_OBJ, 
            forStart = getFirstNInstructions(
              fromIntegral(BS.index (snd (Prelude.last (frames v))) 1),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(2, frames v)) !! (Prelude.length (frames v) - 1)))
            ),
            forCon = getFirstNInstructions(
              fromIntegral(BS.index (snd(Prelude.last (frames v))) (2 + fromIntegral(BS.index (snd (Prelude.last (frames v))) 1))),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(3 + fromIntegral(BS.index (snd (Prelude.last (frames v))) 1), frames v)) !!(Prelude.length (frames v) - 1)))
            ), 
            forInc = getFirstNInstructions(
              fromIntegral (BS.index (snd(Prelude.last (frames v))) ((3 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))  + fromIntegral (BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))))),
              BS.empty :: ByteString, 
              (0,snd((removeNInstructions(4 + fromIntegral(BS.index (snd (Prelude.last (frames v))) 1) + fromIntegral(BS.index (snd(Prelude.last (frames v))) (2 + fromIntegral(BS.index (snd (Prelude.last (frames v))) 1))), frames v)) !! (Prelude.length (frames v) - 1)))
            ), 
            forBod = getFirstNInstructions(
            fromIntegral (BS.index (snd(Prelude.last (frames v))) (4 + fromIntegral(BS.index (snd(Prelude.last (frames v))) 1) + fromIntegral(BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))) + (fromIntegral (BS.index (snd(Prelude.last (frames v))) ((3 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))  + fromIntegral (BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1))))))))),
            BS.empty :: ByteString, 
              (0,snd((removeNInstructions(
                5 + 
                fromIntegral(BS.index (snd (Prelude.last (frames v))) 1) + 
                fromIntegral(BS.index (snd(Prelude.last (frames v))) (2 + fromIntegral(BS.index (snd (Prelude.last (frames v))) 1))) + 
                fromIntegral (BS.index (snd(Prelude.last (frames v))) ((3 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))  + fromIntegral (BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1))))))
                , frames v)) !! (Prelude.length (frames v ) - 1)))
              ),
            forLocals = fromIntegral(BS.index (snd (Prelude.last (frames v))) (
              1 
              + 
              4 + fromIntegral(BS.index (snd(Prelude.last (frames v))) 1) + fromIntegral(BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))) + (fromIntegral (BS.index (snd(Prelude.last (frames v))) ((3 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))  + fromIntegral (BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))))))
              +
              fromIntegral (BS.index (snd(Prelude.last (frames v))) (4 + fromIntegral(BS.index (snd(Prelude.last (frames v))) 1) + fromIntegral(BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))) + (fromIntegral (BS.index (snd(Prelude.last (frames v))) ((3 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))  + fromIntegral (BS.index(snd(Prelude.last (frames v))) (2 + (fromIntegral (BS.index (snd(Prelude.last (frames v))) 1)))))))))
            ))
            }:stack v,
            outputs = outputs v
          }
      | getFirstInstruction(Prelude.last (frames v)) == 3 = VM{
                frames = removeNInstructions(4 + (fromIntegral(BS.index (snd(Prelude.last (frames v))) 3)), frames v),
                bpOffset = bpOffset v, 
                global = global v, 
                stack = VM.parseFunc(
                  fromIntegral(BS.index (snd(Prelude.last (frames v))) 1),
                  fromIntegral(BS.index (snd(Prelude.last (frames v))) 2),
                  getFirstNInstructions(
                    fromIntegral(BS.index (snd(Prelude.last (frames v))) 3),
                    BS.empty :: ByteString, 
                    (0,snd((removeNInstructions(4, frames v)) !!(Prelude.length (frames v) - 1)))
                  )
                ):stack v,
                outputs = outputs v
              } 
      | otherwise = error ("unknown const " ++ show(fromIntegral(getFirstInstruction (Prelude.last (frames v)))))

parseInt :: Int-> Object 
parseInt i = IntObject{objectType = INT_OBJ, intValue =i }

parseFunc :: (Int,Int,ByteString) -> Object 
parseFunc (a,l,b) = FuncObject{objectType = FUNC_OBJ, numArgs = a, numLocals = l, funcValue = b}

parseString :: String -> Object 
parseString s = StringObject{objectType = STRING_OBJ, stringValue = s}

parseStack ::  VM -> String 
parseStack v = ("Stack: " ++ Prelude.concat [inspectObject x | x <- (stack v)] ++ " " ++ "Globals: " ++ Prelude.concat [inspectGlobal x | x <- (global v)])

inspectGlobal :: (Int, Object) -> String 
inspectGlobal (i, o) = (show i) ++ " = " ++ inspectObject o ++ " "

getGlobal :: (Int, VM) -> Object 
getGlobal (i, v) = o 
  where 
    o
      | member i (fromList (global v)) == False = error ("Can't find global: " ++ (show i) ++ " in " ++ Prelude.concat ["i:" ++ show (fst x) ++  " o:" ++ inspectObject(snd x) ++ " ,"| x <- global v])
      | otherwise = fromList(global v) ! i
