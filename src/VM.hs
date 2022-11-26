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
        frames = [(0, scopes c!!0)],
        frameIndex = 0,
        bpOffset = 0, 
        constVM = constants c,
        global = [],
        stack = []
      }
  )

traceVM :: (String, VM) -> String 
traceVM (s, v) =  s ++ " bpOffset: " ++ show(bpOffset v) ++ " bp: " ++ (show (fst (Prelude.last (frames v)))) ++ " firstInstruct: " ++ show(getFirstInstruction(frames v !! frameIndex v)) ++ " sndBP: " ++ show(fst(frames v !!(frameIndex v - 1)))
   


data VM = VM{
    frames :: ![(Int,ByteString)],
    frameIndex :: !Int, 
    bpOffset :: !Int, 
    constVM:: ![Object],
    global :: ![(Int, Object)],
    stack :: ![Object] 
  } deriving (Show)

getFirstInstruction :: (Int, ByteString) -> Int 
getFirstInstruction  f = i 
  where 
    i
      | BS.null (snd (f)) = -1 
      | otherwise = fromIntegral (BS.head (snd f))  

runVM :: VM -> VM
runVM v = 
  case getFirstInstruction(frames v !! frameIndex v) of 
    -- NULL 
    -1 -> v 
    --Constant
    0 -> 
      -- trace (traceVM("CONST", v))
      runVM(pushToStack(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v + 1,
          constVM = constVM v, 
          global = global v, 
          stack = stack v
        }))
    --Pop
    1 -> runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v - 1,
          constVM = constVM v, 
          global = global v, 
          stack = removeFirst (stack v)
          })
    --ADD
    2 ->  
      trace ("add")
      runVM(VM{
              frames = removeFirstInstruction (frames v, frameIndex v),
              frameIndex = frameIndex v,
              bpOffset = bpOffset v- 1,
              constVM = constVM v, 
              global = global v,
              stack =addOp (stack v)
            })
    --Sub
    3 ->  
        trace ("sub")
        runVM(VM{
              frames = removeFirstInstruction (frames v, frameIndex v),
              frameIndex = frameIndex v,
              bpOffset = bpOffset v- 1,
              constVM = constVM v, 
              global = global v,
              stack =subOp (stack v)
            })
    --Mul
    4 -> 
      trace ("mul")
      runVM(VM{
              frames = removeFirstInstruction (frames v, frameIndex v),
              frameIndex = frameIndex v,
              bpOffset = bpOffset v- 1,
              constVM  = constVM v, 
              global = global v,
              stack =mulOp (stack v)
            })
    --Div
    5 -> 
      trace ("div")
      runVM(VM{
              frames = removeFirstInstruction (frames v, frameIndex v),
              frameIndex = frameIndex v,
              bpOffset = bpOffset v- 1,
              constVM = constVM v, 
              global = global v,
              stack =divOp (stack v)
            })
    --True
    6 -> runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v+ 1,
          constVM = constVM v, 
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = True}:(stack v)
        }
      )
    --False 
    7 -> runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          constVM = constVM v, 
          bpOffset = bpOffset v+ 1,
          global = global v, 
          stack = BoolObject{objectType = BOOL_OBJ, boolValue = False}:(stack v)
        })
    --GT 
    8 -> runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          constVM = constVM v,
          bpOffset = bpOffset v- 1,
          global = global v,
          stack = gtOp (stack v)
        }
      ) 
    --NEQ 
    10 -> 
      runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          constVM = constVM v,
          bpOffset = bpOffset v- 1,
          global = global v, 
          stack = neqOp (stack v)
        }) 
    --EQ
    11 -> runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v- 1,
          constVM = constVM v,
          global = global v, 
          stack = eqOp (stack v)
        }) 
    --MINUS
    12 -> runVM(VM{
          frames = removeFirstInstruction (frames v,frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v,
          constVM = constVM v,
          global = global v, 
          stack = minusOp (stack v)
        }) 
    --BANG
    13 -> runVM(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v,
          constVM = constVM v,
          global = global v, 
          stack = bangOp (stack v)
        }) 
    -- JUMP
    14 -> runVM(evalJump(v))
    -- JUMPNT
    15 -> runVM(evalJumpNT(v))
    -- SETGLOBAL
    16 -> 
      -- trace (traceVM ("setGlobal", v))
      runVM(evalSetGlobal(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))
    -- GETGLOBAL
    17 -> 
      -- trace (traceVM ("getGlobal", v))
      runVM(evalGetGlobal(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))  
      -- ARRAY
    18 -> runVM(addArray (VM{
            frames = removeFirstInstruction (frames v, frameIndex v),
            frameIndex = frameIndex v,
            constVM = constVM v,
            bpOffset = bpOffset v,
            global = global v,
            stack = stack v
          }))  
    20 -> runVM(addMap (VM{
          frames = removeFirstInstruction (frames v,frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        })) 
    -- INDEX
    22 -> runVM(addIndexToStack(VM{
          frames = removeFirstInstruction (frames v, frameIndex v),
          frameIndex = frameIndex v,
          bpOffset = bpOffset v,
          constVM = constVM v, 
          global = global v,
          stack = stack v
        }))
    -- SETINDEX
    23 -> runVM(VM{
          frames = removeFirstInstruction(frames v, frameIndex v), 
          frameIndex = frameIndex v, 
          constVM = constVM v, 
          bpOffset = bpOffset v - 2,
          global = global v,
          stack = evalAssignIndex(
            removeFirst(removeFirst(stack v)),
            stack v!!0,
            stack v!!1
          ):[]--(removeFirst(removeFirst(stack v))) 
        })
    -- OPCALL 
    24 ->   
        -- trace("OPCALL: bp: " ++ (show (bpOffset v)))
        runVM(evalCall(VM{
          frames = removeFirstInstruction(frames v, frameIndex v), 
          frameIndex = frameIndex v, 
          bpOffset = bpOffset v,
          constVM = constVM v,
          global = global v,
          stack = stack v
        }))
    -- OPRETURNVALUE 
    25 -> 
         VM{
          frames = frames v, 
          frameIndex = frameIndex v, 
          bpOffset = bpOffset v,
          constVM = constVM v, 
          global = global v,
          stack = stack v
        }
    -- OPRETURN 
    26 -> VM{
          frames = removeFirstInstruction(frames v,  frameIndex v), 
          frameIndex = frameIndex v, 
          bpOffset = bpOffset v,
          constVM = constVM v, 
          global = global v,
          stack = NullObject{objectType = NULL_OBJ}:stack v 
        } 
    -- SETLOCAL
    27 -> runVM(setLocal(VM{
          frames = removeFirstInstruction(frames v, frameIndex v), 
          frameIndex = frameIndex v, 
          bpOffset = bpOffset v,
          constVM = constVM v, 
          global = global v,
          stack = stack v 
        })) 
    -- GETLOCAL
    28 -> runVM(getLocal(VM{
          frames = removeFirstInstruction(frames v, frameIndex v), 
          frameIndex = frameIndex v, 
          bpOffset = bpOffset v,
          constVM = constVM v, 
          global = global v,
          stack = stack v 
        }))
    _ -> error "run"


getLocal :: VM -> VM 
getLocal v = 
  trace(traceVM ("getLocal", v))
  trace("     accessing idx: " ++ show(getLocalEleIdx v))
  trace("     element: " ++ show(stack v!!(getLocalEleIdx v)))
  trace("     new stack: " ++ concStack(stack v!!(getLocalEleIdx v):stack v))
  VM{
    frames = removeFirstInstruction(removeFirstInstruction (frames v, frameIndex v), frameIndex v),
    frameIndex = frameIndex v,
    bpOffset = bpOffset v+ 1,
    constVM = constVM v,
    global = global v,
    stack = stack v!!(getLocalEleIdx v) :stack v
  }

concStack :: [Object] -> String 
concStack o = s 
  where 
    s
     | Prelude.null o = ""
     | otherwise =  Prelude.concat [inspectObject x ++ " " | x <- o]

setLocal :: VM -> VM 
setLocal v = 
  trace(traceVM ("setLocal", v))
  trace("     bp: " ++ show(fst(frames v!!(Prelude.length (frames v) - 1))))
  trace("     bps:" ++ Prelude.concat [show(fst x) ++ " "| x <- frames v])
  trace("     idx: " ++ show(getLocalEleIdx v))
  trace("     element: " ++ inspectObject (stack v !!0))
  trace("     stack: " ++ concStack(stack v))
  trace("     localEleIdx: " ++ show(getLocalEleIdx v))
  trace("     replaced: " ++ inspectObject (stack v !!(getLocalEleIdx v)))
  trace("     new stack: " ++ concStack(
    removeFirst(
      stack v & element (getLocalEleIdx v)
       .~ stack v!!0)
    )
  )
  VM{
    frames = removeFirstInstruction(removeFirstInstruction (frames v, frameIndex v), frameIndex v),
    frameIndex = frameIndex v,
    bpOffset = bpOffset v- 1,
    constVM = constVM v,
    global = global v,
    stack = removeFirst(stack v & element (getLocalEleIdx v) .~ stack v!!0)
  }

getNInstruction :: (Int, [(Int, ByteString)], Int) -> Int 
getNInstruction (i, f, idx) = 
  fromIntegral(BS.index(snd(f!!idx)) i)

errorStack :: VM -> VM 
errorStack v = error ("FirstInstruct: " ++ (show (getFirstInstruction (frames v !! frameIndex v))) ++  " Stack: " ++ Prelude.concat [inspectObject x ++  " " | x <- stack v])

getBasePointer :: VM -> Int 
getBasePointer v = fst(frames v !! frameIndex v)

getLocalEleIdx :: VM -> Int 
getLocalEleIdx v = 
  trace("   getLocalEleIdx: ")
  -- trace("       getBPIDX: " ++ show(getBasePointerIdx(getNInstruction(0, frames v, frameIndex v), v) ))
  trace("       getNInstruction: " ++ show(getNInstruction(Prelude.length (frames v) - 1, frames v, frameIndex v)))
  trace("       instructs: " ++ Prelude.concat[show(fromIntegral x)++ " "| x <- unpack(snd(frames v!!frameIndex v))])
  trace("       Prelude.lgnth: " ++ show(Prelude.length(frames v) - 1))
  trace("       mby should be this?: " ++ show(getNInstruction(1, frames v, frameIndex v)))
  getBasePointerIdx(getNInstruction(0, frames v, frameIndex v), v) + 
  bpOffset v + 
  getNInstruction(1, frames v, frameIndex v) 

getBasePointerIdx :: (Int,VM) -> Int 
getBasePointerIdx (i,v) = fst(frames v !!i)

evalParams :: VM -> VM 
evalParams v = vm 
  where 
    vm 
      | otherwise = 
        trace("evalParams")
        trace("      new stack: " ++  concStack([NullObject{objectType = NULL_OBJ} | x <- [1 .. (numLocals (Prelude.head (stack v)) - numArgs(Prelude.head (stack v)))]] ++ removeFirst (stack v)))
        trace("      new offset: " ++  show(bpOffset v - numArgs(Prelude.head (stack v)) - 1))
        trace("      new bps: " ++ Prelude.concat [show(fst x) ++ " " | x <- removeFirstInstruction(changeBP(numLocals (Prelude.head (stack v)), frames v), frameIndex v) ++ [(0,funcValue (Prelude.head (stack v)))]])
        VM{ 
          frames = removeFirstInstruction(changeBP(numLocals (Prelude.head (stack v)), frames v), frameIndex v) ++ [(0,funcValue (Prelude.head (stack v)))], 
          frameIndex = frameIndex v + 1, 
          bpOffset = bpOffset v - numArgs(Prelude.head (stack v)) - 1,
          constVM = constVM v, 
          global = global v,
          stack =removeFirst (stack v)++ [NullObject{objectType = NULL_OBJ} | x <- [1 .. (numLocals (Prelude.head (stack v)) - numArgs(Prelude.head (stack v)))]] 
        } 

changeBP :: (Int, [(Int, ByteString)]) -> [(Int, ByteString)] 
changeBP (i, f) = [(fst x + i, snd x) | x <- f] 

evalCall :: VM -> VM 
evalCall v = evalReturn(runVM(evalParams(v)))

evalReturn :: VM -> VM 
evalReturn v = vm 
  where 
    vm 
      | objectType (Prelude.head (stack v)) == NULL_OBJ = 
        trace("evalReturn")
        trace("     removeN:" ++show(getBasePointerIdx(Prelude.length (frames v) -2, v) + 1) )
        VM{
          frames =  changeBP( getBasePointerIdx(Prelude.length (frames v) -2, v) + 1, pop (frames v)),
          frameIndex = frameIndex v - 1,
          constVM = constVM v, 
          bpOffset = 0,
          global = global v, 
          stack = removeFirstN( getBasePointerIdx(Prelude.length (frames v) -2, v), stack v)
        }
      | otherwise =
        trace(traceVM("evalReturn", v))
        trace("     bpOffset: " ++ show(bpOffset v))
        trace("     removing: " ++ show(getBasePointerIdx(Prelude.length (frames v) -2, v)))
        trace("     bps:" ++ Prelude.concat (pop[show(fst x) ++ " "| x <- frames v]))
        trace("     prev stack: " ++ show(concStack(stack v)))
        trace("     returning: " ++ inspectObject(stack v!!0))
        VM{
          frames =  changeBP(getBasePointerIdx(Prelude.length (frames v) -2, v) + 1, pop (frames v)),
          frameIndex = frameIndex v - 1,
          constVM = constVM v, 
          bpOffset = 1,
          global = global v, 
          stack =stack v !!0:removeFirstN(getBasePointerIdx(Prelude.length (frames v) -2, v), stack v) 
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
      trace("     addIndexToStack: ")
      trace("     newStack: " ++ concStack((evalIndex (stack v!!0, stack v!!1)):(removeFirst(removeFirst (stack v)))))
      VM{
          frames = frames v, 
          frameIndex = frameIndex v,
          bpOffset = bpOffset v - 1,
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
      | otherwise = error ("key doesn't exist: key: " ++ inspectObject key ++ " mp: " ++ inspectObject mp)

evalArrayIndex :: (Object, Object) -> Object 
evalArrayIndex (idx, arr) = val
  where 
    val 
      | objectType idx == INT_OBJ && isWithinBounds(intValue idx, Prelude.length (arrValue arr)) = arrValue arr!!intValue idx 
      | otherwise = error "can't access array with non int key"

addMap :: VM -> VM 
addMap v = VM{
    frames = removeFirstInstruction(frames v, frameIndex v), 
    frameIndex = frameIndex v,
    constVM = constVM v,
    bpOffset = bpOffset v - fromIntegral(BS.head (snd(frames v !! frameIndex v))) + 1,
    global = global v,
    stack = MapObject{objectType = MAP_OBJ, mapValue = generateMap(getFirstN(fromIntegral(BS.head(snd(frames v !!frameIndex v))),stack v, []))}:removeFirstN(fromIntegral (BS.head (snd(frames v !!frameIndex v))), stack v) 
  } 
generateMap :: [Object] -> [(Object, Object)]
generateMap [] = []
generateMap (k:v:t) = (k, v): generateMap t

addArray :: VM -> VM 
addArray v = VM{
    frames = removeFirstInstruction(frames v, frameIndex v),
    frameIndex = frameIndex v, 
    bpOffset = bpOffset v - fromIntegral(BS.head (snd(frames v !! frameIndex v))) + 1,
    constVM = constVM v, 
    global = global v, 
    stack = ArrayObject{objectType = ARRAY_OBJ, arrValue = getFirstN(fromIntegral(BS.head (snd(frames v !! frameIndex v))), stack v, [])}:removeFirstN(fromIntegral(BS.head (snd(frames v !!frameIndex v))),stack v)
  } 

evalGetGlobal :: VM ->  VM 
evalGetGlobal v = 
  VM{
    frames = removeFirstInstruction(frames v, frameIndex v),
    frameIndex = frameIndex v,
    constVM = constVM v,
    bpOffset = bpOffset v + 1,
    global = global v, 
    stack = fromList (global v) ! (getFirstInstruction (frames v !! frameIndex v)):(stack v) 
  }

evalSetGlobal :: VM ->  VM 
evalSetGlobal v = vm
  where 
    vm 
      | member (getFirstInstruction (frames v !! frameIndex v)) (fromList (global v)) == False = VM{
          frames = removeFirstInstruction (frames v, frameIndex v), 
          frameIndex = frameIndex v,
          bpOffset = bpOffset v - 1,
          constVM = constVM v,
          global = global v ++ [(getFirstInstruction (frames v !!frameIndex v),(Prelude.head (stack v)))], 
          stack = removeFirst (stack v)
        } 
      | otherwise = VM{
          frames = removeFirstInstruction (frames v, frameIndex v), 
          frameIndex = frameIndex v,
          bpOffset = bpOffset v - 1,
          constVM = constVM v,
          global = (getFirstInstruction(frames v !! frameIndex v), Prelude.head (stack v)):[x | x <- global v, fst x /= getFirstInstruction(frames v !! frameIndex v)],
          stack = removeFirst (stack v)
        }


evalJump :: VM -> VM 
evalJump v = VM{
    frames = removeNInstructions (fromIntegral (toInteger (BS.index (snd (frames v !!frameIndex v)) 1)) :: Int, frames v, frameIndex v),
    frameIndex = frameIndex v,
    constVM = constVM v,
    bpOffset = bpOffset v,
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
            bpOffset = bpOffset v - 1,
            constVM = constVM v,
            global = global v,
            stack = removeFirst (stack v)
          }
      -- Should jump 
      | otherwise = 
        VM{
            frames = removeNInstructions (fromIntegral (BS.index (snd (frames v !! frameIndex v)) 1), frames v, frameIndex v),
            frameIndex = frameIndex v,
            bpOffset = bpOffset v - 1,
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
addOp o =
  trace("     stack: " ++concStack(evalAddOp(o!!0, o!!1):(removeFirst (removeFirst o)) ) )
  evalAddOp(o!!0, o!!1):(removeFirst (removeFirst o)) 

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
            bpOffset = bpOffset v,
            constVM = constVM v,
            global = global v, 
            stack = (constVM v)!!(getFirstInstruction(frames v !!frameIndex v)):(stack v)
          }


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
