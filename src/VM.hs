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


getOutputs :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> [String]
getOutputs (f, b, g, s, o) = o

getStack:: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> [Object] 
getStack(f, b, g, s, o) = s 

run :: ByteString -> [String] 
run b = 
  getOutputs(runVM(
            [(0, b)],
            0, 
            [],
            [],
            []
        ))

runTest :: ByteString -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
runTest b = runVM([(0,b)], 0, [],[],[])

getFirstInstruction :: (Int, ByteString) -> Int 
getFirstInstruction  f = i 
  where 
    i
      | BS.null (snd (f)) = -1 
      | otherwise = fromIntegral (BS.head (snd f))  

runVM :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) ->([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
runVM (frames, bpOffset, global, stack, outputs) = (f,b,g,s,o)
  where 
    (f,b,g,s,o)
      | getFirstInstruction(Prelude.last frames ) == -1 = (pop frames, bpOffset, global, stack, outputs) 
    --Constant
      | getFirstInstruction(Prelude.last frames ) == 0 =
          runVM(pushToStack(
              removeFirstInstruction (frames, Prelude.length frames - 1),
              bpOffset + 1,
              global,
              stack,
              outputs
            ))
    --Pop
      | getFirstInstruction(Prelude.last frames ) == 1 = runVM(
          removeFirstInstruction (frames, Prelude.length frames - 1),
          bpOffset - 1,
          global, 
          removeFirst stack,
          outputs
          )
    --ADD
      | getFirstInstruction(Prelude.last frames ) == 2 = runVM(
              removeFirstInstruction (frames, Prelude.length frames - 1),
              bpOffset- 1,
              global,
              evalAddOp(Prelude.head stack, stack!!1):removeFirstN(2,stack),
              outputs
            )
    --Sub
      | getFirstInstruction(Prelude.last frames ) == 3 = runVM(
              removeFirstInstruction (frames, Prelude.length frames - 1),
              bpOffset- 1,
              global,
              evalSubOp(Prelude.head stack, stack!!1):removeFirstN(2, stack),
              outputs
            )
    --Mul
      | getFirstInstruction(Prelude.last frames ) == 4 = runVM(
              removeFirstInstruction (frames, Prelude.length frames - 1),
              bpOffset - 1,
              global,
              evalMulOp(Prelude.head stack, stack!!1):removeFirstN(2, stack),
              outputs
            )
    --Div
      | getFirstInstruction(Prelude.last frames ) == 5 = runVM(
            removeFirstInstruction (frames, Prelude.length frames - 1),
            bpOffset- 1,
            global,
            evalDivOp(Prelude.head stack, stack!!1):removeFirstN(2, stack),
            outputs
            )
    --True
      | getFirstInstruction(Prelude.last frames ) == 6 = runVM(
          removeFirstInstruction (frames, Prelude.length frames - 1),
          bpOffset + 1,
          global,
          BoolObject{objectType = BOOL_OBJ, boolValue = True}:stack,
          outputs
        )
    --False 
      | getFirstInstruction(Prelude.last frames ) == 7 = runVM(
          removeFirstInstruction (frames, Prelude.length frames - 1),
          bpOffset + 1,
          global, 
          BoolObject{objectType = BOOL_OBJ, boolValue = False}:stack,
          outputs
        )
    --GT 
      | getFirstInstruction(Prelude.last frames ) == 8 = runVM(
          removeFirstInstruction (frames, Prelude.length frames - 1),
          bpOffset - 1,
          global,
          evalGTOp(Prelude.head stack, stack!!1):removeFirstN(2, stack),
          outputs
      ) 
    --NEQ 
      | getFirstInstruction(Prelude.last frames ) == 10 = runVM(
          removeFirstInstruction (frames, Prelude.length frames - 1),
          bpOffset - 1,
          global,
          BoolObject{objectType = BOOL_OBJ, boolValue = Prelude.head stack /= stack!!1}:removeFirstN(2, stack),
          outputs
        ) 
    --EQ
      | getFirstInstruction(Prelude.last frames ) == 11 = runVM(
          removeFirstInstruction (frames, Prelude.length frames - 1),
          bpOffset- 1,
          global, 
          BoolObject{objectType = BOOL_OBJ, boolValue = (Prelude.head stack == stack !!1)}:(removeFirstN(2, stack)),
          outputs
        ) 
    --MINUS
      | getFirstInstruction(Prelude.last frames ) == 12 = runVM(
          removeFirstInstruction (frames,Prelude.length frames - 1),
          bpOffset,
          global, 
          minusOp (Prelude.head stack):(removeFirst stack),
          outputs
        ) 
    --BANG
      | getFirstInstruction(Prelude.last frames ) == 13 = runVM(
          removeFirstInstruction (frames,Prelude.length frames - 1),
          bpOffset,
          global, 
          bangOp (Prelude.head stack):(removeFirst stack),
          outputs
        ) 
    -- JUMP
      | getFirstInstruction(Prelude.last frames ) == 14 = runVM(
        removeNInstructions (fromIntegral (toInteger (BS.index (snd (Prelude.last frames)) 1)) :: Int, frames, Prelude.length frames - 1), 
        bpOffset,
        global,
        stack,
        outputs
      )
    -- JUMPNT
      | getFirstInstruction(Prelude.last frames ) == 15 = runVM(evalJumpNT(
          frames,
          bpOffset,
          global,
          stack,
          outputs
        ))
    -- SETGLOBAL
      | getFirstInstruction(Prelude.last frames ) == 16 = runVM(evalSetGlobal(
            removeFirstInstruction (frames, Prelude.length frames - 1),
            bpOffset,
            global,
            stack,
            outputs
          )
        )
    -- GETGLOBAL
      | getFirstInstruction(Prelude.last frames ) == 17 = runVM(evalGetGlobal(
          removeFirstInstruction (frames, Prelude.length frames - 1),
          bpOffset,
          global,
          stack,
          outputs
        ))  
      -- ARRAY
      | getFirstInstruction(Prelude.last frames ) == 18 = runVM(addArray(
            removeFirstInstruction (frames, Prelude.length frames - 1),
            bpOffset,
            global,
            stack,
            outputs
          )
        )  
      | getFirstInstruction(Prelude.last frames ) == 20 = runVM(addMap(
          removeFirstInstruction (frames,Prelude.length frames - 1),
          bpOffset,
          global,
          stack,
          outputs
        )) 
    -- INDEX
      | getFirstInstruction(Prelude.last frames ) == 22 = runVM(addIndexToStack(
          removeFirstInstruction(frames, Prelude.length frames - 1),
          bpOffset,
          global,
          stack,
          outputs
        ))
    -- SETINDEX
      | getFirstInstruction(Prelude.last frames ) == 23 = runVM(
          removeFirstInstruction(frames, Prelude.length frames - 1), 
          bpOffset - 2,
          global,
          evalAssignIndex( -- wtf is this
            removeFirst(removeFirst stack),
            Prelude.head stack,
            stack!!1
          ):[],
          outputs
        )
    -- OPCALL 
      | getFirstInstruction(Prelude.last frames ) == 24 = runVM(evalCall(
          removeFirstInstruction(frames, Prelude.length frames - 1), 
          bpOffset,
          global,
          stack,
          outputs
        ))
    -- OPRETURNVALUE 
      | getFirstInstruction(Prelude.last frames ) == 25 = 
          (frames,
          bpOffset,
          global,
          stack,
          outputs)
    -- OPRETURN 
      | getFirstInstruction(Prelude.last frames ) == 26 = 
          (removeFirstInstruction(frames,  Prelude.length frames - 1), 
          bpOffset,
          global,
          NullObject{objectType = NULL_OBJ}:stack, 
          outputs)
    -- SETLOCAL
      | getFirstInstruction(Prelude.last frames ) == 27 = runVM(setLocal(
            removeFirstInstruction(frames, Prelude.length frames - 1), 
            bpOffset,
            global,
            stack, 
            outputs
          )
        ) 
    -- GETLOCAL
      | getFirstInstruction(Prelude.last frames ) == 28 = runVM(getLocal(
          removeFirstInstruction(frames, Prelude.length frames - 1), 
          bpOffset,
          global,
          stack, 
          outputs
        ))
      | getFirstInstruction(Prelude.last frames ) == 29 = runVM(runPrebuilt(
          removeFirstInstruction(frames, Prelude.length frames - 1), 
          bpOffset,
          global,
          stack, 
          outputs
        ))
      | getFirstInstruction(Prelude.last frames ) == 30 = runVM(runFor(
          removeFirstInstruction(frames,Prelude.length frames - 1), 
          bpOffset,
          global,
          stack, 
          outputs
      ))
      | otherwise =  error "run"


getStart ::([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
getStart (frames, bpOffset, global, stack, outputs) = runVM(
      changeBP(1, frames) ++ [(0,forStart(Prelude.head stack))], 
      bpOffset - 1, 
      global, 
      NullObject{objectType = NULL_OBJ}:stack,
      outputs
    )

runFor ::([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
runFor (frames, bpOffset, global, stack, outputs) = (f,b,g,s,o) 
  where 
    (f,b,g,s,o)
      | Prelude.null stack = error "null stack?"
      | objectType (Prelude.head stack) /= FOR_OBJ = error ("not for :) " ++ (concStack stack))
      | otherwise = runForEval(
            getStart(
              frames, 
              bpOffset,
              global,
              stack,
              outputs
          )
        )


runForEval ::([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
runForEval (frames, bpOffset, global, stack, outputs) = (f,b,g,s,o) 
  where 
    (f,b,g,s,o) 
      | boolValue (Prelude.head (getStack (runVM(
          Utils.append frames (0, forCon (stack!!1)),
          bpOffset,
          global,
          stack,
          outputs
        )))) == False = 
            (
              changeBP(-1, frames),
              bpOffset,
              global, 
              removeFirstN(2, stack),
              outputs
            )
      | otherwise =  
        runForEval(runInc(runVM(
          Utils.append frames (0, forBod(stack!!1)),
          bpOffset,
          global,
          stack,
          outputs
        )))

runInc ::([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
runInc (frames, bpOffset, global, stack, outputs) =
  (runVM(
    Utils.append frames (0, forInc (stack !! 1)),
    bpOffset,
    global,
    stack,
    outputs
  ))


runPrebuilt ::([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
runPrebuilt (frames, bpOffset, global, stack, outputs) = (f,b,g,s,o)
  where 
    (f,b,g,s,o)
    --len
      | getFirstInstruction(Prelude.last frames) == 0 =( 
          removeFirstInstruction(frames, Prelude.length frames - 1), 
          bpOffset,
          global,
          IntObject{objectType = INT_OBJ, intValue = getObjectLen(Prelude.head stack)}:(removeFirst stack),
          outputs
          )
      --print
      | getFirstInstruction(Prelude.last frames) == 1 = ( 
          removeFirstInstruction(frames, Prelude.length frames - 1), 
          bpOffset - 1,
          global,
          removeFirst stack,
          Utils.append outputs (inspectObject(Prelude.head stack))
        )
      -- append
      | getFirstInstruction(Prelude.last frames) == 2 = ( 
          removeFirstInstruction(frames, Prelude.length frames - 1), 
          bpOffset - 1,
          global,
          ArrayObject{objectType = ARRAY_OBJ, arrValue = arrValue (stack !!1) ++ [Prelude.head stack]}:removeFirstN(2, stack),
          outputs
        )
      | otherwise = error ("not implemented prebuilt" ++ show (getFirstInstruction(Prelude.last frames)))

getLocal ::([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
getLocal (frames, bpOffset, global, stack, outputs) = (
    removeFirstInstruction(removeFirstInstruction (frames, Prelude.length frames - 1), Prelude.length frames - 1),
    bpOffset + 1,
    global,
    stack!!getLocalEleIdx(frames, bpOffset) :stack,
    outputs
  )

concStack :: [Object] -> String 
concStack o = s 
  where 
    s
     | Prelude.null o = ""
     | otherwise =  Prelude.concat [inspectObject x ++ " " | x <- o]

setLocal :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
setLocal (frames, bpOffset, global, stack, outputs) =
  (
    removeFirstInstruction(removeFirstInstruction (frames, Prelude.length frames - 1), Prelude.length frames - 1),
    bpOffset - 1,
    global,
    removeFirst(stack & element (getLocalEleIdx (frames, bpOffset)) .~ stack!!0),
    outputs
  ) 

getNInstruction :: (Int, [(Int, ByteString)], Int) -> Int 
getNInstruction (i, f, idx) = 
  fromIntegral(BS.index(snd(f!!idx)) i)


getLocalEleIdx :: ([(Int, ByteString)], Int) -> Int 
getLocalEleIdx (frames, bpOffset) = 
  getBasePointerIdx(getNInstruction(0,frames, Prelude.length frames - 1)-1, frames) - 1 + 
  bpOffset - 
  getNInstruction(1, frames, Prelude.length frames - 1) 

getBasePointerIdx :: (Int,[(Int, ByteString)]) -> Int 
getBasePointerIdx (i,frames) = fst(frames !!i)

evalParams :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
evalParams (frames, bpOffset, global, stack, outputs) =
        (
          removeFirstInstruction(changeBP(numLocals (Prelude.head stack), frames), Prelude.length frames - 1) ++ [(0,funcValue (Prelude.head stack))], 
          0,
          global,
          [NullObject{objectType = NULL_OBJ} | x <- [1 .. (numLocals (Prelude.head stack) - numArgs(Prelude.head stack))]] ++ removeFirst stack,
          outputs
        ) 

changeBP :: (Int, [(Int, ByteString)]) -> [(Int, ByteString)] 
changeBP (i, f) = [(fst x + i, snd x) | x <- f] 


evalCall :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
evalCall (frames, bpOffset, global, stack, outputs) = evalReturn(runVM(evalParams(frames, bpOffset, global, stack, outputs)))

concGlobal :: [(Int, Object)] -> String 
concGlobal o = concStack([snd x | x <- o])


evalReturn :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
evalReturn (frames, bpOffset, global, stack, outputs) = (f,b,g,s,o)
  where 
    (f,b,g,s,o)
      | objectType (Prelude.head stack) == NULL_OBJ = 
          (
            changeBP(-1 * getBasePointerIdx(Prelude.length frames -2, frames),pop frames),
            0,
            global, 
            removeFirstN( 1 + getBasePointerIdx(Prelude.length frames-2, frames), stack),
            outputs
          )
      | otherwise =
          (
            changeBP(-1 * getBasePointerIdx(Prelude.length frames -2, frames), pop frames),
            1,
            global, 
            stack!!0:removeFirstN(getBasePointerIdx(Prelude.length frames -2, frames) + 1, stack), 
            outputs
          )


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

addIndexToStack :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
addIndexToStack (frames, bpOffset, global, stack, outputs) = 
      (
        frames, 
        bpOffset - 1,
        global, 
        (evalIndex (stack!!0, stack!!1)):(removeFirstN(2, stack)),
        outputs
      )

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

addMap :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
addMap (frames, bpOffset, global, stack, outputs) = 
  (
    removeFirstInstruction(frames, Prelude.length frames - 1), 
    bpOffset - fromIntegral(BS.head (snd(Prelude.last frames))) + 1,
    global,
    MapObject{objectType = MAP_OBJ, mapValue = generateMap(getFirstN(fromIntegral(BS.head(snd(Prelude.last frames))),stack, []))}:removeFirstN(fromIntegral (BS.head (snd(Prelude.last frames))), stack), 
    outputs
  )
generateMap :: [Object] -> [(Object, Object)]
generateMap [] = []
generateMap (k:v:t) = (k, v): generateMap t

addArray :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
addArray (frames, bpOffset, global, stack, outputs) = 
    (
      removeFirstInstruction(frames, Prelude.length frames - 1),
      bpOffset - fromIntegral(BS.head (snd(Prelude.last frames))) + 1,
      global, 
      ArrayObject{objectType = ARRAY_OBJ, arrValue = getFirstN(fromIntegral(BS.head (snd(Prelude.last frames))), stack, [])}:removeFirstN(fromIntegral(BS.head (snd(Prelude.last frames))),stack),
      outputs
    )

evalGetGlobal :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
evalGetGlobal (frames, bpOffset, global, stack, outputs) = 
    (
      removeFirstInstruction(frames, Prelude.length frames - 1),
      bpOffset + 1,
      global, 
      fromList global ! (getFirstInstruction (Prelude.last frames)):stack, 
      outputs
    )

evalSetGlobal :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) -> ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
evalSetGlobal (frames, bpOffset, global, stack, outputs) = (f,b,g,s,o) 
  where 
    (f,b,g,s,o) 
      | Prelude.null stack = error "can't set null"
      | member (getFirstInstruction (Prelude.last frames)) (fromList global) == False =(
          removeFirstInstruction (frames, Prelude.length frames - 1), 
          bpOffset - 1,
          global ++ [(getFirstInstruction (Prelude.last frames),(Prelude.head stack))], 
          removeFirst stack,
          outputs
        )
      | otherwise = 
        (removeFirstInstruction (frames, Prelude.length frames - 1), 
        bpOffset - 1,
        (getFirstInstruction(Prelude.last frames), Prelude.head stack):[x | x <- global, fst x /= getFirstInstruction(Prelude.last frames)],
        removeFirst stack,
        outputs)


evalJumpNT :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) ->([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) 
evalJumpNT (frames, bpOffset, global, stack, outputs) = (f,b,g,s,o) 
  where 
    (f, b, g, s, o) 
      | objectType (Prelude.head stack) /= BOOL_OBJ = error ("jump not bool"++ Prelude.concat [inspectObject x ++ " "| x <- stack]) 
      | boolValue (Prelude.head stack) == True = 
            (removeNInstructions(2, frames, Prelude.length frames - 1),
            bpOffset - 1,
            global,
            removeFirst stack,
            outputs)
      -- Should jump 
      | otherwise = 
            (removeNInstructions (fromIntegral (BS.index (snd (Prelude.last frames)) 1), frames, Prelude.length frames - 1),
            bpOffset - 1,
            global,
            removeFirst stack,
            outputs)


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
      | otherwise = BoolObject{objectType = BOOL_OBJ, boolValue = intValue o1 < intValue o2}

evalAddOp:: (Object, Object) -> Object 
evalAddOp(o1, o2) = o 
  where 
    o
      | objectType o1 == INT_OBJ && objectType o2 == INT_OBJ =  IntObject{objectType = INT_OBJ, intValue = intValue o2 + intValue o1}
      | objectType o1 == STRING_OBJ = StringObject{objectType = STRING_OBJ, stringValue = stringValue o2 ++ stringValue o1}
      | otherwise = error ("can't do operation with types: " ++ inspectObject(o1) ++ " " ++ inspectObject(o2))

evalSubOp :: (Object, Object) -> Object 
evalSubOp (o1, o2) = o 
  where 
    o
      | objectType o1 == INT_OBJ && objectType o2 == INT_OBJ = 
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

pushToStack :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String]) ->([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  
pushToStack (frames, bpOffset, global, stack, outputs) = (f, b, g, s, o) 
  where 
    (f, b, g, s, o)
      | getFirstInstruction(Prelude.last frames) == 1 = 
          (removeNInstructions(2 + (fromIntegral(BS.index (snd(Prelude.last frames)) 1)), frames, Prelude.length frames - 1),
          bpOffset, 
          global, 
          parseInt(bsToInt(
          getFirstNInstructions(
              fromIntegral(BS.index (snd(Prelude.last frames)) 1),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(2, frames, (Prelude.length frames)- 1)) !! ((Prelude.length frames)- 1))))))
          :stack,
          outputs)
      | getFirstInstruction(Prelude.last frames) == 0 =
          (removeNInstructions(2 + (fromIntegral(BS.index (snd(Prelude.last frames)) 1)), frames, Prelude.length frames - 1),
          bpOffset,
          global,
          VM.parseString(convertBytesToString(
          getFirstNInstructions(
              fromIntegral(BS.index (snd(Prelude.last frames)) 1),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(2, frames, Prelude.length frames - 1)) !! (Prelude.length frames - 1))))))
          :stack,
          outputs)
      | getFirstInstruction (Prelude.last frames) == 2 = 
          (removeNInstructions(
            -- for + start + stop + inc + body 
            5 +  
            -- start
            fromIntegral(BS.index (snd(Prelude.last frames)) 1) + 
            -- stop
            fromIntegral(BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))) +  
            -- inc 
            fromIntegral (BS.index (snd(Prelude.last frames)) ((3 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))  + fromIntegral (BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))))) + 
            -- body
            fromIntegral (BS.index (snd(Prelude.last frames)) (4 + fromIntegral(BS.index (snd(Prelude.last frames)) 1) + fromIntegral(BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))) + (fromIntegral (BS.index (snd(Prelude.last frames)) ((3 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))  + fromIntegral (BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames )) 1)))))))))
          , frames, Prelude.length frames - 1),
          bpOffset,
          global,
          ForObject{
            objectType = FOR_OBJ, 
            forStart = getFirstNInstructions(
              fromIntegral(BS.index (snd (Prelude.last frames)) 1),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(2, frames, Prelude.length frames - 1)) !! (Prelude.length frames - 1)))
            ),
            forCon = getFirstNInstructions(
              fromIntegral(BS.index (snd(Prelude.last frames)) (2 + fromIntegral(BS.index (snd (Prelude.last frames)) 1))),
              BS.empty :: ByteString,
              (0,snd((removeNInstructions(3 + fromIntegral(BS.index (snd (Prelude.last frames)) 1), frames, Prelude.length frames - 1)) !! (Prelude.length frames - 1)))
            ), 
            forInc = getFirstNInstructions(
              fromIntegral (BS.index (snd(Prelude.last frames)) ((3 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))  + fromIntegral (BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))))),
              BS.empty :: ByteString, 
              (0,snd((removeNInstructions(4 + fromIntegral(BS.index (snd (Prelude.last frames)) 1) + fromIntegral(BS.index (snd(Prelude.last frames)) (2 + fromIntegral(BS.index (snd (Prelude.last frames)) 1))), frames, Prelude.length frames - 1)) !! (Prelude.length frames - 1)))
            ), 
            forBod = getFirstNInstructions(
            fromIntegral (BS.index (snd(Prelude.last frames)) (4 + fromIntegral(BS.index (snd(Prelude.last frames)) 1) + fromIntegral(BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))) + (fromIntegral (BS.index (snd(Prelude.last frames)) ((3 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))  + fromIntegral (BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1))))))))),
            BS.empty :: ByteString, 
              (0,snd((removeNInstructions(
                5 + 
                fromIntegral(BS.index (snd (Prelude.last frames)) 1) + 
                fromIntegral(BS.index (snd(Prelude.last frames)) (2 + fromIntegral(BS.index (snd (Prelude.last frames)) 1))) + 
                fromIntegral (BS.index (snd(Prelude.last frames)) ((3 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1)))  + fromIntegral (BS.index(snd(Prelude.last frames)) (2 + (fromIntegral (BS.index (snd(Prelude.last frames)) 1))))))
                , frames, Prelude.length frames - 1)) !! (Prelude.length frames - 1)))
              )
            }:stack,
            outputs)
      | getFirstInstruction(Prelude.last frames) == 3 =
                (removeNInstructions(4 + (fromIntegral(BS.index (snd(Prelude.last frames)) 3)), frames, Prelude.length frames - 1),
                bpOffset,
                global, 
                VM.parseFunc(
                  fromIntegral(BS.index (snd(Prelude.last frames)) 1),
                  fromIntegral(BS.index (snd(Prelude.last frames)) 2),
                  getFirstNInstructions(
                    fromIntegral(BS.index (snd(Prelude.last frames)) 3),
                    BS.empty :: ByteString, 
                    (0,snd((removeNInstructions(4, frames, Prelude.length frames - 1)) !! (Prelude.length frames - 1)))
                  )
                ):stack,
                outputs)
      | otherwise = error ("unknown const " ++ show(fromIntegral(getFirstInstruction (Prelude.last frames))))

parseInt :: Int-> Object 
parseInt i = IntObject{objectType = INT_OBJ, intValue =i }

parseFunc :: (Int,Int,ByteString) -> Object 
parseFunc (a,l,b) = FuncObject{objectType = FUNC_OBJ, numArgs = a, numLocals = l, funcValue = b}

parseString :: String -> Object 
parseString s = StringObject{objectType = STRING_OBJ, stringValue = s}

parseStack :: ([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])  -> String 
parseStack (frames, bpOffset, global, stack, outputs) = ("Stack: " ++ Prelude.concat [inspectObject x | x <- stack] ++ " " ++ "Globals: " ++ Prelude.concat [inspectGlobal x | x <- global])

inspectGlobal :: (Int, Object) -> String 
inspectGlobal (i, o) = (show i) ++ " = " ++ inspectObject o ++ " "

getGlobal :: (Int,([(Int, ByteString)], Int, [(Int, Object)], [Object], [String])) -> Object 
getGlobal (i,(frames, bpOffset, global, stack, outputs)) = o 
  where 
    o
      | member i (fromList global) == False = error ("Can't find global: " ++ (show i) ++ " in " ++ Prelude.concat ["i:" ++ show (fst x) ++  " o:" ++ inspectObject(snd x) ++ " ,"| x <- global])
      | otherwise = fromList global ! i

removeNInstructions :: (Int, [(Int, ByteString)], Int) -> [(Int, ByteString)]
removeNInstructions (i, b, idx) = bs 
  where 
    bs 
      | i == 0 = b 
      | otherwise = removeNInstructions(i-1, removeFirstInstruction (b, idx), idx)
