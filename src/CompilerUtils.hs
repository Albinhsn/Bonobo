module CompilerUtils where

import Debug.Trace
import Object 
import Ast
import Code 
import Lexer 
import Utils
import Parser

import Control.Lens
import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU 

reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = Prelude.last xs : reverseList (Prelude.init xs)

parseStringToStatementsString :: String -> String 
parseStringToStatementsString s =  statementsToString(parseStatements(getTokens(parseTokens(0, s, []))))

parseStringToStatements :: String -> [Statement] 
parseStringToStatements s = parseStatements(getTokens(parseTokens(0, s, [])))



disassembleConst :: (String, Compiler)-> (String, Compiler) 
disassembleConst (s, c) = str 
  where 
    str 
      -- STR 
      | BS.index (scopes c !! scopeIndex c) 1 == 0 = (s ++ " CONST " ++ "STR LEN " ++ 
        (show (BS.index (scopes c !! scopeIndex c) 2)) ++ " " ++ 
        (
          convertBytesToString(
            getFirstNInstructions(
              fromIntegral(BS.index (scopes c !! scopeIndex c) 2),
              BS.empty :: ByteString,
              (0,scopes (removeNFromScope(3, c)) !! scopeIndex c)))), 
            removeNFromScope(3 + fromIntegral(BS.index (scopes c !! scopeIndex c) 2), c))
      -- INT  
      | BS.index (scopes c !! scopeIndex c) 1 == 1 = (s ++ " CONST " ++ "INT LEN " ++ 
        (show (BS.index (scopes c!!scopeIndex c) 2)) ++ " " ++ show(bsToInt(
          getFirstNInstructions(
              fromIntegral(BS.index (scopes c !! scopeIndex c) 2),
              BS.empty :: ByteString,
              (0,scopes (removeNFromScope(3, c)) !! scopeIndex c)))), 
        removeNFromScope(3 + (fromIntegral(BS.index (scopes c!!scopeIndex c) 2)),c))
      -- FUNC
      | BS.index (scopes c !! scopeIndex c) 1 == 3  = (s ++ " CONST " ++ "FUNC ARGS: " ++ (show (BS.index (scopes c!!scopeIndex c) 2)) ++ " LOCALS: "++ (show (BS.index (scopes c!!scopeIndex c) 3)) ++ " LEN: " ++ (show (BS.index (scopes c!!scopeIndex c) 4)), removeNFromScope(5 + (fromIntegral (BS.index (scopes c !!scopeIndex c) 4)), c))

disassemble :: (String, Compiler) -> String 
disassemble (s,c)= str 
  where 
    str 
      | BS.null (scopes c!!scopeIndex c) = s
      | BS.head (scopes c!!scopeIndex c) == 0 = disassemble(CompilerUtils.disassembleConst(s,c))
      | BS.head (scopes c!!scopeIndex c) == 1 = disassemble(s ++ " POP", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 2 = disassemble(s ++ " ADD", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 3 = disassemble(s ++ " SUB", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 4 = disassemble(s ++ " MUL", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 5 = disassemble(s ++ " DIV", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 6 = disassemble(s ++ " TRUE", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 7 = disassemble(s ++ " FALSE", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 8 = disassemble(s ++ " GT", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 9 = disassemble(s ++ " LT", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 10 = disassemble(s ++ " NEQ", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 11 = disassemble(s ++ " EQ", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 12 = disassemble(s ++ " MINUS", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 13 = disassemble(s ++ " BANG", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 14 = disassemble(s ++ " JUMP " ++ (show (BS.index (scopes c!!scopeIndex c) 1)),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 15 = disassemble(s ++ " JUMPNT " ++ (show (BS.index (scopes c!!scopeIndex c) 1)),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 16 = 
        disassemble(s ++ " SETGLOBAL " ++ (show (BS.index (scopes c!!scopeIndex c) 1)),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 17 = 
        disassemble(s ++ " GETGLOBAL " ++ (show (BS.index (scopes c!!scopeIndex c) 1)),removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 18 = disassemble(s ++ " ARRAY "++ (show (BS.index (scopes c!!scopeIndex c) 1)), removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 20 =  disassemble(s ++ " HASH "++ (show ((BS.index (scopes c!!scopeIndex c) 1)`div` 2)), removeFromScope(removeFromScope c))      
      | BS.head (scopes c!!scopeIndex c) == 21 = disassemble(s ++ " HASHEND", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 22 = disassemble(s ++ " INDEX", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 23 = disassemble(s ++ " SETINDEX", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 24 = disassemble(s ++ " OPCALL " ++ (show (BS.index (scopes c!!scopeIndex c) 1)), removeFromScope(removeFromScope c))
      | BS.head (scopes c!!scopeIndex c) == 25 = disassemble(s ++ " RETURNVALUE ", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 26 = disassemble(s ++ " OPRETURN ", removeFromScope c)
      | BS.head (scopes c!!scopeIndex c) == 27 = disassemble(s ++ " SETLOCAL "++ (show (BS.index (scopes c!!scopeIndex c) 1))++ (show (BS.index (scopes c!!scopeIndex c) 2)), removeFromScope(removeFromScope(removeFromScope c)))
      | BS.head (scopes c!!scopeIndex c) == 28 = disassemble(s ++ " GETLOCAL "++ (show (BS.index (scopes c!!scopeIndex c) 1))++ (show (BS.index (scopes c!!scopeIndex c) 2)), removeFromScope(removeFromScope(removeFromScope c)))
      | BS.head (scopes c!!scopeIndex c) == 29 = disassemble(s ++ " CALLPREBUILT "++ (show (BS.index (scopes c!!scopeIndex c) 1)), removeFromScope(removeFromScope c))
      | otherwise = error ("disassemble " ++ (show (BS.head (scopes c!!scopeIndex c))))

removeFromScope :: Compiler -> Compiler 
removeFromScope c = Compiler{
    symbols = symbols c, 
    scopes = (scopes c) & element (scopeIndex c) .~ (removeFirstBS (scopes c!!scopeIndex c)),
    scopeIndex = scopeIndex c 
  }

removeNFromScope :: (Int, Compiler) -> Compiler 
removeNFromScope (i, c) = comp 
  where 
    comp 
      | i == 0 = c 
      | otherwise = removeNFromScope(i-1, removeFromScope c)

removeFirstBS :: ByteString -> ByteString 
removeFirstBS b = 
  case BS.length b of 
    0 -> error "can't remove instruction of length 0?"
    1 -> BS.empty :: ByteString 
    _ -> pack(removeFirst(unpack b))

removeNInstructions :: (Int, [(Int, ByteString)], Int) -> [(Int, ByteString)]
removeNInstructions (i, b, idx) = bs 
  where 
    bs 
      | i == 0 = b 
      | otherwise = removeNInstructions(i-1, removeFirstInstruction (b, idx), idx)

removeFirstInstruction :: ([(Int, ByteString)], Int)-> [(Int, ByteString)]
removeFirstInstruction (b,i) = 
  case BS.length (snd (b!!i)) of 
    0 -> error "can't remove instruction of length 0?"
    1 -> b & element i .~ (fst (b!!i), BS.empty :: ByteString)
    _ -> b & element i .~ (fst (b!!i), pack(removeFirst(unpack (snd (b!!i)))))

getFirstNInstructions :: (Int, ByteString,(Int, ByteString)) -> ByteString 
getFirstNInstructions (i,new, old) = bs 
  where   
    bs 
      | i == 0 = new 
      | otherwise =getFirstNInstructions(i-1, new <> pack([BS.head (snd old)]), Prelude.head (removeFirstInstruction([old], 0)))

mergeLists :: [a] -> [a] -> [a]
mergeLists [] ys = ys 
mergeLists (x:xs) ys = x:mergeLists ys xs 

convertStringToBytes :: String -> BS.ByteString 
convertStringToBytes s = BSU.fromString s 

convertBytesToString :: BS.ByteString -> String 
convertBytesToString b = BSU.toString b
