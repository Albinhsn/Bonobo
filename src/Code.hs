module Code where 

import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU 
import Data.ByteString.Char8 as BSC
import Data.Word (Word8)
import Numeric as DN 
import Data.Map as DM
import Data.Binary 
import Data.Bits as DB
import Debug.Trace
import Object 
import Lexer
import Utils
import Ast


data OpCode = OPFOR | CALLPREBUILT | SETLOCAL | GETLOCAL | OPRETURN | OPRETURNVALUE | OPCALL | INDEXEND | SETINDEX | INDEX | HASHEND | HASH | ARRAYEND | ARRAY | GETGLOBAL | SETGLOBAL | JUMPNT | JUMP | OPMINUS | OPBANG | OPGT | OPNEQ | OPEQ | OPTRUE | OPFALSE | OPSUB | OPMUL | OPDIV | OPPOP | OPCONST | OPADD deriving (Eq, Show, Ord) 

data Scope = GLOBAL | LOCAL deriving (Eq, Show)

data Symbol = Symbol{
    symName :: !String, 
    symIndex :: !Int,
    symScope :: !Scope
  } deriving (Eq, Show)

opCodes = DM.fromList [
    (OPCONST ,fromIntegral 0 )
  , (OPPOP, fromIntegral 1)
  , (OPADD, fromIntegral 2)
  , (OPSUB, fromIntegral 3)
  , (OPMUL, fromIntegral 4)
  , (OPDIV, fromIntegral 5)
  , (OPTRUE, fromIntegral 6)
  , (OPFALSE, fromIntegral 7)
  , (OPGT, fromIntegral 8)
  , (OPNEQ, fromIntegral 10)
  , (OPEQ, fromIntegral 11)
  , (OPMINUS, fromIntegral 12)
  , (OPBANG, fromIntegral 13)
  , (JUMP, fromIntegral 14)
  , (JUMPNT, fromIntegral 15)
  , (SETGLOBAL, fromIntegral 16)
  , (GETGLOBAL, fromIntegral 17)
  , (ARRAY, fromIntegral 18)
  , (HASH, fromIntegral 20)
  , (INDEX, fromIntegral 22)
  , (SETINDEX, fromIntegral 23)
  , (OPCALL, fromIntegral 24)
  , (OPRETURNVALUE, fromIntegral 25)
  , (OPRETURN, fromIntegral 26)
  , (SETLOCAL, fromIntegral 27)
  , (GETLOCAL, fromIntegral 28)
  , (CALLPREBUILT, fromIntegral 29)
  , (OPFOR, fromIntegral 30)
  ]

isSymbolName :: (Int, [[Symbol]], String)-> Bool 
-- isSymbolName (idx, s, i) = error (show (idx) ++ " " ++ show (Prelude.length s))
isSymbolName (idx, s, i) = b 
  where 
    b
      | idx == 0 &&  Prelude.null [x | x <- s!!0, i == symName x] = False  
      | Prelude.null [x | x <- s!!idx, i == symName x] == False = True 
      | otherwise = isSymbolName(idx - 1, s, i)


getSymbolKey :: ([[Symbol]], String) -> Int
getSymbolKey (sym, s)= i 
  where 
    i 
      | isSymbolName(Prelude.length sym - 1, sym, s)== False = error ("Trying to access variable that doesn't exist: " ++ s)
      | otherwise = gsk(Prelude.length sym - 1, sym, s) 

gsk :: (Int, [[Symbol]], String)-> Int 
gsk (idx, s,str) = i
  where 
    i 
      | idx == 0 = Prelude.head [symIndex x | x <- s!!0, str == symName x]
      | Prelude.null [symIndex x | x <- s!!idx, str == symName x] = gsk(idx-1, s, str)
      | otherwise = Prelude.head [symIndex x | x <- s!!idx, str == symName x] 

data Compiler = Compiler{
    symbols :: [[Symbol]],
    scopes :: ![ByteString],
    scopeIndex :: !Int
  } deriving (Show)

    
isValidOpCode :: OpCode -> Bool
isValidOpCode o = DM.member o opCodes

lookupOpCode :: OpCode -> ByteString 
lookupOpCode o = BS.singleton (opCodes ! o)


bsToInt :: ByteString -> Int 
bsToInt b = (roll(0, b, 0))


roll :: (Int, ByteString, Int) -> Int 
roll (i,b, x) = int 
  where 
    int 
      | BS.null b = x 
      | i == 0 = roll(i+1,rmfBS b, x + (fromIntegral (BS.head b)))
      | otherwise = roll(i+1,rmfBS b, x + (fromIntegral(BS.head b)*  2^(8 * i)))

rmfBS :: ByteString -> ByteString 
rmfBS b = BS.pack(removeFirst(BS.unpack b))


reverseUnroll :: Int -> ByteString
reverseUnroll i = BS.reverse (unroll i)

chooseToUnroll :: Int -> ByteString 
chooseToUnroll i = 
  case i of 
    0 -> BS.singleton 0  
    _ -> unroll i 

unroll :: Int -> ByteString 
unroll = BS.unfoldr step
  where 
    step 0 = Nothing 
    step i = Just (fromIntegral i, i `shiftR` 8)

addPrebuilts :: [Symbol]
addPrebuilts = [
    Symbol{symIndex = 0, symName = "len", symScope = GLOBAL}
  , Symbol{symIndex = 1, symName = "print", symScope = GLOBAL}
  , Symbol{symIndex = 2, symName = "append", symScope = GLOBAL}
  ] 

