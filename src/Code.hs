module Code where 

import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU 
import Data.Word (Word8)
import Data.Map as DM
import Data.Binary 
import Data.Bits
import Numeric (showHex)
import Object 
import Ast


data OpCode = INDEXEND | SETINDEX | INDEX | HASHEND | HASH | ARRAYEND | ARRAY | GETGLOBAL | SETGLOBAL | JUMPNT | JUMP | OPMINUS | OPBANG | OPGT | OPNEQ | OPEQ | OPTRUE | OPFALSE | OPSUB | OPMUL | OPDIV | OPPOP | OPCONST | OPADD deriving (Eq, Show, Ord) 


data Symbol = Symbol{
    symName :: !String, 
    symIndex :: !Integer
  }

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
  , (ARRAYEND, fromIntegral 19)
  , (HASH, fromIntegral 20)
  , (HASHEND, fromIntegral 21)
  , (INDEX, fromIntegral 22)
  , (SETINDEX, fromIntegral 23)
  ]

data Compiler = Compiler{
    symbols :: [(String, Int)],
    bytes :: ByteString,
    constants :: [Object]
  }

    
isValidOpCode :: OpCode -> Bool
isValidOpCode o = DM.member o opCodes

lookupOpCode :: OpCode -> ByteString 
lookupOpCode o = BS.singleton (opCodes ! o)


prettyPrint :: BS.ByteString -> String
prettyPrint = BS.foldr showHex ""


reverseUnroll :: Int -> ByteString
reverseUnroll i = BS.reverse (unroll i)



chooseToUnroll :: Int -> ByteString 
chooseToUnroll i = 
  case i of 
    0 -> BS.singleton 0  
    _ -> unroll i 

unroll :: Int -> ByteString 
unroll = unfoldr step
  where 
    step 0 = Nothing 
    step i = Just (fromIntegral i, i `shiftR` 8)

make :: (OpCode, Int) -> ByteString 
make (o, i) = b 
  where 
    b 
      | isValidOpCode o == False = error "opcode doesn't exist"
      | otherwise = (lookupOpCode o) <> (chooseToUnroll i) 

