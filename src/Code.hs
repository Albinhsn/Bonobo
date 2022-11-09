module Code where 

import Data.ByteString as BS
import Data.ByteString.UTF8 as BSU 
import Data.Word (Word8)
import Data.Map as DM
import Data.Binary 
import Data.Bits
import Numeric (showHex)

-- OpCode CheatSheet: 
-- OpConstant : 0

data OpCode = OPGT | OPLT | OPNEQ | OPEQ | OPTRUE | OPFALSE | OPSUB | OPMUL | OPDIV | OPPOP | OPCONST | OPADD deriving (Eq, Show, Ord) 

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
  , (OPLT, fromIntegral 9)
  , (OPNEQ, fromIntegral 10)
  , (OPEQ, fromIntegral 11)
  ]


    
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

