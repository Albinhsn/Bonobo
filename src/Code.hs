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

data OpCode = SUB | MUL | DIV | POP | CONST | ADD deriving (Eq, Show, Ord) 

opCodes = DM.fromList [
    (CONST ,fromIntegral 0 )
  , (POP, fromIntegral 1)
  , (ADD, fromIntegral 2)
  , (SUB, fromIntegral 3)
  , (MUL, fromIntegral 4)
  , (DIV, fromIntegral 5)
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

