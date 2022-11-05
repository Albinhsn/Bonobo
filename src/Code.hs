module Code where 

import Data.ByteString.Lazy.Internal
import Data.Map

newtype Instructions = Instructions [ByteString]

newtype OpCode = OpCode ByteString

data TranslatedOpCode = TranslatedOpCode{strName :: !String, opCode :: !OpCode}

data OpCodes = OPCONST

data Definition = Definition {defName :: !String, operandWidths :: ![Int]}

definitions = [
  ("OpConstant", 1)
  ]

getDefinition :: ByteString -> Definition 
getDefinition b = d 
  where 
    d 
      | otherwise = 
    



