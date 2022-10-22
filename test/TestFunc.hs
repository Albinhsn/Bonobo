module TestFunc where 

import Token 
import Lexer 
import Ast
import Parser 
import Utils


testEmptyFunc :: String 
testEmptyFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("func(){};", [])),
            []
          )))
        )
      )
    )
testParamFunc :: String 
testParamFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("func(a, b){};", [])),
            []
          )))
        )
      )
    )
testReturnFunc :: String 
testReturnFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("func(){return 5;};", [])),
            []
          )))
        )
      )
    )
testParamReturnFunc :: String 
testParamReturnFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("func(a, b){return 5;};", [])),
            []
          )))
        )
      )
    )
