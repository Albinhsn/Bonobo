module TestTFT where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 

testLetTrue :: String
testLetTrue =
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = true;", [])),
            []
          ))
        )
      ))
    )
