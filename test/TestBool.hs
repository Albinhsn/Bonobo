module TestBool where

import Parser
import Ast 
import Lexer 
import Token 
import Utils 

testBools:: String
testBools=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 == 5; let five = 5 == 5 + 5; let five = 5 == 5 + 5 * 5; let five = 5 == 5 + -5; let five = 5 == 5 + -5 * -5; let five = 5 > 5 * 2 == 2 + -3 * 5 > 1; let five = b[0] == b[0];", [])),
            []
          ))
        )
      ))
