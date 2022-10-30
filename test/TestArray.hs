module TestArray where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 

testArray:: String
testArray=
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = [1,\"Hi\", (2+3), add(2,3), True]", [])),
            []
          ))
        )
      ))
    )
testArrayLet:: String
testArrayLet=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = [1,\"Hi\", (2+3), add(2,3), True]; let five = 5;", [])),
            []
          ))
        )
      ))
testArrayIdxInArr:: String
testArrayIdxInArr=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = [a[2], 1];", [])),
            []
          ))
        )
      ))
testArrayIdx:: String
testArrayIdx=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = 2 + a[2];", [])),
            []
          ))
        )
      ))
testArrayIdxGrouped:: String
testArrayIdxGrouped =
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = (2 + a[2]);", [])),
            []
          ))
        )
      ))
