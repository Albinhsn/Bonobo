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
          (EXP,( getTokens(parseTokens(0, "let arr = [1,\"Hi\", (2+3), add(2,3), True, -1, 1 == 1];", [])),
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
testArrayIdxOp:: String
testArrayIdxOp=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = a[2 + 3];", [])),
            []
          ))
        )
      ))
testArrayIdxGroupedOp:: String
testArrayIdxGroupedOp=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = a[(2 + 3)];", [])),
            []
          ))
        )
      ))
testArrayIdxIdent:: String
testArrayIdxIdent=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = a[b];", [])),
            []
          ))
        )
      ))
testArrayIdxWierd:: String
testArrayIdxWierd=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let arr = a[((2 * b) + c[3])];", [])),
            []
          ))
        )
      ))
