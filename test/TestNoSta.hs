module TestNoSta where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 
import Eval
import Utils2

testEmpty:: String
testEmpty=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "5;", [])),
            []
          ))
        )
      ))

testIfEmpty:: String
testIfEmpty=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "if(True){5;}else{3;}; 2;", [])),
            []
          ))
        )
      ))

testFuncEmpty:: String
testFuncEmpty=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "fn add(a,b){5; return a + b;}; 10;", [])),
            []
          ))
        )
      ))
testForEmpty:: String
testForEmpty=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "for(i = 0; i < 5; i + 1;){5;}; 10;", [])),
            []
          ))
        )
      ))
