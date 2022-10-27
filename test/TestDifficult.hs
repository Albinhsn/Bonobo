module TestDifficult where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 

testDiffFunc:: String
testDiffFunc=
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){if(5 == 5){let five = 5; five = five - 2; return five + 3;};};", [])),
            []
          ))
        )
      ))
    )
testDiffIf:: String
testDiffIf=
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("if(){if(){}else{if(){};};}else{if(){if(){if(){}else{return 5;};};};};", [])),
            []
          ))
        )
      ))
    )
