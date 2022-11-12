module TestDifficult where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 
import Eval
import Utils2

testDiffFunc:: String
testDiffFunc=
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "fn five(){if(5 == 5){let five = 5; five = five - 2; return five + 3;};};", [])),
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
          (EXP,( getTokens(parseTokens(0, "if(){if(){}else{if(){};};}else{if(){if(){if(){}else{five = 5;};};};};", [])),
            []
          ))
        )
      ))
    )
testInsaneIf:: String
testInsaneIf=
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "if(){if(){if(){five = 5;};}else{if(){five = 5;}else{five = 5;};};}else{if(){if(){if(){}else{five = 5;};}else{if(){}else{if(){}else{five = 5;};};};};};", [])),
            []
          ))
        )
      ))
    )

testEvalMapArrayComb :: String
testEvalMapArrayComb = concatContext(snd(evaluateProgram(parseStringToStatements "let a = [{0:[{0:[1]}]},1]; a[0][0][0][0][0] = True;", ([], []))))

