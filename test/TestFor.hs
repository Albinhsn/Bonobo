module TestFor where 


import Parser
import Ast 
import Lexer 
import Token 
import Utils 
import Eval
import Utils2

testFor:: String
testFor=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "for(i = 0; i < 5; i + 1;){k = k + i}", [])),
            []
          ))
        )
      ))

testForEval:: String
testForEval= concatContext(snd(evaluateProgram(parseStringToStatements "let k = 0; for(i = 0; i < 5; i + 1;){k = k + i}", ([], []))))
