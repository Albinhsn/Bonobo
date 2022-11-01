module TestMap where 


import Parser
import Ast 
import Lexer 
import Token 
import Utils 
import Eval
import TestUtils

testMap:: String
testMap=
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let m = {x:x, 1:1, \"a\":\"a\", 1+2:1+2, (1/2):(1/2), a:True}", [])),
            []
          ))
        )
      ))
    )
testMapFunc:: String
testMapFunc=
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "fn add(){let m = {x:x, 1:1, \"a\":\"a\", 1+2:1+2, (1/2):(1/2), a:True}}", [])),
            []
          ))
        )
      ))
    )

testEvalMapFunc:: String
testEvalMapFunc= concatContext(snd(evaluateProgram(parseStringToStatements "fn add(a,b){let c = {1:3, 2:4}; return c[a] + c[b]}; let a = add(1,2);", ([], []))))

testEvalMap:: String
testEvalMap= concatContext(snd(evaluateProgram(parseStringToStatements "let x = 0; let a = 1; let m = {x:x, 1:1, \"a\":\"a\", 1+2:1+2, (2/1):(2/1), a:True};", ([], []))))

testEvalMapFuncIf:: String
testEvalMapFuncIf= concatContext(snd(evaluateProgram(parseStringToStatements "fn a(b){if(b > 1){let c = {3:4}; return c[b];}else{let c = {0:5}; return c[b];};}; let d = a(3);", ([], []))))

testEvalMapFuncElse:: String
testEvalMapFuncElse= concatContext(snd(evaluateProgram(parseStringToStatements "fn a(b){if(b > 1){let c = {3:4}; return c[b];}else{let c = {0:5}; return c[b];};}; let d = a(0);", ([], []))))

testEvalMapAssign:: String
testEvalMapAssign= concatContext(snd(evaluateProgram(parseStringToStatements "let a = {1:1, 2:2, \"3\":3}; a[\"3\"] = \"4\"; a[\"k\"] = True; a[1] = (2 + 3) * 2", ([], []))))
