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
          (EXP,( snd (parseTokens("fn five(){}", [])),
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
          (EXP,( snd (parseTokens("fn five(a, b){}", [])),
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
          (EXP,( snd (parseTokens("fn five(){return 5;}", [])),
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
          (EXP,( snd (parseTokens("fn five(a, b){return 5;}", [])),
            []
          )))
        )
      )
    )
testMultipleBodyFunc :: String 
testMultipleBodyFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){let five = 5; return five;}", [])),
            []
          )))
        )
      )
    )
testOperatorBodyFunc :: String 
testOperatorBodyFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){let five = 2 + 3;}", [])),
            []
          )))
        )
      )
    )
testGroupedOperatorBodyFunc :: String 
testGroupedOperatorBodyFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){let five = (2 + 3);}", [])),
            []
          )))
        )
      )
    )
testInfixBodyFunc :: String 
testInfixBodyFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){let five = -5;}", [])),
            []
          )))
        )
      )
    )
testBoolBodyFunc :: String 
testBoolBodyFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){let five = 5 == 5;}", [])),
            []
          )))
        )
      )
    )
testIfFunc :: String 
testIfFunc = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){if(5 == 5){let five = 5;}else{return 10;};};", [])),
            []
          )))
        )
      )
    )
testFuncCallInFunc:: String 
testFuncCallInFunc= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("fn five(){return five();};", [])),
            []
          )))
        )
      )
    )
testFuncCall :: String 
testFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("add(5);", [])),
            []
          )))
        )
      )
    )
testOpFuncCall :: String 
testOpFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("add(5 + 5);", [])),
            []
          )))
        )
      )
    )
testInfixFuncCall :: String 
testInfixFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("add(-5);", [])),
            []
          )))
        )
      )
    )
testGroupedOpFuncCall :: String 
testGroupedOpFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("add((5 + 3) * 2);", [])),
            []
          )))
        )
      )
    )
testLetWithFuncCall :: String 
testLetWithFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = five();", [])),
            []
          )))
        )
      )
    )
testReturnWithFuncCall :: String 
testReturnWithFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("return five();", [])),
            []
          )))
        )
      )
    )
testOperatorWithFuncCall :: String 
testOperatorWithFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = 2 + addThree();", [])),
            []
          )))
        )
      )
    )
testInfixWithFuncCall :: String 
testInfixWithFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = -five();", [])),
            []
          )))
        )
      )
    )
testBoolWithFuncCall :: String 
testBoolWithFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = five() == five();", [])),
            []
          )))
        )
      )
    )
testGroupedWithFuncCall :: String 
testGroupedWithFuncCall = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = (2 + addThree());", [])),
            []
          )))
        )
      )
    )
testFuncCallWithParam :: String 
testFuncCallWithParam  = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = addThree(a);", [])),
            []
          )))
        )
      )
    )
testFuncCallWithMulParams :: String 
testFuncCallWithMulParams = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = addThree(a,b);", [])),
            []
          )))
        )
      )
    )
