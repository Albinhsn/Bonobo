module TestMultiple where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 

testMultipleLet:: String
testMultipleLet=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5; let ten = 10;", [])),
            []
          ))
        )))
testMultipleIdent:: String
testMultipleIdent=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5; let two = 2; five = (two * five) / two;", [])),
            []
          ))
        )
      ))
testMultipleFunc :: String
testMultipleFunc =
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "fn double(numb){if(numb < 0){return -2 * numb;}else{return numb * 2;}}; fn divBy(div, numb){return numb / div;}", [])),
            []
          ))
        )
      ))
testFnDecCall:: String
testFnDecCall=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "fn isGTF(numb){return numb > 5}; let ten = 10; let gtf = isGTF(ten);", [])),
            []
          ))
        )
      ))
testFnCall:: String
testFnCall=
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "fn add (a,b){return a + b}; let five = add(2,3);", [])),
            []
          ))
        )
      ))
