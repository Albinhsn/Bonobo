module TestFunc where 

import Token 
import Lexer 
import Ast
import Parser 
import Utils


testFunc :: String 
testFunc = 
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "fn five(){}; fn five(a, b){}; fn five(){return 5;}; fn five(a, b){return 5;}; fn five(){let five = 5; return five;}; fn five(){let five = 2 + 3;}; fn five(){let five = (2 + 3);}; fn five(){let five = -5;}; fn five(){let five = 5 == 5;}; fn five(){if(5 == 5){let five = 5;}else{return 10;};}; fn five(){return five();}; add(5); add(5 + 5); add(-5); add((5 + 3) * 2); let five = five(); return five(); let five = 2 + addThree(); let five = -five(); let five = five() == five();", [])),
            []
          )))
        )
      )
testCall :: String 
testCall = 
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = (2 + addThree()); let five = addThree(a); let five = addThree(a,b);", [])),
            []
          )))
        )
      )
