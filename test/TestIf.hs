module TestIf where 

import Ast
import Token
import Parser 
import Utils 
import Lexer 

testIf:: String 
testIf=
  statementsToString
        (snd( snd
            ( parseStatements
                (EXP,( getTokens(parseTokens (0, "if(){}; if(){}else {}; if(){let five = 5;}else {}; if(){}else {let five = 5;}; if(){let five = 5; let ten = 5 + 5;} else {}; if(){} else {let five = 5; let ten = 5 + 5;}; if(){let five = 5;} else {let five = 5;}; if(){let five = 5; let ten = 10;} else {let five = 5; let ten = 10;}; if(5 == 5){if(5 == 5){let five = 5;}else{five = 5;};}; if(5 == 5){return 5;};", [])),
                  []
                ))
            )
        ))
