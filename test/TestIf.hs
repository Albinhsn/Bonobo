module TestIf where 

import Ast
import Token
import Parser 
import Utils 
import Lexer 

testEmptyIf:: String 
testEmptyIf=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){}", [])),
                  []
                ))
            )
        ))
    )
testEmptyIfElse:: String 
testEmptyIfElse=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){} else {}", [])),
                  []
                ))
            )
        ))
    )
testCon:: String 
testCon=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){let five = 5;} else {}", [])),
                  []
                ))
            )
        ))
    )
testAlt:: String 
testAlt=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){} else {let five = 5;}", [])),
                  []
                ))
            )
        ))
    )
testMultipleCon:: String 
testMultipleCon=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){let five = 5; let ten = 5 + 5;} else {}", [])),
                  []
                ))
            )
        ))
    )
testMultipleAlt:: String 
testMultipleAlt=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){} else {let five = 5; let ten = 5 + 5;}", [])),
                  []
                ))
            )
        ))
    )
testConAlt:: String 
testConAlt=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){let five = 5;} else {let five = 5;}", [])),
                  []
                ))
            )
        ))
    )
testMultipleConAlt:: String 
testMultipleConAlt=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(){let five = 5; let ten = 10;} else {let five = 5; let ten = 10;}", [])),
                  []
                ))
            )
        ))
    )
testMultipleIf:: String 
testMultipleIf=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(5 == 5){if(5 == 5){let five = 5;}else{return 5;}}", [])),
                  []
                ))
            )
        ))
    )
testIfParam:: String 
testIfParam=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("if(5 == 5){return 5;}", [])),
                  []
                ))
            )
        ))
    )
