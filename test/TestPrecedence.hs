module TestPrecedence where

import Ast
import Token
import Parser 
import Utils 
import Lexer 

testPlusMinus :: Bool  
testPlusMinus =
  hasPrecedence (MINUS, PLUS)

testCallPlus :: Bool 
testCallPlus =
  hasPrecedence (FUNCTION, PLUS)

testBasicGrouped:: String 
testBasicGrouped=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("let five = (2 + 3) * 5", [])),
                  []
                ))
            )
        ))
    )

testAdvGrouped :: String 
testAdvGrouped =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("let five = (2 + -3) * -4 + (10 /5)", [])),
                  []
                ))
            )
        ))
    )

testBoolGrouped :: String 
testBoolGrouped =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("let five = (-3 + 5) + 2 == 8 / 5", [])),
                  []
                ))
            )
        ))
    )

testBoolGrouped2 :: String 
testBoolGrouped2 =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("let five = (5 + 2) * 5 > -4 / 1", [])),
                  []
                ))
            )
        ))
    )

testGrouped2 :: String 
testGrouped2 =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("let five = (2 + 2) * (5 * 2) / (-4 - -4)", [])),
                  []
                ))
            )
        ))
    )

test1:: String 
test1 =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( snd (parseTokens ("return ((1 + 2) + 3) * (4 * (5 + 6)) / ((-7 - 8))", [])),
                  []
                ))
            )
        ))
    )
test2:: String 
test2 =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP, (snd (parseTokens ("return ((1 + 2))", [])),
                  []
                ))
            )
        ))
    )
test3:: String 
test3 =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP, (snd (parseTokens ("let five = ((3 * 4 + 1) == (-13))", [])),
                  []
                ))
            )
        ))
    )
