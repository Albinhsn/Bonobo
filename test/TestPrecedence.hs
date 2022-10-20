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
        ( snd
            ( parseStatements
                ( snd (parseTokens ("let five = (2 + 3) * 5", [])),
                  []
                )
            )
        )
    )

testAdvGrouped :: String 
testAdvGrouped =
  statementToString
    ( head
        ( snd
            ( parseStatements
                ( snd (parseTokens ("let five = (2 + -3) * -4 + (10 /5)", [])),
                  []
                )
            )
        )
    )

