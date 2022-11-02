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

testGroupedPrecedence:: String 
testGroupedPrecedence=
  statementsToString
        (snd( snd
            ( parseStatements
                (EXP,( getTokens(parseTokens (0, "let five = (2 + 3) * 5; let five = (2 + -3) * -4 + (10 /5); let five = (-3 + 5) + 2 == 8 / 5; let five = (5 + 2) * 5 > -4 / 1; let five = (2 + 2) * (5 * 2) / (-4 - -4); return ((1 + 2) + 3) * (4 * (5 + 6)) / ((-7 - 8)); return ((1 + 2)); let five = ((3 * 4 + 1) == (-13));", [])),
                  []
                ))
            )
        ))
