module TestBool where

import Parser
import Ast 
import Lexer 
import Token 
import Utils 

testFiveEqualsFive :: String
testFiveEqualsFive =
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 == 5;", [])),
            []
          ))
        )
      ))
    )

testFiveEqualsFivePlusFive :: String
testFiveEqualsFivePlusFive = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 == 5 + 5;", [])),
            []
         )))
        )
      )
    )
testFiveEqualsFivePlusFiveTimesFive :: String
testFiveEqualsFivePlusFiveTimesFive = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 == 5 + 5 * 5;", [])),
            []
          ))
        ))
      )
    )
testFiveEqualsFivePlusMinusFive:: String
testFiveEqualsFivePlusMinusFive = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 == 5 + -5;", [])),
            []
         )))
        )
      )
    )
testFiveEqualsFivePlusMinusFiveTimesMinusFive:: String
testFiveEqualsFivePlusMinusFiveTimesMinusFive = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 == 5 + -5 * -5;", [])),
            []
          ))
        )
      ))
    )

testMultipleBools :: String 
testMultipleBools = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 > 5 * 2 == 2 + -3 * 5 > 1;", [])),
            []
          )))
        )
      )
    )
testBoolArrayAssign:: String 
testBoolArrayAssign= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = b[0] == b[0]", [])),
            []
          )))
        )
      )
    )
