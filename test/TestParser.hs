module TestParser where

import Ast
import Lexer
import Parser
import Token
import Utils


testInfix :: String 
testInfix = 
  statementToString 
    ( head
      ( snd 
        ( parseStatements
          ( snd (parseTokens("let five = -5;", [])),
            []
          )
        )
      )
    )

testPlusAsteriskOperators :: String 
testPlusAsteriskOperators = 
  statementToString 
    ( head
      ( snd 
        ( parseStatements
          ( snd (parseTokens("let five = 5 + 5 * 5;", [])),
            []
          )
        )
      )
    )
testAsteriskPlusOperators :: String 
testAsteriskPlusOperators = 
  statementToString 
    ( head
      ( snd 
        ( parseStatements
          ( snd (parseTokens("let five = 5 * 5 + 5;", [])),
            []
          )
        )
      )
    )


testAssignment :: String 
testAssignment = 
   statementToString
      ( head
          ( snd
              ( parseStatements
                  ( snd (parseTokens ("let five = 5;", [])),
                    []
                  )
              )
          )
      )
        
    
testMultipleOperators :: String
testMultipleOperators =
  statementToString
    ( head
        ( snd
            ( parseStatements
                ( snd (parseTokens ("let five = 5 + 5 + 5 ;", [])),
                  []
                )
            )
        )
      )
  
testSlashOperator :: String
testSlashOperator =
   statementToString
      ( head
          ( snd
              ( parseStatements
                  ( snd (parseTokens ("let five = 5 / 5;", [])),
                    []
                  )
              )
          )
      )
        
    
testAsteriskOperator :: String
testAsteriskOperator =
 statementToString
    ( head
        ( snd
            ( parseStatements
                ( snd (parseTokens ("let five = 5 * 5;", [])),
                  []
                )
            )
        )
    )
testMinusOperator :: String
testMinusOperator =
  statementToString
      ( head
          ( snd
              ( parseStatements
                  ( snd (parseTokens ("let five = 5 - 5;", [])),
                    []
                  )
              )
          )
      )

testPlusOperator :: String
testPlusOperator =
  statementToString
    ( head
        ( snd
            ( parseStatements
                ( snd (parseTokens ("let five = 5 + 5;", [])),
                  []
                )
            )
        )
    )

testReturnStatement :: String 
testReturnStatement =
  statementToString
      ( head
          ( snd
              (parseStatements (snd (parseTokens ("return 5;", [])), []))
          )
      )

testArithmeticReturnStatement :: String 
testArithmeticReturnStatement =
  statementToString
    ( head
        ( snd
            ( parseStatements
                ( snd (parseTokens ("return 5 + 5;", [])),
                  []
                )
            )
        )
    )
