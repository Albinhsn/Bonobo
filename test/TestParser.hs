module TestParser where

import Ast
import Lexer
import Parser
import Token
import Utils


testPrefix:: String 
testPrefix= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = -5;", [])),
            []
          ))
        ))
      )
    )

testPlusAsteriskOperators :: String 
testPlusAsteriskOperators = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 + 5 * 5;", [])),
            []
          ))
        ))
      )
    )
testAsteriskPlusOperators :: String 
testAsteriskPlusOperators = 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5 * 5 + 5;", [])),
            []
          ))
        )
        )
      )
    )


testAssignment :: String 
testAssignment = 
   statementToString
      ( head
          (snd( snd
              ( parseStatements
                  (EXP,( getTokens(parseTokens (0, "let five = 5;", [])),
                    []
                  ))
              )
          ))
      )
        
    
testMultipleOperators :: String
testMultipleOperators =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( getTokens(parseTokens (0, "let five = 5 + 5 + 5 ;", [])),
                  []
                ))
           ) )
        )
      )
  
testSlashOperator :: String
testSlashOperator =
   statementToString
      ( head
          (snd( snd
              ( parseStatements
                  (EXP,( getTokens(parseTokens (0, "let five = 5 / 5;", [])),
                    []
                  ))
              )
          ))
      )
        
    
testAsteriskOperator :: String
testAsteriskOperator =
 statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( getTokens(parseTokens (0, "let five = 5 * 5;", [])),
                  []
           )     ))
            )
        )
    )
testMinusOperator :: String
testMinusOperator =
  statementToString
      ( head
          (snd( snd
              ( parseStatements
                  (EXP,( getTokens(parseTokens (0, "let five = 5 - 5;", [])),
                    []
                  ))
              )
          ))
      )

testPlusOperator :: String
testPlusOperator =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( getTokens(parseTokens (0, "let five = 5 + 5;", [])),
                  []
                ))
            )
        ))
    )

testReturnStatement :: String 
testReturnStatement =
  statementToString
      ( head
          (snd( snd
              (parseStatements (EXP,(getTokens(parseTokens (0, "return 5;", [])), [])))
          ))
      )

testArithmeticReturnStatement :: String 
testArithmeticReturnStatement =
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( getTokens(parseTokens (0, "return 5 + 5;", [])),
                  []
                ))
           ))
        )
      )
testLetString:: String 
testLetString=
  statementToString
    ( head
        (snd( snd
            ( parseStatements
                (EXP,( getTokens(parseTokens (0, "let five = \"five\";", [])),
                  []
                ))
           ))
        )
      )
