module TestIdent where 

import Token 
import Lexer 
import Ast
import Parser 
import Utils


testIdent:: String 
testIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("five = 5;", [])),
            []
          )))
        )
      )
    )
testOpIdent:: String 
testOpIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("five = 5 * 5;", [])),
            []
          )))
        )
      )
    )
testBoolIdent:: String 
testBoolIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("five = 5 == 5;", [])),
            []
          )))
        )
      )
    )
testInfixIdent:: String 
testInfixIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("five = -5;", [])),
            []
          )))
        )
      )
    )
testGroupedIdent:: String 
testGroupedIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("five = (5 + 5);", [])),
            []
          )))
        )
      )
    )
