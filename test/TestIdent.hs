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
testLetOpIdent:: String 
testLetOpIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = 2 + three;", [])),
            []
          )))
        )
      )
    )
testLetAssignIdent:: String 
testLetAssignIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = three;", [])),
            []
          )))
        )
      )
    )
testLetGroupedIdent:: String 
testLetGroupedIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = (three + two);", [])),
            []
          )))
        )
      )
    )
testLetInfixIdent:: String 
testLetInfixIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = -five", [])),
            []
          )))
        )
      )
    )
testLetBoolIdent:: String 
testLetBoolIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( snd (parseTokens("let five = three == 3", [])),
            []
          )))
        )
      )
    )
