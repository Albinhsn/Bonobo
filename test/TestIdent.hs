module TestIdent where 
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
          (EXP,( getTokens(parseTokens(0, "five = 5;", [])),
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
          (EXP,( getTokens(parseTokens(0, "five = 5 * 5;", [])),
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
          (EXP,( getTokens(parseTokens(0, "five = 5 == 5;", [])),
            []
          )))
        )
      )
    )
testPrefixIdent:: String 
testPrefixIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "five = -5;", [])),
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
          (EXP,( getTokens(parseTokens(0, "five = (5 + 5);", [])),
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
          (EXP,( getTokens(parseTokens(0, "let five = 2 + three;", [])),
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
          (EXP,( getTokens(parseTokens(0, "let five = three;", [])),
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
          (EXP,( getTokens(parseTokens(0, "let five = (three + two);", [])),
            []
          )))
        )
      )
    )
testLetPrefixIdent:: String 
testLetPrefixIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = -five", [])),
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
          (EXP,( getTokens(parseTokens(0, "let five = three == 3", [])),
            []
          )))
        )
      )
    )
