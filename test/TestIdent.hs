module TestIdent where 
import Lexer 
import Ast
import Parser 
import Utils


testIdent:: String 
testIdent= 
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "five = 5; five = 5 * 5; five = 5 == 5; five = -5; five = (5 + 5); let five = 2 + three; let five = three; let five = -five; let five = (three + two); let five = three == 3; ", [])),
            []
          )))
        )
      )
testLetGroupedIdent:: String 
testLetGroupedIdent= 
  statementToString 
    ( head
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "", [])),
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
