module TestParser where

import Ast
import Lexer
import Parser
import Token
import Utils


testBasicLet:: String 
testBasicLet= 
  statementsToString 
      (snd( snd 
        ( parseStatements
          (EXP,( getTokens(parseTokens(0, "let five = 5; let five = \"five\"; let five = 5 + 5 + 5 ; let five = 5 / 5; let five = 5 * 5; let five = 5 + 5 * 5; let five = 5 * 5 + 5; let five = 5 - 5; let five = -5;", [])),
            []
          ))
        ))
      )

testReturnStatement :: String 
testReturnStatement =
  statementsToString(snd(snd(parseStatements (EXP,(getTokens(parseTokens (0, "return 5; return 5 + 5;", [])), [])))))
  
