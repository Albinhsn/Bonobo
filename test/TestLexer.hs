module TestLexer where

import Lexer
import Utils

testActualNumbers :: [String]
testActualNumbers =
  [ tokenToString t
    | t <-
        getTokens 
          ( parseTokens
              (0, "let five = 5;\nlet ten = 10;\nlet add = fn(five, ten){\nreturn x + y;\n};\n\nlet result = add(five, ten);\n!-/*5;\n5 < 10 > 5;\n\nif (5 < 10){\n return true;\n}else {\n return false;\n}\n 10 == 10;\n10 != 9;", [])
          )
  ]

testActual :: [String]
testActual =
  [tokenToString t | t <- getTokens(parseTokens (0, "let five = ;\nlet ten = ;\nlet add = fn(x, y) {x + y;};\nlet result = add(five, ten);", []))]
