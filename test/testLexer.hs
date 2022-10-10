module Test where

import Lexer
import Test.HUnit
import Token
import Utils

testActualNumbers =
  TestCase
    ( assertEqual
        "testing actual code with numbers from the book"
        [ "let",
          "five",
          "=",
          "5",
          ";",
          "let",
          "ten",
          "=",
          "10",
          ";",
          "let",
          "add",
          "=",
          "fn",
          "(",
          "five",
          ",",
          "ten",
          ")",
          "{",
          "return",
          "x",
          "+",
          "y",
          ";",
          "}",
          ";",
          "let",
          "result",
          "=",
          "add",
          "(",
          "five",
          ",",
          "ten",
          ")",
          ";",
          "!",
          "-",
          "/",
          "*",
          "5",
          ";",
          "5",
          "<",
          "10",
          ">",
          "5",
          ";",
          "if",
          "(",
          "5",
          "<",
          "10",
          ")",
          "{",
          "return",
          "true",
          ";",
          "}",
          "else",
          "{",
          "return",
          "false",
          ";",
          "}",
          "10",
          "==",
          "10",
          ";",
          "10",
          "!=",
          "9",
          ";",
          "EOF"
        ]
        [ tokenToString t
          | t <-
              snd
                ( parseTokens
                    ("let five = 5;\nlet ten = 10;\nlet add = fn(five, ten){\nreturn x + y;\n};\n\nlet result = add(five, ten);\n!-/*5;\n5 < 10 > 5;\n\nif (5 < 10){\n return true;\n}else {\n return false;\n}\n 10 == 10;\n10 != 9;", [])
                )
        ]
    )

testActual =
  TestCase
    ( assertEqual
        "test actual code"
        ["let", "five", "=", ";", "let", "ten", "=", ";", "let", "add", "=", "fn", "(", "x", ",", "y", ")", "{", "x", "+", "y", ";", "}", ";", "let", "result", "=", "add", "(", "five", ",", "ten", ")", ";", "EOF"]
        [tokenToString t | t <- snd (parseTokens ("let five = ;\nlet ten = ;\nlet add = fn(x, y) {x + y;};\nlet result = add(five, ten);", []))]
    )

tests = TestList [testActual, testActualNumbers]

main = runTestTT tests
