module Test where

import Lexer
import Test.HUnit
import Token

testParseStringToToken =
  TestCase
    ( assertEqual
        "testing token parsing"
        [ Token {typ = ASSIGN, literal = "="},
          Token {typ = PLUS, literal = "+"},
          Token {typ = COMMA, literal = ","},
          Token {typ = SEMICOLON, literal = ";"},
          Token {typ = LPAREN, literal = "("},
          Token {typ = RPAREN, literal = ")"},
          Token {typ = LBRACE, literal = "{"},
          Token {typ = RBRACE, literal = "}"},
          Token {typ = EOF, literal = "EOF"}
        ]
        (parseStringToToken "=+,;(){} ")
    )

tests = TestList [testParseStringToToken]

main = runTestTT tests
