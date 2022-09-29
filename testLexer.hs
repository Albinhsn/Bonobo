module Test where

import Lexer
import Test.HUnit
import Token

testParseSpecials =
  TestCase
    ( assertEqual
        "testing special chars"
        ( "",
          [ Token {typ = ASSIGN, literal = "="},
            Token {typ = PLUS, literal = "+"},
            Token {typ = COMMA, literal = ","},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = LPAREN, literal = "("},
            Token {typ = RPAREN, literal = ")"},
            Token {typ = LBRACE, literal = "{"},
            Token {typ = RBRACE, literal = "}"},
            Token {typ = ILLEGAL, literal = "ILLEGAL"},
            Token {typ = EOF, literal = "EOF"}
          ]
        )
        (parse ("=+,;(){}!", []))
    )

testActual =
  TestCase
    ( assertEqual
        "test actual code"
        ( "",
          [ Token {typ = LET, literal = "let"},
            Token {typ = IDENT, literal = "five"},
            Token {typ = ASSIGN, literal = "="},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = LET, literal = "let"},
            Token {typ = IDENT, literal = "ten"},
            Token {typ = ASSIGN, literal = "="},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = LET, literal = "let"},
            Token {typ = IDENT, literal = "add"},
            Token {typ = ASSIGN, literal = "="},
            Token {typ = FUNCTION, literal = "fn"},
            Token {typ = LPAREN, literal = "("},
            Token {typ = IDENT, literal = "x"},
            Token {typ = COMMA, literal = ","},
            Token {typ = IDENT, literal = "y"},
            Token {typ = RPAREN, literal = ")"},
            Token {typ = LBRACE, literal = "{"},
            Token {typ = IDENT, literal = "x"},
            Token {typ = PLUS, literal = "+"},
            Token {typ = IDENT, literal = "y"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = RBRACE, literal = "}"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = LET, literal = "let"},
            Token {typ = IDENT, literal = "result"},
            Token {typ = ASSIGN, literal = "="},
            Token {typ = IDENT, literal = "add"},
            Token {typ = LPAREN, literal = "("},
            Token {typ = IDENT, literal = "five"},
            Token {typ = COMMA, literal = ","},
            Token {typ = IDENT, literal = "ten"},
            Token {typ = RPAREN, literal = ")"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = EOF, literal = "EOF"}
          ]
        )
        (parse ("let five = ;\nlet ten = ;\nlet add = fn(x, y) {x + y;};\nlet result = add(five, ten);", []))
    )

testNormalString =
  TestCase
    ( assertEqual
        "testing normal string"
        ( "",
          [ Token {typ = IDENT, literal = "arlaharen"},
            Token {typ = EOF, literal = "EOF"}
          ]
        )
        (parse ("arlaharen", []))
    )

testFunction =
  TestCase
    ( assertEqual
        "testing function token"
        ( "",
          [ Token {typ = FUNCTION, literal = "fn"},
            Token {typ = EOF, literal = "EOF"}
          ]
        )
        (parse ("fn", []))
    )

testLet =
  TestCase
    ( assertEqual
        "testing let"
        ( "",
          [ Token {typ = LET, literal = "let"},
            Token {typ = EOF, literal = "EOF"}
          ]
        )
        (parse ("let", []))
    )

tests = TestList [testParseSpecials, testNormalString, testLet, testFunction, testActual]

main = runTestTT tests
