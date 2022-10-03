module Test where

import Lexer
import Test.HUnit
import Token

testActualNumbers =
  TestCase
    ( assertEqual
        "testing actual code with numbers from the book"
        ( "",
          [ Token {typ = LET, literal = "let"},
            Token {typ = IDENT, literal = "five"},
            Token {typ = ASSIGN, literal = "="},
            Token {typ = INT, literal = "5"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = LET, literal = "let"},
            Token {typ = IDENT, literal = "ten"},
            Token {typ = ASSIGN, literal = "="},
            Token {typ = INT, literal = "10"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = LET, literal = "let"},
            Token {typ = IDENT, literal = "add"},
            Token {typ = ASSIGN, literal = "="},
            Token {typ = FUNCTION, literal = "fn"},
            Token {typ = LPAREN, literal = "("},
            Token {typ = IDENT, literal = "five"},
            Token {typ = COMMA, literal = ","},
            Token {typ = IDENT, literal = "ten"},
            Token {typ = RPAREN, literal = ")"},
            Token {typ = LBRACE, literal = "{"},
            Token {typ = RETURN, literal = "return"},
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
            Token {typ = BANG, literal = "!"},
            Token {typ = MINUS, literal = "-"},
            Token {typ = SLASH, literal = "/"},
            Token {typ = ASTERISK, literal = "*"},
            Token {typ = INT, literal = "5"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = INT, literal = "5"},
            Token {typ = LESS_T, literal = "<"},
            Token {typ = INT, literal = "10"},
            Token {typ = GREATER_T, literal = ">"},
            Token {typ = INT, literal = "5"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = IF, literal = "if"},
            Token {typ = LPAREN, literal = "("},
            Token {typ = INT, literal = "5"},
            Token {typ = LESS_T, literal = "<"},
            Token {typ = INT, literal = "10"},
            Token {typ = RPAREN, literal = ")"},
            Token {typ = LBRACE, literal = "{"},
            Token {typ = RETURN, literal = "return"},
            Token {typ = TRUE, literal = "true"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = RBRACE, literal = "}"},
            Token {typ = ELSE, literal = "else"},
            Token {typ = LBRACE, literal = "{"},
            Token {typ = RETURN, literal = "return"},
            Token {typ = FALSE, literal = "false"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = RBRACE, literal = "}"},
            Token {typ = INT, literal = "10"},
            Token {typ = EQUALS, literal = "=="},
            Token {typ = INT, literal = "10"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = INT, literal = "10"},
            Token {typ = NOT_EQUALS, literal = "!="},
            Token {typ = INT, literal = "9"},
            Token {typ = SEMICOLON, literal = ";"},
            Token {typ = EOF, literal = "EOF"}
          ]
        )
        (parseTokens ("let five = 5;\nlet ten = 10;\nlet add = fn(five, ten) {\nreturn x + y;\n};\n\nlet result = add(five, ten);\n!-/*5;\n5 < 10 > 5;\n\nif (5 < 10){\n return true;\n}else {\n return false;\n}\n 10 == 10;\n10 != 9;", []))
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
        (parseTokens ("let five = ;\nlet ten = ;\nlet add = fn(x, y) {x + y;};\nlet result = add(five, ten);", []))
    )

tests = TestList [testActual, testActualNumbers]

main = runTestTT tests
