module TestParser where

import Ast
import Lexer
import Parser
import Test.HUnit
import Token

testAssignment =
  TestCase
    ( assertEqual
        "testing 'let five = 5;'"
        ([], [LetStatement {identifier = "five", expression = IntegerLiteralExpression {integerLiteral = 5}}])
        ( parseStatements
            ( snd (parseTokens ("let five = 5;", [])),
              []
            )
        )
    )

testMultipleOperators =
  TestCase
    ( assertEqual
        "testing 'let five = 5 + 5 + 5;'"
        ( [],
          [ LetStatement
              { identifier = "five",
                expression =
                  OperatorExpression
                    { leftOperator = IntegerLiteralExpression {integerLiteral = 5},
                      operator = Token {typ = PLUS, literal = "+"},
                      rightOperator = IntegerLiteralExpression {integerLiteral = 5}
                    }
              }
          ]
        )
        ( parseStatements
            ( snd (parseTokens ("let five = 5 + 5 + 5 ;", [])),
              []
            )
        )
    )

testSlashOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 / 5;'"
        ( [],
          [ LetStatement
              { identifier = "five",
                expression =
                  OperatorExpression
                    { leftOperator = IntegerLiteralExpression {integerLiteral = 5},
                      operator = Token {typ = SLASH, literal = "/"},
                      rightOperator = IntegerLiteralExpression {integerLiteral = 5}
                    }
              }
          ]
        )
        ( parseStatements
            ( snd (parseTokens ("let five = 5 / 5;", [])),
              []
            )
        )
    )

testAsteriskOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 * 5;'"
        ( [],
          [ LetStatement
              { identifier = "five",
                expression =
                  OperatorExpression
                    { leftOperator = IntegerLiteralExpression {integerLiteral = 5},
                      operator = Token {typ = ASTERISK, literal = "*"},
                      rightOperator = IntegerLiteralExpression {integerLiteral = 5}
                    }
              }
          ]
        )
        ( parseStatements
            ( snd (parseTokens ("let five = 5 * 5;", [])),
              []
            )
        )
    )

testMinusOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 - 5;'"
        ( [],
          [ LetStatement
              { identifier = "five",
                expression =
                  OperatorExpression
                    { leftOperator = IntegerLiteralExpression {integerLiteral = 5},
                      operator = Token {typ = MINUS, literal = "-"},
                      rightOperator = IntegerLiteralExpression {integerLiteral = 5}
                    }
              }
          ]
        )
        ( parseStatements
            ( snd (parseTokens ("let five = 5 - 5;", [])),
              []
            )
        )
    )

testPlusOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 + 5;'"
        ( [],
          [ LetStatement
              { identifier = "five",
                expression =
                  OperatorExpression
                    { leftOperator = IntegerLiteralExpression {integerLiteral = 5},
                      operator = Token {typ = PLUS, literal = "+"},
                      rightOperator = IntegerLiteralExpression {integerLiteral = 5}
                    }
              }
          ]
        )
        ( parseStatements
            ( snd (parseTokens ("let five = 5 + 5;", [])),
              []
            )
        )
    )

tests = TestList [testPlusOperator, testMinusOperator, testSlashOperator, testAsteriskOperator, testAssignment]

main = runTestTT tests
