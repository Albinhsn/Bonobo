module TestParser where

import Ast
import Lexer
import Parser
import Test.HUnit
import Token
import Utils

testAssignment =
  TestCase
    ( assertEqual
        "testing 'let five = 5;'"
        "let five = 5;"
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("let five = 5;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

testMultipleOperators =
  TestCase
    ( assertEqual
        "testing 'let five = 5 + 5 + 5;'"
        "let five = 5 + 5 + 5 ; "
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("let five = 5 + 5 + 5 ;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

testSlashOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 / 5;'"
        "let five = 5 / 5;"
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("let five = 5 / 5;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

testAsteriskOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 * 5;'"
        "let five = 5 * 5;"
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("let five = 5 * 5;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

testMinusOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 - 5;"
        "let five = 5 - 5;"
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("let five = 5 - 5;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

testPlusOperator =
  TestCase
    ( assertEqual
        "testing 'let five = 5 + 5;'"
        "let five = 5 + 5;"
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("let five = 5 + 5;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

testReturnStatement =
  TestCase
    ( assertEqual
        "testing 'return 5;' "
        "return 5;"
        ( statementToString
            ( head
                ( snd
                    (parseStatements (snd (parseTokens ("return 5;", [])), []))
                )
            )
        )
    )

testArithmeticReturnStatement =
  TestCase
    ( assertEqual
        "testing 'return 5 + 5 + 5;'"
        "return 5 + 5 + 5;"
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("return 5 + 5 + 5;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

-- tests = TestList [testPlusOperator, testMinusOperator, testSlashOperator, testAsteriskOperator, testAssignment]
tests = TestList [testReturnStatement, testArithmeticReturnStatement]

main = runTestTT tests
