module TestParser where

import Ast
import Lexer
import Parser
import Test.HUnit
import Token
import Utils

testAssignment :: Test
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

testReturnStatement :: Test
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

testArithmeticReturnStatement :: Test
testArithmeticReturnStatement =
  TestCase
    ( assertEqual
        "testing 'return 5 + 5 + 5;'"
        "return 5 + 5 + 5;"
        ( statementToString
            ( head
                ( snd
                    ( parseStatements
                        ( snd (parseTokens ("return 5 + 5;", [])),
                          []
                        )
                    )
                )
            )
        )
    )

tests :: Test
tests = TestList [testPlusOperator, testMinusOperator, testSlashOperator, testAsteriskOperator, testAssignment]

runParserTests :: IO Counts
runParserTests = runTestTT tests
