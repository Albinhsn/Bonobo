module TestPrecendence where

import Ast
import Test.HUnit
import Token

testPlusMinus =
  TestCase
    ( assertEqual
        "testing plus vs minus"
        True
        (hasPrecedence (MINUS, PLUS))
    )

testCallPlus =
  TestCase
    ( assertEqual
        "testing plus vs minus"
        True
        (hasPrecedence (FUNCTION, PLUS))
    )

testList = TestList [testPlusMinus, testCallPlus]

main = runTestTT testList
