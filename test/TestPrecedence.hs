module TestPrecedence where

import Ast
import Test.HUnit
import Token

testPlusMinus :: Test
testPlusMinus =
  TestCase
    ( assertEqual
        "testing plus vs minus"
        False
        (hasPrecedence (MINUS, PLUS))
    )

testCallPlus :: Test
testCallPlus =
  TestCase
    ( assertEqual
        "testing plus vs minus"
        True
        (hasPrecedence (FUNCTION, PLUS))
    )

testList :: Test
testList = TestList [testPlusMinus, testCallPlus]

runPrecedenceTests :: IO Counts
runPrecedenceTests = runTestTT testList
