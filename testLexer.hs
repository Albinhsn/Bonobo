module Test where

import Lexer
import Test.HUnit
import Token

testLexer = TestCase (assertEqual "testing import of lexer" getLexer "Lexer")

testNextToken = TestCase (assertEqual "testing nextToken" nextToken (Token {typ = ILLEGAL, literal = "ILLEGAL"}))

tests = TestList [testLexer, testNextToken]

main = runTestTT tests
