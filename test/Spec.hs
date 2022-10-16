module Main where

import Test.Hspec 
import TestLexer
import TestParser
import TestPrecedence

main :: IO ()
main = hspec $ do
  describe "Testing Lexer" $ do
    it "testing actual code" $
      do
        testActualNumbers 
        `shouldBe`
        ["let", "five", "=", "5", ";", "let", "ten", "=", "10", ";", "let", "add", "=", "fn", "(", "five", ",", "ten", ")", "{", "return", "x", "+", "y", ";", "}", ";", "let", "result", "=", "add", "(", "five", ",", "ten", ")", ";", "!", "-", "/", "*", "5", ";", "5", "<", "10", ">", "5", ";", "if", "(", "5", "<", "10", ")", "{", "return", "true", ";", "}", "else", "{", "return", "false", ";", "}", "10", "==", "10", ";", "10", "!=", "9", ";", "EOF"]
    it "test actual" $
      do 
        testActual 
        `shouldBe`
        ["let", "five", "=", ";", "let", "ten", "=", ";", "let", "add", "=", "fn", "(", "x", ",", "y", ")", "{", "x", "+", "y", ";", "}", ";", "let", "result", "=", "add", "(", "five", ",", "ten", ")", ";", "EOF"]
        
  describe "Testing Parser" $ do
    it "testing assignment" $ 
      do 
        testAssignment 
        `shouldBe`
        "let five = 5;"
  
    it "testing multiple operators" $ 
      do 
        testMultipleOperators
        `shouldBe`
        "let five = 5 + 5 + 5 ;"
  
    it "testing slash operators" $ 
      do 
        testSlashOperator
        `shouldBe`
        "let five = 5 / 5;"

    it "testing asterisk operator" $ 
      do 
        testAsteriskOperator
        `shouldBe`
        "let five = 5 * 5;"

    it "testing minus operator" $ 
      do 
        testMinusOperator
        `shouldBe`
        "let five = 5 - 5;"

    it "testing arithmetic return statement" $ 
      do 
        testArithmeticReturnStatement 
        `shouldBe`
        "return 5 + 5 + 5;"

    it "testing infix" $ 
      do 
        testInfix 
        `shouldBe`
        "let five = -5;"

  describe "Testing precedence" $ do
    it "testing plus minus" $ 
      do 
        testPlusMinus 
        `shouldBe`
        True

    it "testing call plus" $ 
      do 
        testCallPlus 
        `shouldBe`
        True
