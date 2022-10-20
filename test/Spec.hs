module Main where

import Test.Hspec 
import TestLexer
import TestParser
import TestPrecedence
import TestBool

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
        "let five = ((5 + 5) + 5);"
  
    it "testing slash operators" $ 
      do 
        testSlashOperator
        `shouldBe`
        "let five = (5 / 5);"

    it "testing asterisk operator" $ 
      do 
        testAsteriskOperator
        `shouldBe`
        "let five = (5 * 5);"

    it "testing plus asterisk" $
      do 
        testPlusAsteriskOperators
        `shouldBe`
        "let five = (5 + (5 * 5));"

    it "testing asterisk plus " $
      do 
        testAsteriskPlusOperators
        `shouldBe`
        "let five = ((5 * 5) + 5);"

    it "testing minus operator" $ 
      do 
        testMinusOperator
        `shouldBe`
        "let five = (5 - 5);"

    it "testing arithmetic return statement" $ 
      do 
        testArithmeticReturnStatement 
        `shouldBe`
        "return (5 + 5);"

    it "testing infix" $ 
      do 
        testInfix 
        `shouldBe`
        "let five = (-5);"

  describe "Testing precedence" $ do
    it "testing plus minus" $ 
      do 
        testPlusMinus 
        `shouldBe`
        False  

    it "testing call plus" $ 
      do 
        testCallPlus 
        `shouldBe`
        True
    it "testing basic grouped" $
      do 
        testBasicGrouped
        `shouldBe`
        "let five = ((2 + 3) * 5);"
    it "testing adv grouped" $
      do 
        testAdvGrouped
        `shouldBe`
        "let five = (((2 + (-3)) * (-4)) + ((10 / 5));"

  describe "Testing bools" $ do
    it "testing five equals five" $ 
      do 
        testFiveEqualsFive
        `shouldBe`
        "let five = (5 == 5);"
    it "testing five equals five + 5" $
      do
        testFiveEqualsFivePlusFive 
        `shouldBe`
        "let five = (5 == (5 + 5));"
    it "testing five equals 5 plus 5 times 5" $ 
      do 
        testFiveEqualsFivePlusFiveTimesFive 
        `shouldBe`
        "let five = (5 == (5 + (5 * 5)));"
    it "testing five equals five plus five minus five" $
      do 
        testFiveEqualsFivePlusMinusFive 
        `shouldBe`
        "let five = (5 == (5 + (-5)));"
    it "testing five equals five plus minus five times minues five" $ 
      do 
        testFiveEqualsFivePlusMinusFiveTimesMinusFive 
        `shouldBe`
        "let five = (5 == (5 + ((-5) * (-5))));"
    it "testing random shit" $ 
      do 
        testMultipleBools
        `shouldBe`
        "let five = (((5 > (5 * 2)) == (2 + ((-3) * 5))) > 1);"

