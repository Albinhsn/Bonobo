module Main where

import Test.Hspec 
import TestLexer
import TestParser
import TestPrecedence
import TestBool
import TestIf 
import TestIdent
import TestFunc

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
        "let five = (((2 + 3)) * 5);"
    it "testing adv grouped" $
      do 
        testAdvGrouped
        `shouldBe`
        "let five = ((((2 + (-3))) * (-4)) + ((10 / 5)));"
    it "testing bool grouped" $ 
      do 
        testBoolGrouped 
        `shouldBe` 
        "let five = ((((-3) + 5)) + 2) == (8 / 5);"
    it "testing bool grouped2" $ 
      do 
        testBoolGrouped2 
        `shouldBe` 
        "let five = (((5 + 2)) * 5) > ((-4) / 1);"
    it "testing grouped" $ 
      do 
        testGrouped2
        `shouldBe` 
        "let five = ((((2 + 2)) * ((5 * 2))) / (((-4) - (-4))));"
    it "testing 1" $ 
      do 
        test1 
        `shouldBe`
        "return ((((((1 + 2)) + 3)) * ((4 * ((5 + 6))))) / ((((-7) - 8))));"
    it "testing 2" $ 
      do 
        test2 
        `shouldBe`
        "return (((1 + 2)));"
    it "testing 3" $ 
      do 
        test3 
        `shouldBe`
        "let five = ((((3 * 4) + 1)) == ((-13)));"

  describe "Testing bools" $ do
    it "testing five equals five" $ 
      do 
        testFiveEqualsFive
        `shouldBe`
        "let five = 5 == 5;"
    it "testing five equals five + 5" $
      do
        testFiveEqualsFivePlusFive 
        `shouldBe`
        "let five = 5 == (5 + 5);"
    it "testing five equals 5 plus 5 times 5" $ 
      do 
        testFiveEqualsFivePlusFiveTimesFive 
        `shouldBe`
        "let five = 5 == (5 + (5 * 5));"
    it "testing five equals five plus five minus five" $
      do 
        testFiveEqualsFivePlusMinusFive 
        `shouldBe`
        "let five = 5 == (5 + (-5));"
    it "testing five equals five plus minus five times minues five" $ 
      do 
        testFiveEqualsFivePlusMinusFiveTimesMinusFive 
        `shouldBe`
        "let five = 5 == (5 + ((-5) * (-5)));"
    it "testing random shit" $ 
      do 
        testMultipleBools
        `shouldBe`
        "let five = 5 > (5 * 2) == (2 + ((-3) * 5)) > 1;"
  describe "Testing if" $ do
    it "testing empty if" $ 
      do 
        testEmptyIf 
        `shouldBe`
        "if ( empty ){}"
    it "testing empty if else" $ 
      do 
        testEmptyIfElse 
        `shouldBe`
        "if ( empty ){}"
    it "testing con" $ 
      do 
        testCon 
        `shouldBe`
        "if ( empty ){let five = 5;}"
    it "testing alt" $ 
      do 
        testAlt
        `shouldBe`
        "if ( empty ){}{let five = 5;}"
    it "testingMultipleCon" $ 
      do 
        testMultipleCon
        `shouldBe`
        "if ( empty ){let five = 5;let ten = (5 + 5);}"
    it "testingMultipleAlt" $ 
      do 
        testMultipleAlt
        `shouldBe`
        "if ( empty ){}{let five = 5;let ten = (5 + 5);}"
    it "testingConAlt" $ 
      do 
        testConAlt
        `shouldBe`
        "if ( empty ){let five = 5;}{let five = 5;}"
    it "testingMultipleConAlt" $ 
      do 
        testMultipleConAlt
        `shouldBe`
        "if ( empty ){let five = 5;let ten = 10;}{let five = 5;let ten = 10;}"
  describe "Testing Ident" $ do
    it "testing ident" $ 
      do 
        testIdent 
        `shouldBe`
        "five = 5;"
    it "Testing Op Ident" $
      do 
        testOpIdent 
        `shouldBe`
        "five = (5 * 5);"
    it "Testing Bool Ident" $
      do 
        testBoolIdent 
        `shouldBe`
        "five = 5 == 5;"
    it "Testing Infix Ident" $
      do 
        testInfixIdent 
        `shouldBe`
        "five = (-5);"
    it "Testing Grouped Ident" $
      do 
        testGroupedIdent 
        `shouldBe`
        "five = ((5 + 5));"
  describe "Testing Func" $ do
    it "testing empty func" $
      do
        testEmptyFunc 
        `shouldBe`
        "func(){};"
    it "testing param func" $
      do
        testParamFunc 
        `shouldBe`
        "func(a, b){};"
    it "testing return func" $
      do
        testReturnFunc 
        `shouldBe`
        "func(){return 5;};"
    it "testing param return func" $
      do
        testParamReturnFunc 
        `shouldBe`
        "func(a,b){return 5;};"
