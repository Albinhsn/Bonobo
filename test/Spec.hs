module Main where

import Test.Hspec 
import TestDifficult
import TestLexer
import TestParser
import TestPrecedence
import TestBool
import TestIf 
import TestIdent
import TestFunc
-- import TestTFT

main :: IO ()
main = hspec $ do
  describe "Testing Lexer" $ do
    it "testing actual code" $
      do
        testActualNumbers 
        `shouldBe`
        ["let", "five", "=", "5", ";", "let", "ten", "=", "10", ";", "let", "add", "=", "fn", "(", "five", ",", "ten", ")", "{", "return", "x", "+", "y", ";", "}", ";", "let", "result", "=", "add", "(", "five", ",", "ten", ")", ";", "!", "-", "/", "*", "5", ";", "5", "<", "10", ">", "5", ";", "if", "(", "5", "<", "10", ")", "{", "return", "true", ";", "}", "else", "{", "return", "false", ";", "}", "10", "==", "10", ";", "10", "!=", "9", ";", "EOF"]
    it "testing actual" $
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
        "if ( empty ){};"
    it "testing empty if else" $ 
      do 
        testEmptyIfElse 
        `shouldBe`
        "if ( empty ){};"
    it "testing con" $ 
      do 
        testCon 
        `shouldBe`
        "if ( empty ){let five = 5;};"
    it "testing alt" $ 
      do 
        testAlt
        `shouldBe`
        "if ( empty ){}else{let five = 5;};"
    it "testingMultipleCon" $ 
      do 
        testMultipleCon
        `shouldBe`
        "if ( empty ){let five = 5;let ten = (5 + 5);};"
    it "testingMultipleAlt" $ 
      do 
        testMultipleAlt
        `shouldBe`
        "if ( empty ){}else{let five = 5;let ten = (5 + 5);};"
    it "testingConAlt" $ 
      do 
        testConAlt
        `shouldBe`
        "if ( empty ){let five = 5;}else{let five = 5;};"
    it "testingMultipleConAlt" $ 
      do 
        testMultipleConAlt
        `shouldBe`
        "if ( empty ){let five = 5;let ten = 10;}else{let five = 5;let ten = 10;};"
    it "testing multiple if" $ 
      do 
        testMultipleIf
        `shouldBe`
        "if (5 == 5){if (5 == 5){let five = 5;}else{return 5;};};"
    it "testing param if" $ 
      do 
        testIfParam
        `shouldBe`
        "if (5 == 5){return 5;};"
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
    it "Testing let op Ident" $
      do 
        testLetOpIdent 
        `shouldBe`
        "let five = (2 + three);"
    it "Testing let assign Ident" $
      do 
        testLetAssignIdent 
        `shouldBe`
        "let five = three;"
    it "testing let infix ident" $
      do 
        testLetInfixIdent 
        `shouldBe`
        "let five = (-five);"
    it "testing let bool ident" $
      do 
        testLetBoolIdent 
        `shouldBe`
        "let five = three == 3;"
  describe "Testing Func" $ do
    it "testing empty func" $
      do
        testEmptyFunc 
        `shouldBe`
        "fn five(){};"
    it "testing param func" $
      do
        testParamFunc 
        `shouldBe`
        "fn five(a,b){};"
    it "testing return func" $
      do
        testReturnFunc 
        `shouldBe`
        "fn five(){return 5;};"
    it "testing param return func" $
      do
        testParamReturnFunc 
        `shouldBe`
        "fn five(a,b){return 5;};"
    it "testing multiple body" $ 
      do 
        testMultipleBodyFunc 
          `shouldBe`
          "fn five(){let five = 5; return five;};"
    it "testing operator body func" $ 
      do 
        testOperatorBodyFunc 
          `shouldBe`
          "fn five(){let five = (2 + 3);};"
    it "testing grouped operator body func" $ 
      do 
        testGroupedOperatorBodyFunc 
          `shouldBe`
          "fn five(){let five = ((2 + 3));};"
    it "testing grouped operator body func" $ 
      do 
        testInfixBodyFunc 
          `shouldBe`
          "fn five(){let five = (-5);};"
    it "testing bool body func" $ 
      do 
        testBoolBodyFunc 
          `shouldBe`
          "fn five(){let five = 5 == 5;};"
    it "testing if func" $ 
      do 
        testIfFunc 
        `shouldBe`
        "fn five(){if (5 == 5){let five = 5;}else{return 10;};};"
    it "testing func call in func" $ 
      do 
        testFuncCallInFunc 
        `shouldBe`
        "fn five(){return five();};"
    it "testing func call" $ 
      do 
        testFuncCall 
          `shouldBe`
          "add(5);"
    it "testing op func call" $ 
      do 
        testOpFuncCall 
          `shouldBe`
          "add((5 + 5));"
    it "testing infix func call" $ 
      do 
        testInfixFuncCall 
          `shouldBe`
          "add((-5));"
    it "testing grouped op func call" $ 
      do 
        testGroupedOpFuncCall 
          `shouldBe`
          "add((((5 + 3)) * 2));"

    it "testing let with func call" $
      do 
        testLetWithFuncCall 
        `shouldBe`
        "let five = five();"

    it "testing return with func call" $
      do 
        testReturnWithFuncCall 
        `shouldBe`
        "return five();"
    it "testing operator with func call" $
      do 
        testOperatorWithFuncCall 
        `shouldBe`
        "let five = (2 + addThree());"
    it "testing infix with func call" $
      do 
        testInfixWithFuncCall 
        `shouldBe`
        "let five = (-five());"
    it "testing bool with func call" $
      do 
        testBoolWithFuncCall 
        `shouldBe`
        "let five = five() == five();"
    it "testing grouped with func call" $
      do 
        testGroupedWithFuncCall 
        `shouldBe`
        "let five = ((2 + addThree()));"
    it "testing func call with param" $
      do 
        testFuncCallWithParam 
        `shouldBe`
        "let five = addThree(a);"
    it "testing func call with multiple param" $
      do 
        testFuncCallWithMulParams  
        `shouldBe`
        "let five = addThree(a,b);"
  
  -- describe "Testing TFT" $ do
  --   it "testing let true" $
  --     do
  --       testLetTrue
  --       `shouldBe`
  --       "let five = true;"
  describe "Test difficult" $ do
   it "Test diff func" $
      do
        testDiffFunc
        `shouldBe`
        "fn five(){if (5 == 5){let five = 5;five = (five - 2);return (five + 3);};};"
  describe "Test difficult" $ do
   it "Test diff if" $
      do
        testDiffIf
        `shouldBe`
        "if ( empty ){if ( empty ){}else{if ( empty ){};};}else{if ( empty ){if ( empty ){if ( empty ){}else{return 5;};};};};"
