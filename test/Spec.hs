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
import TestMultiple
import TestObject
import TestEval
import TestArray
import TestMap
import TestPrebuilt
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
        ["let", "five", "=", ";", "let", "ten", "=", ";", "let", "add", "=", "fn", "(", "x", ",", "y", ")", "{", "x", "+", "y", ";", "}", ";", "let", "result", "=", "add", "(", "five", ",", "ten", ")", ";", "'Hello World!'","EOF"]
        
  describe "Testing Parser" $ do
    it "testing basic let" $ 
      do 
        testBasicLet
        `shouldBe`
        "let five = 5; let five = 'five'; let five = ((5 + 5) + 5); let five = (5 / 5); let five = (5 * 5); let five = (5 + (5 * 5)); let five = ((5 * 5) + 5); let five = (5 - 5); let five = (-5);"
    it "testing return" $ 
      do 
        testReturnStatement
        `shouldBe`
        "return 5; return (5 + 5);"
      
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
        testGroupedPrecedence
        `shouldBe`
        "let five = (((2 + 3)) * 5); let five = ((((2 + (-3))) * (-4)) + ((10 / 5))); let five = ((((-3) + 5)) + 2) == (8 / 5); let five = (((5 + 2)) * 5) > ((-4) / 1); let five = ((((2 + 2)) * ((5 * 2))) / (((-4) - (-4)))); return ((((((1 + 2)) + 3)) * ((4 * ((5 + 6))))) / ((((-7) - 8)))); return (((1 + 2))); let five = ((((3 * 4) + 1)) == ((-13)));"

  describe "Testing bools" $ do
    it "testing bools" $ 
      do 
        testBools
        `shouldBe`
        "let five = 5 == 5; let five = 5 == (5 + 5); let five = 5 == (5 + (5 * 5)); let five = 5 == (5 + (-5)); let five = 5 == (5 + ((-5) * (-5))); let five = 5 > (5 * 2) == (2 + ((-3) * 5)) > 1; let five = b[0] == b[0];"
  describe "Testing if" $ do
    it "testing empty if" $ 
      do 
        testIf 
        `shouldBe`
        "if( empty ){}; if( empty ){}; if( empty ){let five = 5;}; if( empty ){}else{let five = 5;}; if( empty ){let five = 5;let ten = (5 + 5);}; if( empty ){}else{let five = 5;let ten = (5 + 5);}; if( empty ){let five = 5;}else{let five = 5;}; if( empty ){let five = 5;let ten = 10;}else{let five = 5;let ten = 10;}; if(5 == 5){if(5 == 5){let five = 5;}else{five = 5;};}; if(5 == 5){return 5;};"
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
    it "Testing Prefix Ident" $
      do 
        testPrefixIdent 
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
    it "testing let prefix ident" $
      do 
        testLetPrefixIdent 
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
        testPrefixBodyFunc 
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
        "fn five(){if(5 == 5){let five = 5;}else{return 10;};};"
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
    it "testing prefix func call" $ 
      do 
        testPrefixFuncCall 
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
    it "testing prefix with func call" $
      do 
        testPrefixWithFuncCall 
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
  describe "Test difficult" $ do
   it "Test diff func" $
      do
        testDiffFunc
        `shouldBe`
        "fn five(){if(5 == 5){let five = 5;five = (five - 2);return (five + 3);};};"
   it "Test diff if" $
      do
        testDiffIf
        `shouldBe`
        "if( empty ){if( empty ){}else{if( empty ){};};}else{if( empty ){if( empty ){if( empty ){}else{five = 5;};};};};"
   it "Test diff if" $
      do
        testInsaneIf
        `shouldBe`
        "if( empty ){if( empty ){if( empty ){five = 5;};}else{if( empty ){five = 5;}else{five = 5;};};}else{if( empty ){if( empty ){if( empty ){}else{five = 5;};}else{if( empty ){}else{if( empty ){}else{five = 5;};};};};};"
   it "test nested array/map" $ 
      do
        testEvalMapArrayComb 
        `shouldBe`
        "- a = [{0:[{0:[True, ], }, ], }, 1, ]"
  describe "Test Multiple" $ do
   it "Test Multiple let" $
      do
        testMultipleLet
        `shouldBe`
        "let five = 5; let ten = 10;"
   it "Test Multiple Ident" $
      do
        testMultipleIdent
        `shouldBe`
        "let five = 5; let two = 2; five = (((two * five)) / two);"
   it "Test Multiple Func" $
      do
        testMultipleFunc
        `shouldBe`
        "fn double(numb){if(numb < 0){return ((-2) * numb);}else{return (numb * 2);};}; fn divBy(div,numb){return (numb / div);};"
   it "Test Fn Dec Call" $
      do
        testFnDecCall
        `shouldBe`
        "fn isGTF(numb){return numb > 5;}; let ten = 10; let gtf = isGTF(ten);"
   it "Test Fn Call" $
      do
        testFnCall
        `shouldBe`
        "fn add(a,b){return (a + b);}; let five = add(2,3);"
  describe "test object" $ do
    it "test string " $ 
      do 
        testObjString
        `shouldBe`
        "five = 'five'"
    it "test true obj" $ 
      do 
        testObjTrue 
        `shouldBe`
        "five = True"
    it "test false obj" $ 
      do 
        testObjFalse
        `shouldBe`
        "five = False"
    it "test int obj" $ 
      do 
        testObjInt
        `shouldBe`
        "five = 5"
    it "test neg int obj" $ 
      do 
        testObjMinus
        `shouldBe`
        "five = -5"
    it "test bang obj" $ 
      do 
        testObjBang
        `shouldBe`
        "five = False"
    it "test op plus" $ 
      do 
        testObjOpPlus
        `shouldBe`
        "five = 10"
    it "test op minus" $ 
      do 
        testObjOpMinus
        `shouldBe`
        "five = 0"
    it "test op mul" $ 
      do 
        testObjOpMul
        `shouldBe`
        "five = 25"
    it "test op div" $ 
      do 
        testObjOpDiv
        `shouldBe`
        "five = 1"
    it "test op greater than" $ 
      do 
        testObjOpGT
        `shouldBe`
        "five = False"
    it "test op less than" $ 
      do 
        testObjOpLT
        `shouldBe`
        "five = True"
    it "test op eq" $ 
      do 
        testObjOpEQ
        `shouldBe`
        "five = True"
    it "test op neq" $ 
      do 
        testObjOpNEQ
        `shouldBe`
        "five = False"
    it "test obj grouped" $ 
      do 
        testObjGrouped
        `shouldBe`
        "five = 30"
    it "test obj if false" $ 
      do 
        testObjIfFalse
        `shouldBe`
        "five = 5"
    it "test obj if true" $ 
      do 
        testObjIfTrue
        `shouldBe`
        "ten = 10"
    it "test obj func" $ 
      do 
        testObjFunc
        `shouldBe`
        "fn add(a,b){return (a + b);};"
  describe "test eval" $ do
    it "test func + call" $ 
      do 
        testEvalFuncCall
        `shouldBe`
        "fn add(a,b){return (a + b);}; - five = 5"
    it "test mul func" $ 
      do 
        testEvalMulFunc
        `shouldBe`
        "fn add(a,b){return (a + b);}; fn divide(a,b){return (a / b);}; -"
    it "test mul var" $ 
      do 
        testEvalMulVar
        `shouldBe`
        "- five = 5 ten = 10"
    it "test eval context" $ 
      do 
        testEvalContext
        `shouldBe`
        "fn add(){let five = 5; return 3;}; - three = 3"
    it "test eval overwrite var" $ 
      do 
        testEvalOverwriteVar
        `shouldBe`
        "- five = 5"
    it "test eval nested if" $ 
      do 
        testEvalNestedIf
        `shouldBe`
        "- five = 5"
    it "test eval nested if" $ 
      do 
        testEvalNestedElse
        `shouldBe`
        "- five = 5"
    it "test eval string add" $ 
      do 
        testEvalStringAdd
        `shouldBe`
        "- hello = 'Hello' world = ' World!' add = 'Hello World!'"
    it "test eval array" $ 
      do 
        testEvalArray
        `shouldBe`
        "fn add(a,b){return (a + b);}; - arr = [1, 'Hi', 5, 5, True, ]"
    it "test eval array array" $ 
      do 
        testEvalArrayArray
        `shouldBe`
        "- a = [1, 2, 3, ] arr = [3, 1, ]"
    it "test eval array idx op" $ 
      do 
        testEvalArrayIdxOp
        `shouldBe`
        "- a = [1, 2, 3, 4, 5, ] arr = 3"
    it "test eval idx grouped op" $ 
      do 
        testEvalArrayIdxGroupedOp
        `shouldBe`
        "- a = [1, 2, 3, ] arr = 2"
    it "test eval idx ident" $ 
      do 
        testEvalArrayIdxIdent
        `shouldBe`
        "- a = [1, 2, 3, ] b = 0 arr = 1"
    it "test eval func if" $ 
      do 
        testEvalFuncIf
        `shouldBe`
        "fn add(a,b){if(a > b){return (a + b);}else{return (a - b);};}; - sum = -1"
    it "test eval idx wierd" $ 
      do 
        testEvalArrayIdxWierd
        `shouldBe`
        "- c = [1, 2, 4, 1, ] b = 0 a = [False, True, False, ] arr = True"
  describe "test array" $ do
    it "test array" $ 
      do
        testArray
        `shouldBe`
        "let arr = [1, 'Hi', ((2 + 3)), add(2,3), True, ];"
    it "test array with other statement" $ 
      do
        testArrayLet
        `shouldBe`
        "let arr = [1, 'Hi', ((2 + 3)), add(2,3), True, (-1), 1 == 1, ];"
    it "test array with indexing" $ 
      do
        testArrayIdxInArr
        `shouldBe`
        "let arr = [a[2], 1, ];"
    it "test array operator exp" $ 
      do
        testArrayIdx
        `shouldBe`
        "let arr = (2 + a[2]);"
    it "test array operation with index" $ 
      do
        testArrayIdxGrouped
        `shouldBe`
        "let arr = ((2 + a[2]));"
    it "test array grouped operator" $ 
      do
        testArrayIdxGroupedOp
        `shouldBe`
        "let arr = a[((2 + 3))];"
    it "test array idx operation" $ 
      do
        testArrayIdxOp
        `shouldBe`
        "let arr = a[(2 + 3)];"
    it "test array idx ident" $ 
      do
        testArrayIdxIdent
        `shouldBe`
        "let arr = a[b];"
    it "test array idx wierd" $ 
      do
        testArrayIdxWierd
        `shouldBe`
        "let arr = a[((((2 * b)) + c[3]))];"
  describe  "test map" $ do
    it "test map key val" $
      do 
        testMapIndexKeyVal 
        `shouldBe`
        "let a = {b[0]:b[0], b['k']:b['k'], b[a[0]]:b[a[0]], b[call()]:b[call()], };"
    it "test map" $
      do 
        testMap 
        `shouldBe`
        "let m = {x:x, 1:1, 'a':'a', (1 + 2):(1 + 2), ((1 / 2)):((1 / 2)), a:True, b:{x:x, 1:1, 'a':'a', (1 + 2):(1 + 2), ((1 / 2)):((1 / 2)), a:True, }, };"
    it "test map func" $
      do 
        testMapFunc 
        `shouldBe`
        "fn add(){let m = {x:x, 1:1, 'a':'a', (1 + 2):(1 + 2), ((1 / 2)):((1 / 2)), a:True, };};"
    it "test eval map" $
      do 
        testEvalMap
        `shouldBe`
        "- x = 0 a = 1 m = {0:0, 1:1, 'a':'a', 3:3, 2:2, 1:True, }"
    it "test eval map func" $
      do 
        testEvalMapFunc
        `shouldBe`
        "fn add(a,b){let c = {1:3, 2:4, }; return (c[a] + c[b]);}; - a = 7"
    it "test eval map func if " $
      do 
        testEvalMapFuncIf
        `shouldBe`
        "fn a(b){if(b > 1){let c = {3:4, };return c[b];}else{let c = {0:5, };return c[b];};}; - d = 4"
    it "test eval map func else" $
      do 
        testEvalMapFuncElse
        `shouldBe`
        "fn a(b){if(b > 1){let c = {3:4, };return c[b];}else{let c = {0:5, };return c[b];};}; - d = 5"
    it "test eval map assign" $
      do 
        testEvalMapAssign
        `shouldBe`
        "- a = {1:10, 2:2, '3':'4', 4:{4:True, }, 'k':True, }"
    it "test array eval assign" $ 
      do
        testEvalArrayAssign
        `shouldBe`
        "- a = [1, False, 3, [4, True, ], ]"
  describe "test prebuilt funcs" $ do
    it "test append" $
      do
        testEvalAppend 
        `shouldBe`
        "- a = [1, ]"
    it "test len str" $
      do
        testEvalLenStr
        `shouldBe`
        "- s = 'hello world' l = 11"
    it "test len array" $
      do
        testEvalLenArray
        `shouldBe`
        "- a = [1, 2, 3, 4, ] l = 4"
    it "test len map" $
      do
        testEvalLenMap
        `shouldBe`
        "- a = {1:1, 2:2, 3:3, 4:4, } l = 4"
