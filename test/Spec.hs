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
import TestObject
import TestEval
import TestArray
import TestMap
import TestPrebuilt
import TestFor
import TestCode
import TestVM
import TestNoSta
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
        ["let", "five", "=", ";", "let", "ten", "=", ";", "let", "add", "=", "fn", "(", "x", ",", "y", ")", "{", "x", "+", "y", ";", "}", ";", "let", "result", "=", "add", "(", "five", ",", "ten", ")", ";", "'Hello World!'","for","EOF"]
        
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
    it "test vm bool 1" $ 
      do
        testVMBool1
        `shouldBe`
        "True"
    it "test vm bool 2" $ 
      do
        testVMBool2
        `shouldBe`
        "False"
    it "test vm bool 3" $ 
      do
        testVMBool3
        `shouldBe`
        "True"
    it "test vm bool 4" $ 
      do
        testVMBool4
        `shouldBe`
        "False"
    it "test vm bool 5" $ 
      do
        testVMBool5
        `shouldBe`
        "True"
    it "test vm bool 6" $ 
      do
        testVMBool6
        `shouldBe`
        "False"
    it "test vm bool 7" $ 
      do
        testVMBool7
        `shouldBe`
        "True"
    it "test vm bool 8" $ 
      do
        testVMBool8
        `shouldBe`
        "False"
    it "test vm bool book 1" $ 
      do
        testVMBoolBook1
        `shouldBe`
        "True"
    it "test vm bool book 2" $ 
      do
        testVMBoolBook2
        `shouldBe`
        "False"
    it "test vm bool book 3" $ 
      do
        testVMBoolBook3
        `shouldBe`
        "True"
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
        "five = 5; five = (5 * 5); five = 5 == 5; five = (-5); five = ((5 + 5)); let five = (2 + three); let five = three; let five = (-five); let five = ((three + two)); let five = three == 3;"
  describe "Testing Func" $ do
    it "testing empty func" $
      do
        testFunc 
        `shouldBe`
        "fn five(){}; fn five(a,b){}; fn five(){return 5;}; fn five(a,b){return 5;}; fn five(){let five = 5; return five;}; fn five(){let five = (2 + 3);}; fn five(){let five = ((2 + 3));}; fn five(){let five = (-5);}; fn five(){let five = 5 == 5;}; fn five(){if(5 == 5){let five = 5;}else{return 10;};}; fn five(){return five();}; add(5); add((5 + 5)); add((-5)); add((((5 + 3)) * 2)); let five = five(); return five(); let five = (2 + addThree()); let five = (-five()); let five = five() == five();"
    it "testing grouped with func call" $
      do 
        testCall 
        `shouldBe`
        "let five = ((2 + addThree())); let five = addThree(a); let five = addThree(a,b);"
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

   it "Test insane if" $
      do
        testInsaneIf
        `shouldBe`
        "if( empty ){if( empty ){if( empty ){five = 5;};}else{if( empty ){five = 5;}else{five = 5;};};}else{if( empty ){if( empty ){if( empty ){}else{five = 5;};}else{if( empty ){}else{if( empty ){}else{five = 5;};};};};};"
   it "test nested array/map" $ 
      do
        testEvalMapArrayComb 
        `shouldBe`
        "- a = [{0:[{0:[True, ], }, ], }, 1, ]"
  describe "test object" $ do
    it "test string " $ 
      do 
        testObj
        `shouldBe`
        "- one = 'five' two = True three = False four = 5 five = False six = -5"
    it "test op plus" $ 
      do 
        testObjOp
        `shouldBe`
        "- zero = 0 one = 10 two = 25 three = 1"
    it "test op greater than" $ 
      do 
        testObjBoolOp
        `shouldBe`
        "- one = False two = True three = True four = False"
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
    it "test eval nested else" $ 
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
  describe "test test for" $ do
    it "test for" $ 
      do 
        testFor 
        `shouldBe`
        "for(i = 0; i < 5; (i + 1);){k = (k + i);};" 
    it "test for eval" $ 
      do 
        testForEval 
        `shouldBe`
        "- k = 15" 
    it "test" $ 
      do 
        testPrint
        `shouldBe`
        "-" 
  describe "test code" $ do
    it "test make " $
      do 
        testMake 
        `shouldBe`
        "000112 - 1 2 "
    it "test make 2" $
      do 
        testMake2 
        `shouldBe`
        " CONST 2 CONST 2 MUL CONST 3 CONST 2 MUL ADD"
    it "test code if" $
      do 
        testCodeIf
        `shouldBe`
        " TRUE JUMPNT 6 CONST 5 JUMP 2"
    it "test code if else" $
      do 
        testCodeIfElse
        `shouldBe`
        " TRUE JUMPNT 6 CONST 5 JUMP 4 CONST 10"
    it "test make true" $
      do 
        testMakeTFTrue 
        `shouldBe`
        "6"
    it "test make false" $
      do 
        testMakeTFFalse
        `shouldBe`
        "7"
  describe "test vm" $ do
    it "test vm op 1 " $
      do 
        testVMOp1
        `shouldBe`
        "2"
    it "test vm op 2" $
      do 
        testVMOp2
        `shouldBe`
        "5"
    it "test vm op 3" $
      do 
        testVMOp3
        `shouldBe`
        "10"
    it "test vm op 4" $
      do 
        testVMOp4
        `shouldBe`
        "11"
    it "test vm book 1" $
      do 
        testVMOpBook1
        `shouldBe`
        "-1"
    it "test vm book 2" $
      do 
        testVMOpBook2
        `shouldBe`
        "2"
    it "test vm book 3" $
      do 
        testVMOpBook3
        `shouldBe`
        "2"
    it "test vm book 4" $
      do 
        testVMOpBook4
        `shouldBe`
        "55"
    it "test vm book 5" $
      do 
        testVMOpBook5
        `shouldBe`
        "10"
    it "test vm book 6" $
      do 
        testVMOpBook6
        `shouldBe`
        "32"
    it "test vm book 7" $
      do 
        testVMOpBook7
        `shouldBe`
        "20"
    it "test vm book 8" $
      do 
        testVMOpBook8
        `shouldBe`
        "25"
    it "test vm book 9" $
      do 
        testVMOpBook9
        `shouldBe`
        "60"
    it "test vm op big" $
      do 
        testVMOpBig
        `shouldBe`
        "140000"
    it "test vm if" $
      do 
        testVMIf
        `shouldBe`
        "5"
    it "test vm else" $
      do 
        testVMElse
        `shouldBe`
        "10"
    it "test vm nested if" $
      do 
        testVMNestedIf
        `shouldBe`
        "3"
    it "test vm nested else" $
      do 
        testVMNestedElse
        `shouldBe`
        "5"
  describe "test no sta" $ do
    it "test empty" $
      do 
        testEmpty
        `shouldBe`
        "5;"
    it "test empty if " $
      do 
        testIfEmpty
        `shouldBe`
        "if(True){5;}else{3;}; 2;"
    it "test empty func" $
      do 
        testFuncEmpty
        `shouldBe`
        "fn add(a,b){5; return (a + b);}; 10;"
    it "test empty for" $
      do 
        testForEmpty
        `shouldBe`
        "for(i = 0; i < 5; (i + 1);){5;}; 10;"
