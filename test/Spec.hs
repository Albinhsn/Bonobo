module Main where

import Test.Hspec 
import TestLexer
import TestCode
import TestBool 
import TestVM
import TestParser

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
  describe "Testing bools" $ do
    it "test vm bool 1" $ 
      do
        testVMBool1
        `shouldBe`
        "Stack:  Globals: 0 = True "
    it "test vm bool 2" $ 
      do
        testVMBool2
        `shouldBe`
        "Stack:  Globals: 0 = False "
    it "test vm bool 3" $ 
      do
        testVMBool3
        `shouldBe`
        "Stack:  Globals: 0 = True "
    it "test vm bool 4" $ 
      do
        testVMBool4
        `shouldBe`
        "Stack:  Globals: 0 = False "
    it "test vm bool 5" $ 
      do
        testVMBool5
        `shouldBe`
        "Stack:  Globals: 0 = True "
    it "test vm bool 6" $ 
      do
        testVMBool6
        `shouldBe`
        "Stack:  Globals: 0 = False "
    it "test vm bool 7" $ 
      do
        testVMBool7
        `shouldBe`
        "Stack:  Globals: 0 = True "
    it "test vm bool 8" $ 
      do
        testVMBool8
        `shouldBe`
        "Stack:  Globals: 0 = False "
    it "test vm bool book 1" $ 
      do
        testVMBoolBook1
        `shouldBe`
        "Stack:  Globals: 0 = True "
    it "test vm bool book 2" $ 
      do
        testVMBoolBook2
        `shouldBe`
        "Stack:  Globals: 0 = False "
    it "test vm bool book 3" $ 
      do
        testVMBoolBook3
        `shouldBe`
        "Stack:  Globals: 0 = True "
    it "test code if" $
      do 
        testCodeIf
        `shouldBe`
        " TRUE JUMPNT 8 CONST 0 SETGLOBAL 0 JUMP 2"
    it "test code if else" $
      do 
        testCodeIfElse
        `shouldBe`
        " TRUE JUMPNT 8 CONST 0 SETGLOBAL 0 JUMP 6 CONST 1 SETGLOBAL 1"
    it "test code let" $
      do 
        testCodeLet
        `shouldBe`
        " CONST 0 SETGLOBAL 0"
    it "test code array 3" $
      do 
        testCodeArray3
        `shouldBe`
        " ARRAY 0 CONST 0 CONST 1 ARRAY 2 ARRAY 2 SETGLOBAL 0"
    it "test code array 4" $
      do 
        testCodeArray4
        `shouldBe`
        " CONST 0 CONST 1 ARRAY 2 ARRAY 1 ARRAY 1 SETGLOBAL 0"
    it "test code array 5" $
      do 
        testCodeArray5
        `shouldBe`
        " CONST 0 CONST 1 ARRAY 2 ARRAY 0 ARRAY 2 SETGLOBAL 0"
    it "test code map 1" $
      do 
        testCodeMap1
        `shouldBe`
        " CONST 0 CONST 1 CONST 2 CONST 3 CONST 4 CONST 5 CONST 6 CONST 7 HASH 4 SETGLOBAL 0"
    it "test code index 1" $
      do 
        testCodeIndex1
        `shouldBe`
        " CONST 0 CONST 1 ARRAY 2 CONST 2 ARRAY 2 SETGLOBAL 0 GETGLOBAL 0 CONST 3 INDEX CONST 4 INDEX SETGLOBAL 1"
    it "test code index 2" $
      do 
        testCodeIndex2
        `shouldBe`
        " CONST 0 CONST 1 ARRAY 2 CONST 2 ARRAY 2 SETGLOBAL 0 CONST 3 CONST 4 TRUE GETGLOBAL 0 SETINDEX SETGLOBAL 0"
    it "test code index 3" $
      do 
        testCodeIndex3
        `shouldBe`
        " CONST 0 CONST 1 ARRAY 2 CONST 2 ARRAY 2 SETGLOBAL 0 CONST 3 TRUE GETGLOBAL 0 SETINDEX SETGLOBAL 0"
    it "test code fn" $
      do 
        testCodeFN
        `shouldBe`
        " CONST 2 SETGLOBAL 0"
    it "test code fn 2" $
      do 
        testCodeFN2
        `shouldBe`
        " CONST 0 SETGLOBAL 0 CONST 1 CONST 2 GETGLOBAL 0 OPCALL 0 SETGLOBAL 1"
  describe "test vm" $ do
    it "test vm op 1 " $
      do 
        testVMOp1
        `shouldBe`
        "Stack:  Globals: 0 = 2 "
    it "test vm op 2" $
      do 
        testVMOp2
        `shouldBe`
        "Stack:  Globals: 0 = 5 "
    it "test vm op 3" $
      do 
        testVMOp3
        `shouldBe`
        "Stack:  Globals: 0 = 10 "
    it "test vm op 4" $
      do 
        testVMOp4
        `shouldBe`
        "Stack:  Globals: 0 = 11 "
    it "test vm book 1" $
      do 
        testVMOpBook1
        `shouldBe`
        "Stack:  Globals: 0 = -1 "
    it "test vm book 2" $
      do 
        testVMOpBook2
        `shouldBe`
        "Stack:  Globals: 0 = 2 "
    it "test vm book 3" $
      do 
        testVMOpBook3
        `shouldBe`
        "Stack:  Globals: 0 = 2 "
    it "test vm book 4" $
      do 
        testVMOpBook4
        `shouldBe`
        "Stack:  Globals: 0 = 55 "
    it "test vm book 5" $
      do 
        testVMOpBook5
        `shouldBe`
        "Stack:  Globals: 0 = 10 "
    it "test vm book 6" $
      do 
        testVMOpBook6
        `shouldBe`
        "Stack:  Globals: 0 = 32 "
    it "test vm book 7" $
      do 
        testVMOpBook7
        `shouldBe`
        "Stack:  Globals: 0 = 20 "
    it "test vm book 8" $
      do 
        testVMOpBook8
        `shouldBe`
        "Stack:  Globals: 0 = 25 "
    it "test vm book 9" $
      do 
        testVMOpBook9
        `shouldBe`
        "Stack:  Globals: 0 = 60 "
    it "test vm str book" $
      do 
        testVMStrBook
        `shouldBe`
        "Stack:  Globals: 0 = 'monkey' "
    it "test vm str op book 1" $
      do 
        testVMOpStrBook1
        `shouldBe`
        "Stack:  Globals: 0 = 'monkey' "
    it "test vm str op book 2" $
      do 
        testVMOpStrBook2
        `shouldBe`
        "Stack:  Globals: 0 = 'monkeybanana' "
    it "test vm op big" $
      do 
        testVMOpBig
        `shouldBe`
        "Stack:  Globals: 0 = 140000 "
    it "test vm if" $
      do 
        testVMIf
        `shouldBe`
        "Stack:  Globals: 0 = 5 "
    it "test vm else" $
      do 
        testVMElse
        `shouldBe`
        "Stack:  Globals: 1 = 10 "
    it "test vm nested if" $
      do 
        testVMNestedIf
        `shouldBe`
        "Stack:  Globals: 0 = 3 "
    it "test vm nested else" $
      do 
        testVMNestedElse
        `shouldBe`
        "Stack:  Globals: 0 = 5 "
    it "test vm let book 1" $
      do 
        testVMLetBook1
        `shouldBe`
        "Stack:  Globals: 0 = 1 "
    it "test vm let book 2" $
      do 
        testVMLetBook2
        `shouldBe`
        "Stack:  Globals: 0 = 1 1 = 2 2 = 3 "
    it "test vm let book 2" $
      do 
        testVMLetBook3
        `shouldBe`
        "Stack:  Globals: 0 = 1 1 = 2 2 = 3 "
    it "test vm array 1" $
      do 
        testVMArray1
        `shouldBe`
        "Stack:  Globals: 0 = [] "
    it "test vm array 2" $
      do 
        testVMArray2
        `shouldBe`
        "Stack:  Globals: 0 = [1, 'hi', ] "
    it "test vm array 3" $
      do 
        testVMArray3
        `shouldBe`
        "Stack:  Globals: 0 = [[], [1, 2, ], ] "
    it "test vm array 4" $
      do 
        testVMArray4
        `shouldBe`
        "Stack:  Globals: 0 = [[[1, 2, ], ], ] "
    it "test vm array 5" $
      do 
        testVMArray5
        `shouldBe`
        "Stack:  Globals: 0 = [[1, 2, ], [], ] "
    it "test vm map 1" $
      do 
        testVMMap1
        `shouldBe`
        "Stack:  Globals: 0 = {1:1, 2:2, 3:3, 4:4, } "
    it "test vm map 2" $
      do 
        testVMMap2
        `shouldBe`
        "Stack:  Globals: 0 = {1:{1:1, 2:2, }, } "
    it "test vm array map 1" $
      do 
        testVMAM1
        `shouldBe`
        "Stack:  Globals: 0 = [{1:2, }, ] "
    it "test vm array map 2" $
      do 
        testVMAM2
        `shouldBe`
        "Stack:  Globals: 0 = {1:[1, 2, ], } "
    it "test vm index 1" $
      do 
        testVMIndex1
        `shouldBe`
        "Stack:  Globals: 0 = [[0, 1, ], 2, ] 1 = 1 "
    it "test vm index 2" $
      do 
        testVMIndex2
        `shouldBe`
        "Stack:  Globals: 0 = {0:{1:2, }, } 1 = 2 "
    it "test vm index 3" $
      do 
        testVMIndex3
        `shouldBe`
        "Stack:  Globals: 0 = [{1:True, }, ] 1 = True "
    it "test vm index 4" $
      do 
        testVMIndex4
        `shouldBe`
        "Stack:  Globals: 0 = {0:[1, True, ], } 1 = True "
    it "test vm index 5" $
      do 
        testVMIndex5
        `shouldBe`
        "Stack:  Globals: 0 = [{'a':[0, {'b':True, }, ], }, ] 1 = True "
    it "test vm index 6" $
      do 
        testVMIndex6
        `shouldBe`
        "Stack:  Globals: 0 = [0, 1, True, ] 1 = True "
    it "test vm index assign 1" $
      do 
        testVMIndexAssign1
        `shouldBe`
        "Stack:  Globals: 0 = [[0, True, ], 2, ] "
    it "test vm index assign 2" $
      do 
        testVMIndexAssign2
        `shouldBe`
        "Stack:  Globals: 0 = {0:{1:True, }, } "
    it "test vm index assign 3" $
      do 
        testVMIndexAssign3
        `shouldBe`
        "Stack:  Globals: 0 = {0:[False, {0:[0, True, ], }, ], } "
    it "test vm fn 1" $
      do 
        testVMFN1
        `shouldBe`
        "15"
    it "test vm fn 2" $
      do 
        testVMFN2
        `shouldBe`
        "15"
    it "test vm fn 3" $
      do 
        testVMFN3
        `shouldBe`
        "2"
    it "test vm fn 4" $
      do 
        testVMFN4
        `shouldBe`
        "5"
    it "test vm fn 5" $
      do 
        testVMFN5
        `shouldBe`
        "5"
    it "test vm fn 6" $
      do 
        testVMFN6
        `shouldBe`
        "5"
    it "test vm fn 7" $
      do 
        testVMFN7
        `shouldBe`
        "5"
    it "test vm fn 9" $
      do 
        testVMFN9
        `shouldBe`
        "5"
    it "test vm fn 10" $
      do 
        testVMFN10
        `shouldBe`
        "2"
    it "test vm fn 11" $
      do 
        testVMFN11
        `shouldBe`
        "2"
    it "test vm fn 12" $
      do 
        testVMFN12
        `shouldBe`
        "True"
    it "test vm fn 13" $
      do 
        testVMFN13
        `shouldBe`
        "True"
    it "test vm fn 14" $
      do 
        testVMFN14
        `shouldBe`
        "False"
    it "test vm fn 15" $
      do 
        testVMFN15
        `shouldBe`
        "2"
    it "test vm fn 16" $
      do 
        testVMFN16
        `shouldBe`
        "5"
    it "test vm fn 17" $
      do 
        testVMFN17
        `shouldBe`
        "42"
    it "test vm fn 18" $
      do 
        testVMFN18
        `shouldBe`
        "30"
    it "test vm fn book" $
      do 
        testVMFNBook
        `shouldBe`
        "97"
  
  describe "test parser v2" $ do
    it "test parser basic let" $
      do 
        testBasicLet
        `shouldBe`
        "let five = 5; let five = 'five'; let five = (-5); let five = (5 + 5); let five = (5 + (5 * 5)); let five = ((5 * 5) + 5); let five = (5 + (-5)); let five = (((5 + 5)) * 5); let five = five; let five = True; let five = 5 == 5; let five = [1, 2, ]; let five = {1:1, 'hello':'world', }; let five = (!two);"
    it "test parser basic return" $
      do 
        testBasicReturn
        `shouldBe`
        "return 5; return 'five'; return (-5); return (5 + 5); return (5 + (5 * 5)); return ((5 * 5) + 5); return (5 + (-5)); return (((5 + 5)) * 5); return five; return True; return 5 == 5; return [1, 2, ]; return {1:1, 'hello':'world', }; return (!five);"
    it "test parser basic assign" $
      do 
        testBasicAssign
        `shouldBe`
        "five = 5; five = 'five'; five = (-5); five = (5 + 5); five = (5 + (5 * 5)); five = ((5 * 5) + 5); five = (5 + (-5)); five = (((5 + 5)) * 5); five = five; five = True; five = 5 == 5; five = [1, 2, ]; five = {1:1, 'hello':'world', }; five = (!five);"
    it "test parser am" $
      do 
        testAM
        `shouldBe`
        "return [(!True), {1:(!True), '2':[True, {0:((2 + (-3))), }, ], }, ];"
    it "test parser fn" $
      do 
        testFN
        `shouldBe`
        "fn add(){}; fn add(a){let five = 5;}; fn add(a,b){let c = (a + b); return c;}; fn a(a,b){fn b(a,b){return 5;}; b(1,2);}; fn a(){if(True){let five = 5; return 5;}; return 2;}; fn a(){fn a(){fn a(){fn a(){fn a(a,b){let five = [{1:[{1:[{1:[], }, ], }, ], }, ];};};};};}; fn add(((2 + 3))){};"
    it "test parser fnexp" $
      do 
        testFNExp
        `shouldBe`
        "fn add(a){let a = 5; let a = '5'; let a = True; let a = a[0]; let a = {1:1, }; let a = [0, 1, 2, ]; add(2,3);};"
    it "test parser index" $
      do 
        testIndex
        `shouldBe`
        "let a = b[0]; let a = b['c']; let a = b[c]; let a = b[(1 + 2)]; let a = b[((1 + (-5)))]; let a = b[0]; let a = b[0][0]; let a = b[b[b[b[0]]]]; let a = a[add(2,3)]; let a = b[b[0][0]][0];"
    it "test parser call" $
      do 
        testCall
        `shouldBe`
        "add(); add(1); add(1,2); add(a,b); add('a'); add((1 + 2)); add(((1 + (-5)))); add(sub()); add(a[0]);"
    it "test parser if params" $
      do 
        testIfParams
        `shouldBe`
        "if(True){}; if(1 == 1){}; if(a > 1){}; if(add()){}; if('a' == 'a'){}; if(True == False){}; if(a[0]){};"
    it "test parser if" $
      do 
        testIf
        `shouldBe`
        "if(True){let five = 5; return five;}; if(True){return a[0];}; if(True){return True;}; if(True){return 'hi';}; if(True){if(True){};};"
    it "test parser if exp" $
      do 
        testIfExp
        `shouldBe`
        "if(True){let five = a[0]; let b = {1:1, }; add(2,3);};"
    it "test parser else" $
      do 
        testElse
        `shouldBe`
        "if(True){}else{let five = 5;}; if(True){if(True){if(True){}else{return 5;};}else{return 5;};}else{if(True){if(True){}else{return 5;};}else{return 5;};};"
    it "test parser diff" $
      do 
        testDiff
        `shouldBe`
        "fn a(){fn a(){fn a(){fn a(){fn a(a,b){let five = [{1:[{1:[{1:[], }, ], }, ], }, ]; if(True){if(True){if(True){}else{return 5;};}else{return 5;};}else{if(True){if(True){}else{return 5;};}else{return 5;};};};};};};};"

