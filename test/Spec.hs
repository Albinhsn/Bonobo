module Main where

import Test.Hspec 
import TestDifficult
import TestLexer
import TestParser
import TestPrecedence
import TestBool
import TestIf 
import TestNoSta
import TestIdent
import TestFunc
import TestArray
import TestMap
import TestFor
import TestCode
import TestVM

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
    it "testing massive am" $ 
      do 
        testMassiveAM
        `shouldBe`
        "let a = [{1:[{1:[], }, ], }, ]; let b = {1:[{1:[], }, ], };"
    it "testing massive am 2" $ 
      do 
        testMassiveAM2
        `shouldBe`
        "let a = [{'a':[0, {'b':True, }, ], }, ];"
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
  describe "test code" $ do
    it "test make " $
      do 
        testMake 
        `shouldBe`
        "000121 - 1 2 "
    it "test make 2" $
      do 
        testMake2 
        `shouldBe`
        " CONST 2 CONST 2 MUL CONST 3 CONST 2 MUL ADD POP"
    it "test code if" $
      do 
        testCodeIf
        `shouldBe`
        " TRUE JUMPNT 7 CONST 5 POP JUMP 2"
    it "test code if else" $
      do 
        testCodeIfElse
        `shouldBe`
        " TRUE JUMPNT 7 CONST 5 POP JUMP 5 CONST 10 POP"
    it "test code let" $
      do 
        testCodeLet
        `shouldBe`
        " CONST 5 SETGLOBAL five GETGLOBAL five POP"
    it "test code array 1" $
      do 
        testCodeArray1
        `shouldBe`
        " ARRAYEND ARRAY POP"
    it "test code array 2" $
      do 
        testCodeArray2
        `shouldBe`
        " ARRAYEND CONST 'hi' CONST 1 ARRAY POP" 
    it "test code array 3" $
      do 
        testCodeArray3
        `shouldBe`
        " ARRAYEND ARRAYEND CONST 2 CONST 1 ARRAY ARRAYEND ARRAY ARRAY SETGLOBAL a"
    it "test code array 4" $
      do 
        testCodeArray4
        `shouldBe`
        " ARRAYEND ARRAYEND ARRAYEND CONST 2 CONST 1 ARRAY ARRAY ARRAY SETGLOBAL a"
    it "test code array 5" $
      do 
        testCodeArray5
        `shouldBe`
        " ARRAYEND ARRAYEND ARRAY ARRAYEND CONST 2 CONST 1 ARRAY ARRAY SETGLOBAL a"
    it "test code map 1" $
      do 
        testCodeMap1
        `shouldBe`
        " HASHEND CONST 4 CONST 4 CONST 3 CONST 3 CONST 2 CONST 2 CONST 1 CONST 1 HASH SETGLOBAL a"
    it "test code index 1" $
      do 
        testCodeIndex1
        `shouldBe`
        " ARRAYEND CONST 0 ARRAYEND CONST 2 CONST 1 ARRAY ARRAY SETGLOBAL b GETGLOBAL b CONST 0 INDEX CONST 1 INDEX SETGLOBAL a"
    it "test code index 2" $
      do 
        testCodeIndex2
        `shouldBe`
        " ARRAYEND CONST 0 ARRAYEND CONST 2 CONST 1 ARRAY ARRAY SETGLOBAL a CONST 1 CONST 0 TRUE GETGLOBAL a SETINDEX SETGLOBAL a"
    it "test code index 3" $
      do 
        testCodeIndex3
        `shouldBe`
        " ARRAYEND CONST 0 ARRAYEND CONST 2 CONST 1 ARRAY ARRAY SETGLOBAL a CONST 0 TRUE GETGLOBAL a SETINDEX SETGLOBAL a"
    it "test code fn" $
      do 
        testCodeFN
        `shouldBe`
        " CONST 0 CONST 1 ADD RETURNVALUE"
    it "test make true" $
      do 
        testMakeTFTrue 
        `shouldBe`
        "61"
    it "test make false" $
      do 
        testMakeTFFalse
        `shouldBe`
        "71"
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
        "Stack:  Globals: 0 = 5"
    it "test vm let book 1" $
      do 
        testVMLetBook1
        `shouldBe`
        "Stack:  Globals: 0 = 1 "
    it "test vm let book 2" $
      do 
        testVMLetBook2
        `shouldBe`
        "Stack:  Globals: 0 = 1 1 = 2 "
    it "test vm let book 2" $
      do 
        testVMLetBook3
        `shouldBe`
        "Stack:  Globals: 0 = 1 1 = 2 "
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
