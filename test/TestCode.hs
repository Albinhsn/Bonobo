module TestCode where 

import Data.ByteString as BS

import VM
import Code 
import Utils2
import Object



testMake :: String 
testMake = parseMakeToPretty(bytes (parseStatementToCompiled(parseStringToStatements("1 + 2"))), constants (parseStatementToCompiled(parseStringToStatements("1 + 2"))))
    
testMake2 :: String 
testMake2 = disassemble("", parseStatementToCompiled(parseStringToStatements("2 * 2 + 3 * 2")))

testMakeTFTrue :: String 
testMakeTFTrue = prettyPrint(bytes(parseStatementToCompiled(parseStringToStatements("True"))))

testMakeTFFalse :: String 
testMakeTFFalse = prettyPrint(bytes(parseStatementToCompiled(parseStringToStatements("False"))))

testCodeIf:: String 
testCodeIf= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){5;};")))

testCodeIfElse:: String 
testCodeIfElse= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){5;}else{10;};")))

testCodeNestedIf:: String 
testCodeNestedIf= disassemble("", parseStatementToCompiled(parseStringToStatements("if(True){if(False){3;}else{5;};};")))

testCodeNestedElse:: String 
testCodeNestedElse= disassemble("", parseStatementToCompiled(parseStringToStatements("if(False){}else{if(False){3;}else{5;};};")))

testCodeLet:: String 
testCodeLet= disassemble("", parseStatementToCompiled(parseStringToStatements("let five = 5; five;")))

testCodeArray1 :: String 
testCodeArray1 = disassemble("", parseStatementToCompiled(parseStringToStatements("[]")))

testCodeArray2 :: String 
testCodeArray2 = disassemble("", parseStatementToCompiled(parseStringToStatements("[1, \"hi\"]")))

testCodeArray3 :: String 
testCodeArray3 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[], [1,2]];")))

testCodeArray4 :: String 
testCodeArray4 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[[1, 2]]];")))

testCodeArray5 :: String 
testCodeArray5 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2], []];")))

testCodeMap1:: String 
testCodeMap1= disassemble("", parseStatementToCompiled(parseStringToStatements("let a = {1:1, 2:2, 3:3, 4:4}")))

testCodeIndex1 :: String 
testCodeIndex1 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2],0]; let b = a[0][1];")))

testCodeIndex2 :: String 
testCodeIndex2 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2],0]; a[0][1] = True;")))

testCodeIndex3 :: String 
testCodeIndex3 = disassemble("", parseStatementToCompiled(parseStringToStatements("let a = [[1,2],0]; a[0] = True;")))

testCodeFN :: String 
testCodeFN = disassembleFunc ("", funcValue(Prelude.head(constants(parseStatementToCompiled(parseStringToStatements("fn add(){return 5 + 10;};"))))))

disassembleFunc :: (String, ByteString) -> String 
disassembleFunc (s,b)= str 
  where 
    str 
      | BS.null b = s
      | BS.head b == 0 = disassembleFunc(s ++ " CONST " ++ (show (fromIntegral (BS.head (removeFirstInstruction b)))), removeFirstInstruction(removeFirstInstruction b))
      | BS.head b == 1 = disassembleFunc(s ++ " POP", removeFirstInstruction b)
      | BS.head b == 2 = disassembleFunc(s ++ " ADD", removeFirstInstruction b)
      | BS.head b == 3 = disassembleFunc(s ++ " SUB", removeFirstInstruction b)
      | BS.head b == 4 = disassembleFunc(s ++ " MUL", removeFirstInstruction b)
      | BS.head b == 5 = disassembleFunc(s ++ " DIV", removeFirstInstruction b)
      | BS.head b == 6 = disassembleFunc(s ++ " TRUE", removeFirstInstruction b)
      | BS.head b == 7 = disassembleFunc(s ++ " FALSE", removeFirstInstruction b)
      | BS.head b == 8 = disassembleFunc(s ++ " GT", removeFirstInstruction b)
      | BS.head b == 9 = disassembleFunc(s ++ " LT", removeFirstInstruction b)
      | BS.head b == 10 = disassembleFunc(s ++ " NEQ", removeFirstInstruction b)
      | BS.head b == 11 = disassembleFunc(s ++ " EQ", removeFirstInstruction b)
      | BS.head b == 12 = disassembleFunc(s ++ " MINUS", removeFirstInstruction b)
      | BS.head b == 13 = disassembleFunc(s ++ " BANG", removeFirstInstruction b)
      | BS.head b == 14 = disassembleFunc(s ++ " JUMP", removeFirstInstruction b)
      | BS.head b == 15 = disassembleFunc(s ++ " JUMPNT", removeFirstInstruction b)
      | BS.head b == 16 = disassembleFunc(s ++ " SETGLOBAL", removeFirstInstruction b)
      | BS.head b == 17 = disassembleFunc(s ++ " GETGLOBAL", removeFirstInstruction b)
      | BS.head b == 18 = disassembleFunc(s ++ " ARRAY", removeFirstInstruction b)
      | BS.head b == 19 = disassembleFunc(s ++ " ARRAYEND", removeFirstInstruction b)
      | BS.head b == 20 = disassembleFunc(s ++ " HASH", removeFirstInstruction b)
      | BS.head b == 21 = disassembleFunc(s ++ " HASHEND", removeFirstInstruction b)
      | BS.head b == 22 = disassembleFunc(s ++ " INDEX", removeFirstInstruction b)
      | BS.head b == 23 = disassembleFunc(s ++ " SETINDEX", removeFirstInstruction b)
      | BS.head b == 24 = disassembleFunc(s ++ " INDEXEND", removeFirstInstruction b)
      | BS.head b == 25 = disassembleFunc(s ++ " RETURNVALUE", removeFirstInstruction b)
      | BS.head b == 26 = disassembleFunc(s ++ " OPRETURN", removeFirstInstruction b)
      | otherwise = error ("disassemble " ++ (show (BS.head b)))
