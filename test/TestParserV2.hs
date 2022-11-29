module TestParserV2 where 


import Ast
import Lexer
import Parser
import Token
import Utils

testBasicLet:: String 
testBasicLet= statementsToString(parseStatements(getTokens(parseTokens(0, "let five = 5; let five = \"five\"; let five = -5; let five = 5 + 5; let five = 5 + 5 * 5; let five = 5 * 5 + 5; let five = 5 + -5; let five = (5 + 5 ) * 5; let five = five; let five = True; let five = 5 == 5; let five = [1,2]; let five = {1:1, \"hello\":\"world\"}; let five = !two;", []))))
  
testBasicReturn :: String 
testBasicReturn = statementsToString(parseStatements(getTokens(parseTokens(0, "return 5; return \"five\";return -5; return 5 + 5; return 5 + 5 * 5; return 5 * 5 + 5; return 5 + -5; return (5 + 5 ) * 5; return five; return True; return 5 == 5; return [1,2]; return {1:1, \"hello\":\"world\"}; return !five;", []))))

testBasicAssign:: String
testBasicAssign = statementsToString(parseStatements(getTokens(parseTokens(0, "five = 5; five = \"five\"; five = -5; five = 5 + 5; five = 5 + 5 * 5; five =  5 * 5 + 5; five = 5 + -5; five = (5 + 5 ) * 5; five = five; five = True; five = 5 == 5; five = [1,2]; five = {1:1, \"hello\":\"world\"}; five = !five;", []))))

testAM :: String 
testAM = statementsToString(parseStatements(getTokens(parseTokens(0, "return [!True, {1:!True, \"2\": [True,{0:(2+ -3)}]}];", []))))

testFN :: String 
testFN = statementsToString(parseStatements(getTokens(parseTokens(0, "fn add(){}; fn add(a){let five = 5;}; fn add(a,b){let c = a + b; return c;}; fn a(a,b){fn b(a,b){return 5;}; b(1,2);}; fn a(){if(True){let five = 5; return 5;}; return 2;}; fn a(){fn a(){fn a(){fn a(){fn a(a,b){let five = [{1:[{1:[{1:[]}]}]}];};};};};};", []))))

testFNExp :: String 
testFNExp = statementsToString(parseStatements(getTokens(parseTokens(0, "fn add(a){let a = 5; let a = \"5\"; let a = True; let a = a[0]; let a = {1:1}; let a = [0,1,2]}; add(2,3);", []))))

testIndex :: String 
testIndex = statementsToString(parseStatements(getTokens(parseTokens(0, "let a = b[0]; let a = b[\"c\"]; let a = b[c]; let a = b[1+2]; let a = b[(1+-5)];  let a = b[0]; let a = b[0][0];", []))))

testCall :: String 
testCall = statementsToString(parseStatements(getTokens(parseTokens(0, "add(); add(1); add(1,2); add(a,b); add(\"a\"); add(1+2); add((1+-5)); add(sub());", []))))

testIfParams :: String 
testIfParams = statementsToString(parseStatements(getTokens(parseTokens(0, "if(True){}; if(1 == 1){}; if(a > 1){}; if(add()){}; if(\"a\" == \"a\"){}; if(True == False){}; if(a[0]){};", []))))

testIf :: String 
testIf = statementsToString(parseStatements(getTokens(parseTokens(0, "if(True){let five = 5; return five;};if(True){return a[0];};if(True){return True;};if(True){return \"hi\"}; if(True){if(True){};};", []))))

testIfExp :: String 
testIfExp = statementsToString(parseStatements(getTokens(parseTokens(0, "if(True){let five = a[0]; let b = {1:1}; add(2,3);};", []))))

testElse :: String 
testElse = statementsToString(parseStatements(getTokens(parseTokens(0, "if(True){}else{let five = 5;}; if(True){if(True){if(True){}else{return 5;};}else{return 5;};}else{if(True){if(True){}else{return 5;};}else{return 5;};};", []))))

testDiff :: String 
testDiff = statementsToString(parseStatements(getTokens(parseTokens(0, "fn a(){fn a(){fn a(){fn a(){fn a(a,b){let five = [{1:[{1:[{1:[]}]}]}];if(True){if(True){if(True){}else{return 5;};}else{return 5;};}else{if(True){if(True){}else{return 5;};}else{return 5;};}; };};};};};", []))))
