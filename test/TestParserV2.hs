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
testFN = statementsToString(parseStatements(getTokens(parseTokens(0, "fn add(){}; fn add(a){let five = 5;}; ", []))))
