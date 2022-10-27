module TestUtils where 

import Parser
import Ast 
import Lexer 
import Token 
import Utils 


parseStringToStatementsString :: String -> String 
parseStringToStatementsString s =  statementsToString(snd(snd( parseStatements(EXP,( getTokens(parseTokens(0, s, [])),[])))))

parseStringToStatements :: String -> [Statement] 
parseStringToStatements s = snd(snd( parseStatements(EXP,( getTokens(parseTokens(0, s, [])),[]))))



