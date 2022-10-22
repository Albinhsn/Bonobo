module Parser where

import Ast
import Lexer
import Token
import Utils
import ParserUtils


parseProgram :: String -> [Token]
parseProgram s = snd (parseTokens (s, []))

parseStatements :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseStatements (b, (t, s)) = (bok, (tokens, statements))
  where
    (bok, (tokens, statements))
      | null t == True = (b, (t, s))
      | typ (head t) == FUNCTION = 
        parseStatements(
          (b, snd(parseExpression(
            b, (
              removeFirstToken t,
              noPopAddToStatement(b,s, Statement{
              statementType = FUNCSTA, 
              statementUni = FuncStatement{params = [], body = []},
              expression = Expression {expressionType = EMPTYEXP}
              })
            ))
          ))
        )
      | typ (head t) == IDENT = 
        parseStatements(
          (b, snd (parseExpression(
            b, (removeFirstToken t, 
              noPopAddToStatement(b, s, Statement{
                statementType = ASSIGNSTA,
                statementUni = AssignStatement{},
                expression = IdentExpression {
                  expressionType = IDENTEXP, 
                  ident= literal (head t)
                  }}
                )
              )
            )
        )))
      | typ (head t) == LET =
          parseStatements
            (b,
            ( snd (parseExpression(parseIdentifierToLet(b, ( removeFirstToken t, noPopAddToStatement(b, s, Statement {statementType = LETSTA, statementUni = LetStatement{identifier = ""}, expression = Expression {expressionType = EMPTYEXP}})))))))
      | typ (head t) == RETURN =
          parseStatements 
            (parseExpression
                (b,( removeFirstToken t,
                    noPopAddToStatement(b, s, Statement
                           { 
                             statementUni = ReturnStatement {},
                             statementType = RETSTA,  
                             expression = Expression {expressionType = EMPTYEXP}
                           }
                ))
            ))
      | typ (head t) == RBRACE && b == CON = parseElse(ALT, (removeFirstToken t, closeLastOpen(b,s)))
      | typ (head t) == RBRACE && b == ALT = parseStatements(EXP, (removeFirstToken t, closeLastOpen(b,s)))
      | typ (head t) == LBRACE = error "found lbrace in parse statements"
      | typ (head t) == IF = parseStatements(
        parseIf (parseExpression(b, (removeFirstToken t, 
        noPopAddToStatement(b, s, Statement {
          statementType = IFSTA,
          statementUni = IfStatement{closedCon = False, con = [], alt = [], closedAlt = False},
          expression = Expression {expressionType = EMPTYEXP}})))))
      | typ (head t) == EOF = (b, (removeFirstToken t, s))
      | otherwise = error "error parsing statement" 


parseIf :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement])) 
parseIf (b, (t, s)) = (bok, (tok, sta))
  where
    (bok, (tok, sta))
      | null t == True = (b, (t, s))
      | typ (head t) == LBRACE && b == ALT = parseStatements(b, (removeFirstToken t, s))
      | typ (head t) == LBRACE && b == EXP = parseStatements(CON, (removeFirstToken t, s))
      | typ (head t) == IF || typ (head t) == RETURN || typ (head t) == LET= parseStatements(b, (t, s))
      | typ (head t) == EOF = (b, (t, s))
      | otherwise = error "parsing if"

parseElse :: (BlockType, ([Token], [Statement])) ->(BlockType, ([Token], [Statement]))
parseElse (b,(t,s)) = (blo, (tok,sta))
  where 
    (blo, (tok, sta))
      | null t == True = (b, (t, s))
      | typ (head t) == ELSE || typ (head t) == LBRACE = parseElse(ALT, (removeFirstToken t, s))
      | typ (head t) == IF || typ (head t) == RETURN || typ (head t) == LET= parseStatements(b, (t, s))
      | typ (head t) == RBRACE = (b, (removeFirstToken t, closeLastOpen(b, s))) 
      | typ (head t) == EOF = (b, (t, s))
      | otherwise = error "parse else" 

parseExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseExpression (b, (t, s)) = (block, (tokens, statements))
  where
    (block, (tokens, statements))
      | null t = (b, (t, s))
      | typ (head t) == IDENT = 
        parseExpression (
            (b, (removeFirstToken t, addToStatement(b, s, addToLastStatement(b, head t, IDENTEXP, s))))
        )
      | typ (head t) == ASSIGN = 
        parseExpression (
          (b, (removeFirstToken t, addToStatement(b, s, addToLastStatement(b, head t, ASSIGNEXP, s))))
        )
      | typ (head t) == INT =
        parseExpression (
          parseIntegerExpression (b, (t, s))
          )
      | 
          isValidMinus (getLastExpression s)  
        && 
          typ (head t) == MINUS = 
            parseExpression (
              parseInfixExpression (b, (t, s))
            )
      | isOperator (head t) = 
        parseExpression (
          parseOperatorExpression (b, (t, s))
          )
      | isValidInfix (head t)=
        parseExpression (
          parseInfixExpression (b, (t,s ))
          )
      | isBoolPrefix (head t) =
        parseExpression (
          parseBoolExpression (b, (t, s))
        )
      | isTF (head t) = 
        parseExpression(
          parseTFExpression (b, (t, s))
        )
      | typ (head t) == LPAREN = 
        parseExpression (
          parseGroupedExpression(b, (t, s))
          )
      | typ (head t) == RPAREN = 
        parseExpression(b, (
          removeFirstToken t, pop s ++ [addToLastStatement(b, head t, GROUPEDEXP, s)]))
      | typ (head t) == SEMICOLON = (b, (removeFirstToken t, s))
      | typ (head t) == EOF = (b, (removeFirstToken t, s))
      | typ (head t) == RBRACE && b == CON = (CON, (removeFirstToken t, closeLastOpen(b, s)))
      | typ (head t) == RBRACE && b == ALT = (EXP, (removeFirstToken t, closeLastOpen(b,s)))
      -- After parsing the condition call parseIf
      | typ (head t) == LBRACE && statementType (last s) == IFSTA && b == EXP = parseIf(b, (t, s))
      | otherwise = error "error parsing expression" 

parseGroupedExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseGroupedExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement(b, s, addToLastStatement(b, head t, GROUPEDEXP, s))))

parseIntegerExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseIntegerExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement (b, s, addToLastStatement(b, head t, INTEXP, s))))

parseOperatorExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseOperatorExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement (b, s, addToLastStatement(b,head t, OPERATOREXP, s))))

parseInfixExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseInfixExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement(b,s, addToLastStatement(b, head t, INFIXEXP, s))))

parseBoolExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseBoolExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement(b,s,addToLastStatement(b, head t, BOOLEXP, s))))

parseTFExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseTFExpression (b, (t,s )) = (b, (removeFirstToken t, addToStatement(b,s,addToLastStatement(b, head t, TFEXP, s))))
