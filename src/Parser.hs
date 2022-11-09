module Parser where

import Ast
import Lexer
import Token
import Utils
import ParserUtils



parseStatements :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseStatements (b, (t, s)) = (bok, (tokens, statements))
  where
    (bok, (tokens, statements))
      | null t == True = (b, (t, s))
      | typ (head t) == FUNCTION = 
        parseStatements(
          (parseIdent(
            EXP, (
              removeFirst t,
              noPopAddToStatement(b,s, Statement{
                closedSta = False, 
                staLine = line (head t), 
                statementType = FUNCSTA, 
                statementUni = FuncStatement{params = [], body = []},
                expression = Expression {expLine = line (head t), expressionType = EMPTYEXP}
              })
            )
          ))
        )
      | typ (head t) == IDENT = 
        parseStatements(
          parseIdent(
            b, 
              (removeFirst t, 
              noPopAddToStatement(b, s, Statement{
                closedSta = False, 
                staLine = line (head t), 
                statementType = NOSTA,
                statementUni = NoStatement{},
                expression = IdentExpression {
                  expLine = line (head t),
                  expressionType = IDENTEXP, 
                  ident= head t
                  }}
                )
              )
          )
        )
      | typ (head t) == LET = 
          parseStatements(
            parseExpression(
                b, ( 
                  removeFirst(removeFirst t), 
                  noPopAddToStatement(
                    b, 
                    s, 
                    parseIdentifierToLet(removeFirst t)
                  )
                )
              )
            )
      | typ (head t) == FOR = 
        parseStatements(
          parseLPAREN(
            EXP, (
              removeFirst t, 
              noPopAddToStatement(
                b, s, Statement{
                    closedSta = False, 
                    staLine = line (head t),
                    statementType = FORSTA,
                    expression = Expression{
                      expLine = 0, 
                      expressionType = EMPTYEXP
                      },
                    statementUni = ForStatement{
                      start = Expression{expLine = line (head t), expressionType = EMPTYEXP},
                      stop = Expression{expLine = line (head t), expressionType = EMPTYEXP},
                      inc = Expression{expLine = line (head t), expressionType = EMPTYEXP},
                      forBody = []
                      }
                  } 
            )
          )
        ))
      | typ (head t) == RETURN =
          parseStatements 
            (parseExpression
                (b,( removeFirst t,
                    noPopAddToStatement(b, s, Statement
                           { 
                            closedSta = False, 
                            staLine = line (head t), 
                             statementUni = ReturnStatement {},
                             statementType = RETSTA,  
                             expression = Expression {expLine = line (head t), expressionType = EMPTYEXP}
                           }
                ))
            ))
      | typ (head t) == RBRACE && b == CON = parseStatements(parseElse(ALT, (removeFirst t, closeLastOpen(b,s))))
      | typ (head t) == RBRACE && b == ALT = parseStatements(parseSemicolon(findLastBlockType(EXP, last s), (removeFirst t, closeLastOpen(b,s))))
      | typ (head t) == RBRACE && b == BOD = parseStatements(parseSemicolon(EXP, (removeFirst t, s)))
      | typ (head t) == RBRACE && b == EXP && statementType(last s) == FUNCSTA = parseExpression(EXP, (removeFirst t, s))
      | typ (head t) == IF = 
        parseExpression(
        CON, (removeFirst t, 
        noPopAddToStatement(b, s, Statement {
          closedSta = False, 
          staLine = line (head t), 
          statementType = IFSTA,
          statementUni = IfStatement{closedCon = False, con = [], alt = [], closedAlt = True},
          expression = Expression {expLine = line (head t), expressionType = EMPTYEXP}})))
      | typ (head t) == EOF = (b, (removeFirst t, s))
      | typ (head t) == SEMICOLON = parseStatements(b, (removeFirst t, s))
      | typ (head t) == ELSE = parseStatements(parseElse(b, (t, s)))
      -- REMOVE THIS AFTER TESTING COMPILER
      | otherwise = parseExpression(b,(t,s))--error ("error parsing statement: " ++ (literal (head t)) ++ " on line: "++ (show (line (head t))))

parseSemicolon:: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement])) 
parseSemicolon (b, (t, s)) = (bok, (tok, sta))
  where
    (bok, (tok, sta))
      | null t || typ (head t) == EOF = error (show b) 
      | (b == ALT || b == CON) && typ (head t) == ELSE = (b, (t, s))
      | typ (head t) /= SEMICOLON = error "expected semicolon" 
      | otherwise = (b, (removeFirst t, s))

parseLPAREN:: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement])) 
parseLPAREN(b, (t, s)) = (bok, (tok, sta))
  where
    (bok, (tok, sta))
      | typ (head t) == LPAREN = parseExpression(START, (removeFirst t, s))
      | otherwise = error ("expected '(' after for at line: " ++ (show (staLine (last s))))

advForBlock :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement])) 
advForBlock (b, (t, s)) = (bok, (tok, sta))
  where
    (bok, (tok, sta))
      | b == START =parseExpression(STOP,(t,s)) 
      | b == STOP = parseExpression(INC,(t,s)) 
      | b == BOD && typ (head t) == RPAREN = advForBlock(INC, (removeFirst t,s))
      | b == BOD && typ (head t) == LBRACE = parseStatements(BOD, (removeFirst t,s)) 
      | b == INC = advForBlock(BOD, (t, s))
      | otherwise = (b,(t,s)) 


parseIdent :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement])) 
parseIdent (b, (t, s)) = (bok, (tok, sta))
  where
    (bok, (tok, sta))
      | null t == True = (b, (t, s))
      | typ (head t) == LPAREN && b == EXP && statementType (last s) == NOSTA = parseIdent(
          PAR, 
          (removeFirst t,changeSta(b,head t, s))
        ) 
      | typ (head t) == LPAREN && b == EXP = parseIdent(
          PAR, 
          (removeFirst t, s))
      | typ (head t) == RPAREN && b == PAR= parseFunc(b, (t, s))
      | typ (head t) == ASSIGN && b == EXP = parseExpression(
        b, 
        (
          removeFirst t,
          addToStatement(b, s, Statement{
              closedSta = False, 
              staLine = line (head t),
              statementType = ASSIGNSTA, 
              statementUni = AssignStatement{},
              expression = AssignExpression {expLine = line (head t), expressionType = ASSIGNEXP, assignIdent = getLastNestedExpression(b, s), assignExpression = Expression{expLine = line (head t), expressionType = EMPTYEXP}}
            })
        )
      ) 
      | typ (head t) == IDENT = parseIdent(b, (removeFirst t, addToStatement(b, s, addToLastStatement(b, head t, IDENTEXP, s))))
      | typ (head t) == COMMA && b == PAR = parseIdent(b, (removeFirst t, pop s ++ [addEmptyToLastParam(last s)]))
      | typ (head t) == LBRACKET = parseIdent(parseExpression(b, (removeFirst t, pop s ++ [addToLastStatement(b, head t, ARRAYEXP, s)])))
      | typ (head t) == EOF = (b, (t, s))
      | otherwise = parseExpression(b, (t,s )) 


isValidReturn :: Statement -> Bool 
isValidReturn s = 
  case statementType s of 
    FUNCSTA -> True 
    _ -> False


parseIf :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement])) 
parseIf (b, (t, s)) = (bok, (tok, sta))
  where
    (bok, (tok, sta))
      | null t == True = (b, (t, s))
      | typ (head t) == LBRACE && hasValidCondition(b, s) = parseStatements(CON, (removeFirst t, s))
      | typ (head t) == LBRACE = error ("doesn't have grouped + bool in if on line: " ++ (show (line (head t))))
      | typ (head t) == IF || typ (head t) == LET || typ (head t) == IDENT = parseStatements(b, (t, s))
      | typ (head t) == RETURN && isValidReturn(last s)= parseStatements(b,(t,s))
      | typ (head t) == RETURN = error ("not valid returntoken on line: " ++ (show(line(head t))))
      | typ (head t) == EOF || typ (head t) == RBRACE = (b, (removeFirst t, s))
      | otherwise = error ("parsing if: " ++ (literal (head t)) ++ " on line: " ++ statementToString(last s)) 

parseElse :: (BlockType, ([Token], [Statement])) ->(BlockType, ([Token], [Statement]))
parseElse (b,(t,s)) = (blo, (tok,sta))
  where 
    (blo, (tok, sta))
      | null t == True = (b, (t, s))
      | typ (head t) == EOF || typ (head t) == SEMICOLON= parseStatements(b, (removeFirst t, s))
      | typ (head t) == ELSE  = parseElse(ALT, (removeFirst t, openLastAlt(s)))
      | typ (head t) == LBRACE = parseElse(b, (removeFirst t, s))
      | typ (head t) == IF || typ (head t) == LET || typ (head t) == IDENT = parseStatements(b, (t, s))
      | typ (head t) == RETURN && isValidReturn(last s)= parseStatements(b,(t,s))
      | typ (head t) == RETURN = error ("not valid returntoken in " ++ (show(line(head t))))
      | typ (head t) == RBRACE = (b, (removeFirst t, closeLastOpen(b, s))) 
      | otherwise = error ("parse else on line: " ++ (show (line (head t))))

parseFunc:: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseFunc(b, (t, s)) = (block, (tokens, statements))
  where
    (block, (tokens, statements))
      | null t == True = (b, (t, s))
      | typ (head t) == LBRACE  && hasValidParams(s) = parseStatements(BOD, (removeFirst t, s))
      | typ (head t) == LBRACE = error ("non valid params for func on line: "++ (show(line(head t))))
      | typ (head t) == RPAREN && b == PAR && paramHasOpen(b, s) = parseFunc(
        b, 
        (
          removeFirst t, 
          pop s ++ [Statement{
            closedSta = False, 
            staLine = line (head t),
            statementType = statementType (last s),
            statementUni = FuncStatement{
                params = pop (params (statementUni (last s))) ++[closeLastGrouped(last(params(statementUni(last s))))], 
                body = []
              },
            expression =expression (last s)
          }] 
        )
        )

      | typ (head t) == RPAREN && b == PAR = parseFunc(EXP, (removeFirst t, s)) 
      | typ (head t) == EOF = (b, (t,changeFuncToCall s))
      | otherwise = (EXP, (t, changeFuncToCall s)) 

parseExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseExpression (b, (t, s)) = (block, (tokens, statements))
  where
    (block, (tokens, statements))
      | null t = (b, (t, s))
      | typ (head t) == RPAREN && b == PAR && paramHasOpen(b, s) = parseExpression(
        b, 
        (
          removeFirst t, 
          pop s ++ [Statement{
            closedSta = False, 
            staLine = line (head t),
            statementType = statementType (last s),
            statementUni = FuncStatement{
                params = pop (params (statementUni (last s))) ++[closeLastGrouped(last(params(statementUni(last s))))], 
                body = []
              },
            expression =expression (last s)
          }] 
        )
        )
      | typ (head t) == RPAREN && b == PAR = parseFunc(PAR, (t, s)) 
      | typ (head t) == RBRACE && isListExpression(b, last s) == False&& b == BOD= parseExpression(EXP, (removeFirst t, s)) 
      | typ (head t) == RBRACE && isListExpression(b, last s) == False && b == CON = parseIf(CON, (t, s))
      | typ (head t) == RBRACE && isListExpression(b, last s) == False && b == ALT = parseElse(ALT, (t, s))
      | typ (head t) == RBRACE && isListExpression(b, last s) && isValidMap(s) = parseExpression(b, (removeFirst t, s))
      | typ (head t) == COMMA && b == PAR = parseExpression(
        b, 
        (
        removeFirst t, pop s ++ [addEmptyToLastParam(last s)]))
      | typ (head t) == IDENT = 
        parseExpression(
          parseIdent(
              (b, (removeFirst t, addToStatement(b, s, addToLastStatement(b, head t, IDENTEXP, s))))
          )
        )
      | typ (head t) == ASSIGN = 
        parseExpression (
          (b, (removeFirst t, changeSta(b, head t, addToStatement(b, s, addToLastStatement(b, head t, ASSIGNEXP, s))))
        ))
      | typ (head t) == INT =
        parseExpression (
          parseIntegerExpression (b, (t, s))
          )
      | 
          typ (head t) == MINUS
        && 
          isValidMinus (getLastExpression s) =
            parseExpression (
              parsePrefixExpression (b, (t, s))
            )
      | typ (head t) == STRING = 
        parseExpression(
          parseStringExpression(b, (t, s))
        )
      | isOperator (head t) = 
        parseExpression (
          parseOperatorExpression (b, (t, s))
          )
      | isValidPrefix(head t)=
        parseExpression (
          parsePrefixExpression (b, (t,s ))
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
      | typ (head t) == LBRACKET = parseExpression(b, (removeFirst t, pop s ++ [addToLastStatement(b, head t, ARRAYEXP, s)])) 
      | typ (head t) == RBRACKET = parseExpression(b, (removeFirst t, pop s ++ [closeLastIndex(b, last s)]))
      | typ (head t) == COMMA && b /= PAR = parseExpression(b, (removeFirst t, pop s ++ [addToLastStatement(b, head t, ARRAYEXP, s)]))
      | typ (head t) == COLON  = parseExpression(b, (removeFirst t, pop s ++ [addToLastStatement(b, head t, MAPEXP, s)]))
      | typ (head t) == RPAREN = 
        parseExpression(b, (
          removeFirst t, pop s ++ [addToLastStatement(b, head t, GROUPEDEXP, s)]))
      | typ (head t) == SEMICOLON = parseStatements(advForBlock(b, (removeFirst t, s)))
      | typ (head t) == EOF = (b, (removeFirst t, s))
      | typ (head t) == LBRACE && isListExpression(b, last s)= parseExpression(b, (removeFirst t, pop s ++ [addToLastStatement(b, head t, MAPEXP, s)]))
      | typ (head t) == LBRACE = parseIf(b, (t,s))
      | otherwise = error ("error parsing expression" ++ (literal (head t)) ++ " on "++ (show (line (head t))))



parseGroupedExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseGroupedExpression (b, (t, s)) = (b,(removeFirst t, addToStatement(b, s, addToLastStatement(b, head t, GROUPEDEXP, s))))

parseIntegerExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseIntegerExpression (b, (t, s)) = (b, (removeFirst t, addToStatement (b, s, addToLastStatement(b, head t, INTEXP, s))))

parseOperatorExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseOperatorExpression (b, (t, s)) = (b, (removeFirst t, addToStatement (b, s, addToLastStatement(b,head t, OPERATOREXP, s))))

parsePrefixExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parsePrefixExpression (b, (t, s)) = (b, (removeFirst t, addToStatement(b,s, addToLastStatement(b, head t, PREFIXEXP, s))))

parseBoolExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseBoolExpression (b, (t, s)) = (b, (removeFirst t, addToStatement(b,s,addToLastStatement(b, head t, BOOLEXP, s))))

parseTFExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseTFExpression (b, (t,s )) = (b, (removeFirst t, addToStatement(b,s,addToLastStatement(b, head t, TFEXP, s))))

parseStringExpression :: (BlockType, ([Token], [Statement])) ->(BlockType, ([Token], [Statement]))
parseStringExpression (b,(t,s)) = (b, (removeFirst t, addToStatement(b,s,addToLastStatement(b, head t, STRINGEXP, s)))) 
