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
              removeFirstToken t,
              noPopAddToStatement(b,s, Statement{
              statementType = FUNCSTA, 
            statementUni = FuncStatement{params = [], body = []},
            expression = Expression {expressionType = EMPTYEXP}
              })
            )
          ))
        )
      | typ (head t) == IDENT = 
        parseStatements(
          parseIdent(
            b, 
              (removeFirstToken t, 
              noPopAddToStatement(b, s, Statement{
                statementType = NOSTA,
                statementUni = NoStatement{},
                expression = IdentExpression {
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
                  removeFirstToken (removeFirstToken t), --This is bad but first one is removed from parseIdentifierToLet 
                  noPopAddToStatement(
                    b, 
                    s, 
                    parseIdentifierToLet(removeFirstToken t)
                  )
                )
              )
            )
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
      | typ (head t) == RBRACE && b == CON = parseStatements(parseElse(ALT, (removeFirstToken t, closeLastOpen(b,s))))
      | typ (head t) == RBRACE && b == ALT = parseStatements(findLastBlockType(EXP, last s), (removeFirstToken t, closeLastOpen(b,s)))
      | typ (head t) == RBRACE && b == BOD = parseStatements(EXP, (removeFirstToken t, s))
      | typ (head t) == RBRACE && b == EXP && statementType(last s) == FUNCSTA = parseStatements(EXP, (removeFirstToken t, s))
      | typ (head t) == IF = 
        parseExpression(
        CON, (removeFirstToken t, 
        noPopAddToStatement(b, s, Statement {
          statementType = IFSTA,
          statementUni = IfStatement{closedCon = False, con = [], alt = [], closedAlt = False},
          expression = Expression {expressionType = EMPTYEXP}})))
      | typ (head t) == EOF = (b, (removeFirstToken t, s))
      | typ (head t) == SEMICOLON = parseStatements(b, (removeFirstToken t, s))
      | typ (head t) == ELSE = parseStatements(parseElse(b, (t, s)))
      | otherwise = error ("error parsing statement: " ++ (literal (head t)) ++ " on line: "++ (show (line (head t))))


parseIdent :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement])) 
parseIdent (b, (t, s)) = (bok, (tok, sta))
  where
    (bok, (tok, sta))
      | null t == True = (b, (t, s))
      | typ (head t) == LPAREN && b == EXP && statementType (last s) == NOSTA = parseIdent(
          PAR, 
          (removeFirstToken t,changeSta(head t, s))
        ) 
      | typ (head t) == LPAREN && b == EXP = parseIdent(
          PAR, 
          (removeFirstToken t, s))
      | typ (head t) == RPAREN && b == PAR= parseFunc(b, (t, s))
      | typ (head t) == ASSIGN && b == EXP = parseExpression(
        b, 
        (
          removeFirstToken t,
          addToStatement(b, s, Statement{
              statementType = ASSIGNSTA, 
              statementUni = AssignStatement{},
              expression = AssignExpression {expressionType = ASSIGNEXP, assignIdent = getLastNestedExpression(b, s), assignExpression = Expression{expressionType = EMPTYEXP}}
            })
        )
      ) 
      | typ (head t) == IDENT = parseIdent(b, (removeFirstToken t, addToStatement(b, s, addToLastStatement(b, head t, IDENTEXP, s))))
      | typ (head t) == COMMA && b == PAR = parseIdent(b, (removeFirstToken t, pop s ++ [addEmptyToLastParam(last s)]))
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
      | typ (head t) == LBRACE && hasValidCondition(b, s) = parseStatements(CON, (removeFirstToken t, s))
      -- | typ (head t) == LBRACE = parseStatements(CON, (removeFirstToken t, s))
      | typ (head t) == LBRACE = error ("doesn't have grouped + bool in if on line: " ++ (show (line (head t))))
      | typ (head t) == IF || typ (head t) == LET || typ (head t) == IDENT = parseStatements(b, (t, s))
      | typ (head t) == RETURN && isValidReturn(last s)= parseStatements(b,(t,s))
      | typ (head t) == RETURN = error ("not valid returntoken on line: " ++ (show(line(head t))))
      | typ (head t) == EOF || typ (head t) == RBRACE = (b, (removeFirstToken t, s))
      | otherwise = error ("parsing if: " ++ (literal (head t)) ++ " on line: " ++ statementToString(last s)) 

parseElse :: (BlockType, ([Token], [Statement])) ->(BlockType, ([Token], [Statement]))
parseElse (b,(t,s)) = (blo, (tok,sta))
  where 
    (blo, (tok, sta))
      | null t == True = (b, (t, s))
      | typ (head t) == EOF || typ (head t) == SEMICOLON= parseStatements(b, (removeFirstToken t, s))
      | typ (head t) == ELSE || typ (head t) == LBRACE = parseElse(ALT, (removeFirstToken t, s))
      | typ (head t) == IF || typ (head t) == LET || typ (head t) == IDENT = parseStatements(b, (t, s))
      | typ (head t) == RETURN && isValidReturn(last s)= parseStatements(b,(t,s))
      | typ (head t) == RETURN = error ("not valid returntoken in " ++ (show(line(head t))))
      | typ (head t) == RBRACE = (b, (removeFirstToken t, closeLastOpen(b, s))) 
      | otherwise = error ("parse else on line: " ++ (show (line (head t))))

parseFunc:: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseFunc(b, (t, s)) = (block, (tokens, statements))
  where
    (block, (tokens, statements))
      | null t == True = (b, (t, s))
      | typ (head t) == LBRACE  && hasValidParams(s) = parseStatements(BOD, (removeFirstToken t, s))
      | typ (head t) == LBRACE = error ("non valid params for func on line: "++ (show(line(head t))))
      | typ (head t) == RPAREN && b == PAR && paramHasOpen(b, s) = parseFunc(
        b, 
        (
          removeFirstToken t, 
          pop s ++ [Statement{
            statementType = statementType (last s),
            statementUni = FuncStatement{
                params = pop (params (statementUni (last s))) ++[closeLastGrouped(last(params(statementUni(last s))))], 
                body = []
              },
            expression =expression (last s)
          }] 
        )
        )

      | typ (head t) == RPAREN && b == PAR = parseFunc(EXP, (removeFirstToken t, s)) 
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
          removeFirstToken t, 
          pop s ++ [Statement{
            statementType = statementType (last s),
            statementUni = FuncStatement{
                params = pop (params (statementUni (last s))) ++[closeLastGrouped(last(params(statementUni(last s))))], 
                body = []
              },
            expression =expression (last s)
          }] 
        )
        )
      | typ (head t) == RPAREN && b == PAR = parseFunc(PAR, (t, s)) --
      | typ (head t) == RBRACE && b == BOD= parseExpression(EXP, (removeFirstToken t, s)) --Only reason for parseExp is to remove SEMICOLON 
      | typ (head t) == RBRACE && b == CON = parseIf(CON, (t, s))
      | typ (head t) == RBRACE && b == ALT = parseElse(ALT, (t, s))
      | typ (head t) == COMMA && b == PAR = parseExpression(
        b, 
        (
        removeFirstToken t, pop s ++ [Statement{
            statementType = statementType (last s),
            statementUni = FuncStatement{
                params = params (statementUni (last s)) ++ [Expression{expressionType = EMPTYEXP}],
                body = []
              },
            expression = expression (last s) 
          }]))
      | typ (head t) == IDENT = 
        parseExpression(
          parseIdent(
              (b, (removeFirstToken t, addToStatement(b, s, addToLastStatement(b, head t, IDENTEXP, s))))
          )
        )
      | typ (head t) == ASSIGN = 
        parseExpression (
          (b, (removeFirstToken t, addToStatement(b, s, addToLastStatement(b, head t, ASSIGNEXP, s)))
        ))
      | typ (head t) == INT =
        parseExpression (
          parseIntegerExpression (b, (t, s))
          )
      | 
          isValidMinus (getLastExpression s)  
        && 
          typ (head t) == MINUS = 
            parseExpression (
              parsePrefixExpression (b, (t, s))
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
      | typ (head t) == RPAREN = 
        parseExpression(b, (
          removeFirstToken t, pop s ++ [addToLastStatement(b, head t, GROUPEDEXP, s)]))
      | typ (head t) == SEMICOLON = parseStatements(b, (removeFirstToken t, s))
      | typ (head t) == EOF = (b, (removeFirstToken t, s))
      | typ (head t) == LBRACE = parseIf(b, (t,s))
      | otherwise = error ("error parsing expression" ++ (literal (head t)) ++ " on "++ (show (line (head t))))

parseGroupedExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseGroupedExpression (b, (t, s)) = (b,(removeFirstToken t, addToStatement(b, s, addToLastStatement(b, head t, GROUPEDEXP, s))))

parseIntegerExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseIntegerExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement (b, s, addToLastStatement(b, head t, INTEXP, s))))

parseOperatorExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseOperatorExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement (b, s, addToLastStatement(b,head t, OPERATOREXP, s))))

parsePrefixExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parsePrefixExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement(b,s, addToLastStatement(b, head t, PREFIXEXP, s))))

parseBoolExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseBoolExpression (b, (t, s)) = (b, (removeFirstToken t, addToStatement(b,s,addToLastStatement(b, head t, BOOLEXP, s))))

parseTFExpression :: (BlockType, ([Token], [Statement])) -> (BlockType, ([Token], [Statement]))
parseTFExpression (b, (t,s )) = (b, (removeFirstToken t, addToStatement(b,s,addToLastStatement(b, head t, TFEXP, s))))
