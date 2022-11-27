module Parser where 


import Token 
import Ast 
import Utils 
import Lexer


import Debug.Trace


parseStatements :: [Token] -> [Statement]
parseStatements t = parseT(t, [])

parseT :: ([Token], [Statement]) -> [Statement]
parseT (t,s) = 
  case typ (head t) of 
    --Statements
    FOR -> parseT(
      removeFirst t, 
      addStatement(s, Statement{
          closedSta = False,
          staLine = line (head t), 
          statementType = FORSTA, 
          statementUni = ForStatement{
              start = Expression{closedExp = False,expressionType = EMPTYEXP, expLine = -1},
              stop = Expression{closedExp = False,expressionType = EMPTYEXP, expLine = -1},
              inc = Expression{closedExp = False,expressionType = EMPTYEXP, expLine = -1},
              forBody = []
            },
          expression = Expression{closedExp = False,expressionType = EMPTYEXP, expLine = -1}
        }
      )
      ) 
    FUNCTION -> parseT(
      removeFirst t, 
      addStatement(s, Statement{
          closedSta = False,
          staLine = line (head t), 
          statementType = FUNCSTA, 
          statementUni = FuncStatement{
              closedParams = False, 
              params = [],
              body = []
            },
          expression = Expression{closedExp = False, expressionType = EMPTYEXP, expLine = -1}
        })
      ) 
    IDENT -> parseT(
      removeFirst t,
      addIdentifier(head t, s)
      )
    LET -> parseT(
      parseLet(
        removeFirst t, 
        s
          )
      )
    RETURN -> parseT(
      removeFirst t, 
      addStatement(s, Statement{
          closedSta = False,
          staLine = line (head t), 
          statementType = RETSTA, 
          statementUni = ReturnStatement{},
          expression = Expression{closedExp = False,expressionType = EMPTYEXP, expLine = -1}
        })
      ) 
    IF -> parseT(
      removeFirst t, 
      addStatement(s, Statement{
          closedSta = False,
          staLine = line (head t), 
          statementType = IFSTA, 
          statementUni = IfStatement{
              closedCon = False, 
              con = [],
              closedAlt = True, 
              alt = []
            },
          expression = Expression{closedExp = False,expressionType = EMPTYEXP, expLine = -1}
        })
      )
    ILLEGAL -> error ("illegal character: " ++ literal (head t)) 
    EOF -> s  
    SEMICOLON -> parseT(
      removeFirst t,
      append (pop s) (closeLast(last s))
      )
    COMMA -> parseT(
      removeFirst t,
      append (pop s) (addComma(last s))
      )
    ELSE ->parseT(
      addElse(t, s)
      ) 
    RBRACKET -> parseT(
      removeFirst t, 
      append (pop s) (closeArray(last s))
      ) 
    LBRACE -> parseT(
        removeFirst t, 
        append (pop s) (parseLBrace (last s))
      ) 
    RBRACE -> parseT(
        removeFirst t,
        append (pop s) (parseRBrace (last s))
      ) 
    RPAREN -> parseT(
        removeFirst t, 
        append (pop s) (closeLastParen (last s))
      )
    LPAREN -> parseT(
        removeFirst t,
        append (pop s) (addLParen (last s))
      )
    --Expression
    _ -> 
      parseT(
        removeFirst t, 
        append (pop s) (addToLastExpression(last s, head t))
      ) 


parseLet :: ([Token], [Statement]) -> ([Token], [Statement])
parseLet (t,s) = (tok, sta)
  where 
    (tok, sta)
      | typ (t!!0) /= IDENT = error ("can't have non ident after let: " ++ show (literal (head t)))  
      | typ (t!!1) /= ASSIGN = error ("can't have non assign after let + ident: " ++ show (literal (head t)))  
      -- TODO CHECK THIS
      | null s || closedSta (last s)= (removeFirst(removeFirst t),addStatement(s, Statement{
          staLine = line (head t), 
          closedSta = False,
          statementType = LETSTA, 
          statementUni = LetStatement{identifier = literal (head t)},
          expression = Expression{expLine = -1, expressionType = EMPTYEXP, closedExp = False} 
        }))
      | statementType (last s) == FUNCSTA && closedSta (last s) == False = (removeFirst(removeFirst t), append (pop s) Statement{
            staLine = staLine (last s), 
            closedSta = False,
            statementType = FUNCSTA, 
            statementUni = FuncStatement{
                closedParams = True, 
                params = params (statementUni (last s)),
                body = append (pop (body (statementUni (last s)))) Statement{
                    staLine = staLine (last s), 
                    closedSta = False,
                    statementType = LETSTA, 
                    statementUni = LetStatement{identifier = literal (head t)},
                    expression = Expression{expLine = -1, expressionType = EMPTYEXP, closedExp = False} 
                  }
          },
          expression = expression (last s)
        })

addStatement :: ([Statement], Statement) -> [Statement]
addStatement (s, sta) = state 
  where 
    state 
      | null s = [sta]
      | closedSta (last s) == True = append s sta 
      | statementType (last s) == LETSTA = append (pop s) sta
      -- TODO CHECK THIS
      | statementType (last s) == FUNCSTA && null (body (statementUni (last s))) = append (pop s) Statement{
          staLine = staLine (last s), 
          closedSta = False,
          statementType = FUNCSTA, 
          statementUni = FuncStatement{
              closedParams = True, 
              params = params (statementUni (last s)),
              body = [sta] 
            },
          expression = expression (last s)
        }
      | otherwise = error ("addStatement " ++ statementsToString s ++ " " ++ statementToString sta)

addToLastExpression :: (Statement, Token) -> Statement 
addToLastExpression (s, t) = sta
  where 
    sta 
      | 
        -- NORMAL STATEMENT ALWAYS EXPRESSION
        statementType s == LETSTA || 
        statementType s == RETSTA || 
        statementType s == ASSIGNSTA || 
        statementType s == NOSTA = Statement{
          closedSta = False,
          staLine = staLine s, 
          statementType = statementType s, 
          statementUni = statementUni s, 
          expression = addXToExp(expression s, t)
        }
        -- FORSTA START  
      | statementType s == FORSTA && closedExp (start (statementUni s)) == False = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s, 
          statementUni = ForStatement{
              start = addXToExp(start(statementUni s), t),
              stop = stop (statementUni s),
              inc = inc (statementUni s),
              forBody = []
            },
          expression = expression s
        }   
        -- FORSTA STOP 
      | statementType s == FORSTA && closedExp (stop (statementUni s)) == False = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s, 
          statementUni = ForStatement{
              start = start (statementUni s),
              stop = addXToExp(stop (statementUni s), t),
              inc = inc (statementUni s),
              forBody = []
            },
          expression = expression s
        }   
        -- FORSTA INC 
      | statementType s == FORSTA && closedExp (inc (statementUni s)) == False = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s, 
          statementUni = ForStatement{
              start = start (statementUni s),
              stop = stop (statementUni s),
              inc = addXToExp(inc (statementUni s), t),
              forBody = []
            },
          expression = expression s
        }   
        -- FORSTA EXP 
      | statementType s == FORSTA = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s, 
          statementUni = ForStatement{
              start = start (statementUni s),
              stop = stop (statementUni s),
              inc = inc (statementUni s),
              forBody = append (pop (forBody (statementUni s))) (addToLastExpression(last (forBody (statementUni s)), t))
            },
          expression = expression s
        }
      -- IFSTA EXP 
      | statementType s == IFSTA && closedCon (statementUni s) && closedAlt (statementUni s)= Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s, 
          statementUni = IfStatement{
              closedCon = True,
              closedAlt = True, 
              alt = [],
              con = []
            },
          expression = addXToExp(expression s, t) 
        }
      -- IFSTA CON  
      | statementType s == IFSTA && closedCon (statementUni s) == False = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s, 
          statementUni = IfStatement{
              closedCon = False,
              closedAlt = True, 
              alt = [],
              con = append (pop (con (statementUni s))) (addToLastExpression(last (con (statementUni s)), t))
            },
          expression = expression s
        }
      -- IFSTA ALT 
      | statementType s == IFSTA = Statement{
          closedSta = False, 
          staLine = staLine s,
          statementType = statementType s, 
          statementUni = IfStatement{
              closedCon = True,
              con = con (statementUni s), 
              closedAlt = False, 
              alt = append (pop (alt (statementUni s))) (addToLastExpression(last (alt (statementUni s)),t))
            },
          expression = expression s
        }
      | statementType s == FUNCSTA && closedExp (expression s) == False = Statement{ 
          closedSta = False, 
          staLine = staLine s, 
          statementType = statementType s, 
          statementUni = FuncStatement{
              closedParams = False,
              params = [], 
              body = [] 
            },
          expression = addXToExp(expression s, t)
        }
      | statementType s == FUNCSTA && closedParams (statementUni s) == False = Statement{ 
          closedSta = False, 
          staLine = staLine s, 
          statementType = statementType s, 
          statementUni = FuncStatement{
              closedParams = False,
              params = append (pop (params (statementUni s))) (addXToExp(last (params (statementUni s)), t)), 
              body = [] 
            },
          expression = expression s
        }
      | statementType s == FUNCSTA = Statement{ 
          closedSta = False, 
          staLine = staLine s, 
          statementType = statementType s, 
          statementUni = FuncStatement{
              closedParams = True, 
              params = params (statementUni s), 
              body = append (pop (body (statementUni s))) (addToLastExpression(last (body (statementUni s)), t)) 
            },
          expression = expression s
        }
      | otherwise = error ("addToLastExpression" ++ show(statementType s))

addXToExp :: (Expression, Token) -> Expression 
addXToExp (e,t) = 
  case typ t of 
    INT ->  addIntToLastExp(t, e)
    MINUS -> addMinusToLastExp(e)
    BANG -> addBangToLastExp(e)
    TRUE ->  addTFToLastExp(t, e)
    FALSE -> addTFToLastExp(t, e)
    LPAREN -> addGroupToLastExp(e)
    PLUS -> addOperatorToLastExp(t, e)
    SLASH -> addOperatorToLastExp(t, e)
    ASTERISK -> addOperatorToLastExp(t, e)
    LESS_T -> addBoolToLastExp(t,e)
    GREATER_T -> addBoolToLastExp(t,e)
    EQUALS -> addBoolToLastExp(t,e)
    NOT_EQUALS -> addBoolToLastExp(t,e)
    IDENT -> addIdentifierToLastExp(t, e)
    ASSIGN -> addAssignToLastExp(e)
    STRING -> addStringToLastExp(t,e)
    LBRACKET -> addArrayToLastExp(e)
    COLON -> addMapValToLastExp(e)
    _ -> error ("addXToExp: " ++ show (typ t))


-- Add EmptyExp val to latest map
addMapValToLastExp :: Expression -> Expression 
addMapValToLastExp e = exp  
  where 
    exp 
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addMapValToLastExp( assignExpression e)}
      | expressionType e == ARRAYEXP =  ArrayExpression {closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addMapValToLastExp(last (array e)))}
      | expressionType e == MAPEXP && (null (snd (mapMap e))|| checkNestedListMap e == False)= MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), snd(mapMap e) ++ [Expression{expressionType=EMPTYEXP, closedExp= False, expLine = -1}])}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd (mapMap e))) (addMapValToLastExp(last (snd (mapMap e)))))}
      | otherwise = error ("addMapValToLastExp " ++ expressionToString e)
-- TODO FIX PRECEDENCE
addOperatorToLastExp :: (Token, Expression) -> Expression 
addOperatorToLastExp (t,e) = exp 
  where 
    exp 
      | expressionType e == IDENTEXP || expressionType e == INTEXP || expressionType e == STRINGEXP = OperatorExpression{expressionType = OPERATOREXP, closedExp = False, expLine = line t, leftOperator = e, operator = t, rightOperator = Expression{closedExp = False, expLine = -1, expressionType = EMPTYEXP}} 
      | expressionType e == BOOLEXP = BoolExpression {closedExp = False, expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addOperatorToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP && closedExp e == False= GroupedExpression {closedExp = False, expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = addOperatorToLastExp(t, groupedExpression e)}
      | expressionType e == GROUPEDEXP = OperatorExpression{closedExp = False, expLine = expLine e, expressionType = OPERATOREXP, leftOperator = e, operator = t, rightOperator = Expression{expLine = -1, closedExp = False, expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP && checkPrecedence(t, e) && expressionType (leftOperator e) /= GROUPEDEXP  = OperatorExpression{expressionType = OPERATOREXP, expLine = expLine e, closedExp = False, leftOperator = leftOperator e, operator = operator e, rightOperator = OperatorExpression{expressionType = OPERATOREXP, expLine = expLine e, closedExp = False, leftOperator = rightOperator e, operator = t, rightOperator = Expression{closedExp = False, expLine = -1, expressionType = EMPTYEXP}}}
      | expressionType e == OPERATOREXP = OperatorExpression{expressionType = OPERATOREXP, expLine = expLine e, closedExp = False, leftOperator = e, operator = t, rightOperator = Expression{closedExp = False, expLine = -1, expressionType = EMPTYEXP}}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addOperatorToLastExp(t, assignExpression e)}
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedExp = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (append (pop (fst(mapMap e))) (addOperatorToLastExp(t, last (fst(mapMap e)))), snd(mapMap e))}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd(mapMap e))) (addOperatorToLastExp(t, last (snd(mapMap e)))))}
      | expressionType e == CALLEXP = CallExpression {closedExp = False, expLine = expLine e,expressionType = CALLEXP,  callIdent = callIdent e, callParams = append (pop (callParams e)) (addOperatorToLastExp(t, last(callParams e)))}
      | expressionType e == INDEXEXP = IndexExpression {closedExp = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = append (pop (arrayIndex e)) (addOperatorToLastExp(t, last (arrayIndex e)))}
      | expressionType e == ARRAYEXP = ArrayExpression{expressionType = ARRAYEXP, closedExp = False, expLine = expLine e, array = append (pop (array e)) (addOperatorToLastExp(t, last (array e)))}
      | otherwise = error ("addOperatorToLastExp " ++ expressionToString e)

addAssignToLastExp :: Expression -> Expression
addAssignToLastExp e = exp  
  where 
    exp 
      | expressionType e == IDENTEXP || expressionType e == INDEXEXP = AssignExpression {closedExp = False, expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = e, assignExpression = Expression {closedExp = False, expLine = expLine e,expressionType = EMPTYEXP}} 
      | otherwise = error ("addAssignToLastExp " ++ expressionToString e)
  

addStringToLastExp :: (Token, Expression) -> Expression
addStringToLastExp (t, e) = exp 
  where 
    exp 
      | expressionType e == EMPTYEXP = StringExpression{expLine = line t, closedExp = True, expressionType = STRINGEXP, stringLiteral = t}
      | expressionType e == OPERATOREXP = OperatorExpression{expressionType = OPERATOREXP, expLine = expLine e, closedExp = False, leftOperator = leftOperator e, operator = operator e, rightOperator = addStringToLastExp(t, rightOperator e)} 
      | expressionType e == ARRAYEXP && null (array e) =ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = [addStringToLastExp(t, Expression{expLine = -1, expressionType = EMPTYEXP, closedExp = False})]} 
      | expressionType e == ARRAYEXP =ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addStringToLastExp(t, last (array e)))} 
      | expressionType e == MAPEXP && nextItem e == KEY && null (fst (mapMap e))= MapExpression{closedExp = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = ([addStringToLastExp(t, Expression{expLine = -1, expressionType = EMPTYEXP, closedExp = False})], [])}
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedExp = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (append (pop (fst(mapMap e))) (addStringToLastExp(t, last (fst(mapMap e)))), snd(mapMap e))}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd(mapMap e))) (addStringToLastExp(t, last (snd(mapMap e)))))}
      | expressionType e == BOOLEXP = BoolExpression {closedExp = False, expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addStringToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {closedExp = False, expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = addStringToLastExp(t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addStringToLastExp(t, assignExpression e)}
      | expressionType e == CALLEXP && null (callParams e)= CallExpression {closedExp = False, expLine = expLine e,expressionType = CALLEXP, callIdent = callIdent e, callParams = [addStringToLastExp(t, Expression{closedExp = False, expressionType = EMPTYEXP, expLine = expLine e})]}
      | expressionType e == CALLEXP = CallExpression {closedExp = False, expLine = expLine e,expressionType = CALLEXP,  callIdent = callIdent e, callParams = append (pop (callParams e)) (addStringToLastExp(t, last(callParams e)))}
      | expressionType e == INDEXEXP = IndexExpression {closedExp = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = append (pop (arrayIndex e)) (addStringToLastExp(t, last (arrayIndex e)))}
      | otherwise = error ("addStringToLastExp " ++ expressionToString e)
       
-- TODO FIX INDEX
addArrayToLastExp :: Expression -> Expression 
addArrayToLastExp e = exp 
  where 
    exp 
      | expressionType e == EMPTYEXP = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = []}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addArrayToLastExp(assignExpression e)}
      | expressionType e == ARRAYEXP = ArrayExpression {closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addArrayToLastExp(last (array e)))}
      | expressionType e == MAPEXP= MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd(mapMap e))) (addArrayToLastExp(last (snd(mapMap e)))))}
      | otherwise = error ("addArrayToLastExp " ++ expressionToString e)

addIdentifier :: (Token, [Statement]) -> [Statement]
addIdentifier (t, s) = sta 
  where 
    sta 
      | null s || closedSta (last s) = append s Statement{
          closedSta = False,
          staLine = line t, 
          statementType = ASSIGNSTA, 
          statementUni = AssignStatement{},
          expression = IdentExpression{closedExp = False,expLine = (line t), expressionType = IDENTEXP, ident = t} 
        } 
      | statementType (last s) == ASSIGNSTA || statementType (last s) == RETSTA || statementType (last s) == LETSTA = append (pop s) Statement{
          closedSta = False, 
          staLine =staLine (last s),
          statementType = statementType (last s),
          statementUni = statementUni (last s),
          expression = addIdentifierToLastExp(t, expression (last s))
        }
      | statementType (last s) == FUNCSTA && closedExp (expression (last s)) == False = append (pop s) Statement{
          closedSta = False, 
          staLine =staLine (last s),
          statementType = statementType (last s),
          statementUni = statementUni (last s),
          expression = closeExpression(addIdentifierToLastExp(t, expression (last s)))
        }
      | statementType (last s) == FUNCSTA && closedParams (statementUni (last s)) == False && null (params (statementUni (last s))) = append (pop s) Statement{
          closedSta = False, 
          staLine =staLine (last s),
          statementType = statementType (last s),
          statementUni = FuncStatement{
              closedParams = False, 
              params =[addXToExp(Expression{expLine = -1, expressionType = EMPTYEXP, closedExp = False}, t)], 
              body = []
            },
          expression = expression (last s) 
        }
      | statementType (last s) == FUNCSTA && closedParams (statementUni (last s)) == False = append (pop s) Statement{
          closedSta = False, 
          staLine =staLine (last s),
          statementType = statementType (last s),
          statementUni = FuncStatement{
              closedParams = False, 
              params = append (pop (params (statementUni (last s)))) (addXToExp(last (params (statementUni (last s))), t)), 
              body = []
            },
          expression = expression (last s) 
        }
      | otherwise = error ("addIdentifier: " ++ statementsToString s) 
closeExpression :: Expression -> Expression
closeExpression e = ex 
  where 
    ex 
      | expressionType e == IDENTEXP = IdentExpression{closedExp = True,expLine = expLine e, expressionType = IDENTEXP, ident = ident e} 
      | otherwise = error ("closeExpression " ++ expressionToString e)

addIdentifierToLastExp :: (Token, Expression) -> Expression 
addIdentifierToLastExp (t, e) = exp 
  where 
    exp 
      | expressionType e == EMPTYEXP = IdentExpression{closedExp = False ,expLine = (line t), expressionType = IDENTEXP, ident = t}
      | expressionType e == ASSIGNEXP = AssignExpression{closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIdentifierToLastExp(t, assignExpression e)}
      | expressionType e == PREFIXEXP = PrefixExpression{closedExp = False,expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = addIdentifierToLastExp(t, prefixExpression e)}
      | otherwise = error ("addIdentifierToLastExp: " ++ expressionToString e) 


addBoolToLastExp :: (Token, Expression) -> Expression
addBoolToLastExp (t,e) = exp 
  where 
    exp 
      | expressionType e == IDENTEXP || expressionType e == INTEXP || expressionType e == STRINGEXP = BoolExpression{expressionType = BOOLEXP, closedExp = False, expLine = line t, leftBool = e, boolOperator= t, rightBool = Expression{closedExp = False, expLine = -1, expressionType = EMPTYEXP}} 
      | expressionType e == GROUPEDEXP = GroupedExpression {closedExp = False, expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = addBoolToLastExp(t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addBoolToLastExp(t, assignExpression e)}
      | otherwise = error ("addBoolToLastExp: " ++ expressionToString e) 

addIntToLastExp :: (Token, Expression) -> Expression 
addIntToLastExp (t, e) = exp 
  where 
    exp 
      | expressionType e == EMPTYEXP = IntegerLiteralExpression{expLine = line t, closedExp = True, expressionType = INTEXP, integerLiteral = t}
      | expressionType e == OPERATOREXP = OperatorExpression{expressionType = OPERATOREXP, expLine = expLine e, closedExp = False, leftOperator = leftOperator e, operator = operator e, rightOperator = addIntToLastExp(t, rightOperator e)} 
      | expressionType e == ARRAYEXP && null (array e) = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = [addIntToLastExp(t, Expression{expLine = -1, expressionType = EMPTYEXP, closedExp =False})]} 
      | expressionType e == ARRAYEXP =ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addIntToLastExp(t, last (array e)))} 
      --TODO error handle below
      | expressionType e == MAPEXP && null (fst(mapMap e)) = MapExpression{closedExp = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = ([addIntToLastExp(t, Expression{expressionType = EMPTYEXP, expLine = -1, closedExp = False })], [])} 
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedExp = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (append (pop (fst(mapMap e))) (addIntToLastExp(t, last (fst(mapMap e)))), snd(mapMap e))}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd(mapMap e))) (addIntToLastExp(t, last (snd(mapMap e)))))}
      | expressionType e == BOOLEXP = BoolExpression {closedExp = False, expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addIntToLastExp(t, rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {closedExp = False, expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = addIntToLastExp (t, groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addIntToLastExp(t, assignExpression e)}
      | expressionType e == CALLEXP && null (callParams e)= CallExpression {closedExp = False, expLine = expLine e,expressionType = CALLEXP, callIdent = callIdent e, callParams = [addIntToLastExp(t, Expression{closedExp = False, expressionType = EMPTYEXP, expLine = expLine e})]}
      | expressionType e == CALLEXP = CallExpression {closedExp = False, expLine = expLine e,expressionType = CALLEXP,  callIdent = callIdent e, callParams = append (pop (callParams e)) (addIntToLastExp(t, last(callParams e)))}
      | expressionType e == PREFIXEXP = PrefixExpression{closedExp = False,expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = addIntToLastExp(t, prefixExpression e)}
      | expressionType e == INDEXEXP = IndexExpression {closedExp = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = append (pop (arrayIndex e)) (addIntToLastExp(t, last (arrayIndex e)))}
      | otherwise = error ("addIntToLastExp " ++ expressionToString e ++ " " ++ literal t)

addMinusToLastExp :: Expression -> Expression 
addMinusToLastExp e = exp 
  where 
    exp 
      | expressionType e == EMPTYEXP = PrefixExpression{closedExp = False, expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = Token{line = -1, typ = MINUS, literal = "-"}, prefixExpression = Expression {closedExp = False, expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == IDENTEXP || expressionType e == INTEXP || expressionType e == STRINGEXP = OperatorExpression{expressionType = OPERATOREXP, closedExp = False, expLine = expLine e, leftOperator = e, operator =  Token{line = -1, typ = MINUS, literal = "-"}, rightOperator = Expression{closedExp = False, expLine = -1, expressionType = EMPTYEXP}} 
      | expressionType e == OPERATOREXP = OperatorExpression{expressionType = OPERATOREXP, expLine = expLine e, closedExp = False, leftOperator = leftOperator e, operator = operator e, rightOperator = addMinusToLastExp(rightOperator e)} 
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = [PrefixExpression{closedExp = False, expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = Token{line = -1, typ = MINUS, literal = "-"}, prefixExpression = Expression {closedExp = False, expLine = expLine e, expressionType = EMPTYEXP}}]} 
      | expressionType e == ARRAYEXP =ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addMinusToLastExp(last (array e)))} 
      | expressionType e == MAPEXP && nextItem e == KEY = MapExpression{closedExp = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (append (pop (fst( mapMap e))) (addMinusToLastExp(last (fst(mapMap e)))), snd(mapMap e))}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd(mapMap e))) (addMinusToLastExp(last (snd(mapMap e)))))}
      | expressionType e == BOOLEXP = BoolExpression {closedExp = False, expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addMinusToLastExp(rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression {closedExp = False, expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = addMinusToLastExp(groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e,expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addMinusToLastExp(assignExpression e)}
      | expressionType e == CALLEXP && null (callParams e)= CallExpression {closedExp = False, expLine = expLine e,expressionType = CALLEXP, callIdent = callIdent e, callParams = [addMinusToLastExp(Expression{closedExp = False, expressionType = EMPTYEXP, expLine = expLine e})]}
      | expressionType e == CALLEXP = CallExpression {closedExp = False, expLine = expLine e,expressionType = CALLEXP,  callIdent = callIdent e, callParams = append (pop (callParams e)) (addMinusToLastExp(last(callParams e)))}
      | expressionType e == INDEXEXP = IndexExpression {closedExp = False, expLine = expLine e, expressionType = INDEXEXP, arrayIdent = arrayIdent e, arrayIndex = append (pop (arrayIndex e)) (addMinusToLastExp(last (arrayIndex e)))}
      | otherwise = error ("addMinusToLastExp " ++ expressionToString e)

addBangToLastExp:: Expression -> Expression
addBangToLastExp e = exp 
  where 
    exp 
      | expressionType e == EMPTYEXP = PrefixExpression {closedExp = False, expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = Token{line = expLine e, typ = BANG, literal = "!"}, prefixExpression = Expression {closedExp = False, expLine = expLine e, expressionType = EMPTYEXP}}
      | expressionType e == OPERATOREXP = OperatorExpression {closedExp = False, expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e,operator = operator e,rightOperator = addBangToLastExp(rightOperator e)}
      | expressionType e == BOOLEXP = BoolExpression {closedExp = False, expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addBangToLastExp(rightBool e)}
      | expressionType e == GROUPEDEXP = GroupedExpression{expLine = expLine e, expressionType = GROUPEDEXP, closedExp=False, groupedExpression = addBangToLastExp(groupedExpression e)}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False,expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addBangToLastExp(assignExpression e)}
      | expressionType e == ARRAYEXP && null (array e) = ArrayExpression{expLine = expLine e, expressionType = ARRAYEXP, closedExp = False,  array = [PrefixExpression{closedExp = False, expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = Token{line = expLine e, typ = BANG, literal = "!"}, prefixExpression = Expression {closedExp = False, expLine = expLine e, expressionType = EMPTYEXP}}]}
      | expressionType e == ARRAYEXP = ArrayExpression{expLine = expLine e, expressionType = ARRAYEXP, closedExp = False,  array = pop (array e) ++ [addBangToLastExp(last (array e))]}
      | expressionType e == CALLEXP && null (callParams e)= CallExpression{closedExp = False, expLine = expLine e, expressionType = CALLEXP, callIdent = callIdent e, callParams = [addBangToLastExp(Expression{closedExp = False, expLine = -1, expressionType = EMPTYEXP})]}
      | expressionType e == CALLEXP = CallExpression{expLine = expLine e, expressionType = CALLEXP, closedExp = False, callIdent = callIdent e, callParams = pop (callParams e ) ++ [addBangToLastExp(last (callParams e))]}
      | expressionType e == MAPEXP && nextItem e == VAL = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd (mapMap e))) (addBangToLastExp(last (snd (mapMap e)))))}
      | otherwise = error ("addBangToLastExp on line: " ++ (show (expLine e)) ++ " " ++ expressionToString(e)) 

addTFToLastExp :: (Token, Expression) -> Expression 
addTFToLastExp (t,e) = exp 
  where 
    exp 
      | expressionType e == BOOLEXP && expressionType (rightBool e) == EMPTYEXP = BoolExpression {closedExp = False, expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = TFExpression {closedExp =False, expLine = expLine e, expressionType = TFEXP, bool = typ t}} 
      | expressionType e == BOOLEXP = BoolExpression {closedExp = False, expLine = expLine e, expressionType = BOOLEXP, leftBool = leftBool e, boolOperator = boolOperator e, rightBool = addTFToLastExp(t, rightBool e)}
      | expressionType e == ASSIGNEXP = AssignExpression {closedExp = False, expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addTFToLastExp(t, assignExpression e)}
      | expressionType e == EMPTYEXP = TFExpression{closedExp = False, expLine = expLine e, expressionType = TFEXP, bool = typ t} 
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = [TFExpression{closedExp = False, expLine = expLine e, expressionType = TFEXP, bool = typ t} ]} 
      | expressionType e == ARRAYEXP && expressionType (last (array e)) == ARRAYEXP = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addTFToLastExp(t,last (array e))]}
      | expressionType e == MAPEXP && nextItem e == KEY = error "can't add True/False as key"
      | expressionType e == MAPEXP && null (snd (mapMap e))= MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), [addTFToLastExp(t, Expression{expLine = -1, expressionType = EMPTYEXP, closedExp = False})])}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd (mapMap e))) (addTFToLastExp(t, last(snd (mapMap e)))))}
      | expressionType e == ARRAYEXP = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = pop (array e) ++ [addTFToLastExp(t, last (array e))]} 
      | expressionType e == GROUPEDEXP = GroupedExpression{closedExp = False, expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = addTFToLastExp(t, groupedExpression e)}
      | expressionType e == PREFIXEXP = PrefixExpression{closedExp = False, expLine = expLine e, expressionType = PREFIXEXP, prefixOperator = prefixOperator e, prefixExpression = TFExpression{closedExp = False, expLine = expLine e, expressionType = TFEXP, bool = typ t}}
      | otherwise = error ("addTFToLastExp " ++ expressionToString e)

addGroupToLastExp :: Expression -> Expression
addGroupToLastExp e = exp 
  where 
    exp 
      | expressionType e == EMPTYEXP = GroupedExpression{closedExp = False, expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = Expression{closedExp = False, expLine = -1, expressionType =EMPTYEXP}}
      | expressionType e == ASSIGNEXP = AssignExpression{closedExp = False, expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = addGroupToLastExp(assignExpression e)}
      | expressionType e == MAPEXP && nextItem e == VAL = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap =(fst(mapMap e), append (pop (snd (mapMap e))) (addGroupToLastExp(last(snd (mapMap e)))))}
      | expressionType e == ARRAYEXP = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addGroupToLastExp(last (array e)))}
      | otherwise = error ("addGroupToLastExp " ++ expressionToString e) 

addLParen :: Statement -> Statement 
addLParen s = sta 
  where 
    sta 
      | statementType s == RETSTA || statementType s == LETSTA || statementType s == ASSIGNSTA = Statement{
        closedSta = False, 
        staLine =staLine s, 
        statementUni = statementUni s,
        statementType = statementType s,
        expression = addGroupToLastExp(expression s)
      }
      | statementType s == FUNCSTA && closedParams (statementUni s) == False = Statement{
        closedSta = False, 
        staLine =staLine s, 
        statementUni = FuncStatement{
            closedParams = False, 
            params = [],
            body = []
          },
        statementType = statementType s,
        expression = expression s 
        }
      | statementType s == FUNCSTA = Statement{
        closedSta = False, 
        staLine =staLine s, 
        statementUni = FuncStatement{
            closedParams = True, 
            params = params (statementUni s),
            body = append (pop (body (statementUni s))) (addLParen (last (body (statementUni s)))) 
          },
        statementType = statementType s,
        expression = expression s 
        }
      | otherwise = error ("addLParen " ++ statementToString s) 

closeLast :: Statement -> Statement 
closeLast s = sta 
  where 
    sta 
      | statementType s == RETSTA || statementType s == ASSIGNSTA || statementType s == LETSTA = Statement{
          statementType = statementType s,
          statementUni = statementUni s,
          staLine =staLine s, 
          closedSta = True, 
          expression = expression s
        }
      | statementType s == FUNCSTA && (null (body (statementUni s)) || closedSta (last (body (statementUni s)))) = Statement{
        closedSta = True, 
        staLine =staLine s, 
        statementUni = FuncStatement{
            closedParams = True, 
            params = params (statementUni s),
            body = body (statementUni s) 
          },
        statementType = statementType s,
        expression = expression s 
        }
      | statementType s == FUNCSTA = Statement{
        closedSta = False, 
        staLine =staLine s, 
        statementUni = FuncStatement{
            closedParams = True, 
            params = params (statementUni s),
            body = append (pop (body (statementUni s))) (closeLast (last (body (statementUni s))))
          },
        statementType = statementType s,
        expression = expression s 
        } 
      | otherwise = error ("closeLast " ++ statementToString s) 


findGrouped :: Expression -> Bool 
findGrouped e = b 
  where 
    b 
      | otherwise = False 

closeLastParenExp :: Expression -> Expression 
closeLastParenExp e = ex 
  where 
    ex 
      | expressionType e == GROUPEDEXP && findGrouped(groupedExpression e) == False = GroupedExpression{closedExp = True,expLine = expLine e, expressionType = GROUPEDEXP, groupedExpression = groupedExpression e} 
      | expressionType e == OPERATOREXP = OperatorExpression{closedExp = False, expLine = expLine e, expressionType = OPERATOREXP, leftOperator = leftOperator e, operator = operator e, rightOperator = closeLastParenExp(rightOperator e)}
      | expressionType e == ASSIGNEXP = AssignExpression{closedExp = False,expLine = expLine e, expressionType = ASSIGNEXP, assignIdent = assignIdent e, assignExpression = closeLastParenExp(assignExpression e)}
      | expressionType e == ARRAYEXP = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (closeLastParenExp(last (array e)))}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst(mapMap e), append (pop (snd (mapMap e))) (closeLastParenExp(last (snd(mapMap e)))))}
      | otherwise = error ("closeLastParenExp " ++ expressionToString e)

isGroupedExpression :: Expression -> Bool
isGroupedExpression e = False

closeLastParen :: Statement -> Statement
closeLastParen s = sta 
  where 
    sta 
      | statementType s == RETSTA || statementType s == ASSIGNSTA || statementType s == LETSTA = Statement{
          staLine = staLine s, 
          statementType = statementType s, 
          statementUni = statementUni s, 
          closedSta = False, 
          expression = closeLastParenExp (expression s)
        }
      | statementType s == FUNCSTA && closedParams (statementUni s) == False && isGroupedExpression (last (params (statementUni s))) == False = Statement{
        closedSta = False, 
        staLine =staLine s, 
        statementUni = FuncStatement{
            closedParams = True, 
            params = params (statementUni s),
            body = [] 
          },
        statementType = statementType s,
        expression = expression s 
        }
      | otherwise = error ("closeLastParen " ++ statementToString s) 

closeArray :: Statement -> Statement 
closeArray s = sta 
  where 
    sta 
      | statementType s == RETSTA || statementType s == LETSTA || statementType s == ASSIGNSTA = Statement{
          staLine =staLine s,
          statementUni =statementUni s,
          closedSta = False, 
          statementType = statementType s,
          expression = closeArrayExp(expression s)
        } 
      | otherwise = error ("closeArray " ++ statementToString s) 


closeArrayExp :: Expression -> Expression 
closeArrayExp e = ex 
  where 
    ex 
      | expressionType e == ARRAYEXP && null (array e) == False && ((expressionType (last (array e)) == ARRAYEXP || expressionType (last (array e)) == MAPEXP) && closedExp (last (array e)) == False) = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (closeArrayExp(last (array e)))}
      | expressionType e == ARRAYEXP = ArrayExpression{closedExp = True, expLine = expLine e, expressionType = ARRAYEXP, array = array e}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst(mapMap e), append (pop (snd (mapMap e))) (closeArrayExp (last (snd(mapMap e)))))}
      | expressionType e == ASSIGNEXP = AssignExpression{closedExp = False,expLine =expLine e, expressionType = ASSIGNEXP, assignIdent =assignIdent e, assignExpression = closeArrayExp(assignExpression e)}
      | otherwise = error ("closeArrayExp " ++ expressionToString e)

addMapToExp :: Expression -> Expression 
addMapToExp e = ex 
  where 
    ex  
      | expressionType e == EMPTYEXP = MapExpression{closedExp = False, nextItem = KEY , expLine = -1, expressionType = MAPEXP, mapMap = ([], [])} 
      | expressionType e == ASSIGNEXP = AssignExpression{closedExp = False,expLine =expLine e, expressionType = ASSIGNEXP, assignIdent =assignIdent e, assignExpression = addMapToExp(assignExpression e)}
      | expressionType e == ARRAYEXP && null (array e)= ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = [MapExpression{closedExp = False, nextItem = KEY , expLine = -1, expressionType = MAPEXP, mapMap = ([], [])}]} 
      | expressionType e == ARRAYEXP = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addMapToExp (last (array e)))}
      | expressionType e == MAPEXP = MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst(mapMap e), append (pop (snd(mapMap e))) (addMapToExp(last(snd(mapMap e)))))}
      | otherwise = error ("addMapToExp " ++ expressionToString e)


parseLBrace :: Statement -> Statement 
parseLBrace s = sta 
  where 
    sta 
      | statementType s == RETSTA || statementType s == LETSTA || statementType s == ASSIGNSTA = Statement{
          staLine =staLine s,
          statementUni =statementUni s,
          closedSta = False, 
          statementType = statementType s,
          expression = addMapToExp(expression s)
        } 
      | statementType s == FUNCSTA && closedParams (statementUni s) && null (body (statementUni s)) = Statement{
        closedSta = False, 
        staLine =staLine s, 
        statementUni = FuncStatement{
            closedParams = True, 
            params = params (statementUni s),
            body = [] 
          },
        statementType = statementType s,
        expression = expression s 
        }
      | otherwise = error ("parseLBrace " ++ statementToString s) 

checkNestedListArr :: Expression -> Bool
checkNestedListArr e = (expressionType e == ARRAYEXP && null (array e) == False && expressionType (last (array e)) == ARRAYEXP) || (expressionType (last (array e)) == MAPEXP && closedExp (last (array e)) == False)

checkNestedListMap :: Expression -> Bool 
checkNestedListMap e = null (snd (mapMap e)) || (closedExp (last (snd (mapMap e))) == False && (expressionType (last (snd(mapMap e))) == MAPEXP || expressionType (last (snd (mapMap e))) == ARRAYEXP)) 


addEleToArray :: Expression -> Expression 
addEleToArray e = ex 
  where 
    ex 
      | expressionType e == ARRAYEXP && checkNestedListArr e == True = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (addEleToArray(last (array e)))}
      | expressionType e == ARRAYEXP =  ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (array e) Expression{expLine = -1, closedExp = False, expressionType = EMPTYEXP}}
      | expressionType e == ASSIGNEXP = AssignExpression{closedExp = False,expLine =expLine e, expressionType = ASSIGNEXP, assignIdent =assignIdent e, assignExpression = addEleToArray (assignExpression e)}
      | expressionType e == MAPEXP && checkNestedListMap e == False = MapExpression{closedExp = False, nextItem = KEY, expLine = expLine e, expressionType = MAPEXP, mapMap = (append (fst(mapMap e)) Expression{expLine = -1, closedExp = False, expressionType = EMPTYEXP}, snd(mapMap e))}
      | expressionType e == MAPEXP =MapExpression{closedExp = False, nextItem = VAL, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst(mapMap e), append (pop (snd(mapMap e))) (addEleToArray(last (snd(mapMap e)))))}
      | otherwise = error ("addEleToArray " ++ expressionToString e)


addComma :: Statement -> Statement 
addComma s = sta 
  where 
    sta 
      | statementType s == RETSTA || statementType s == LETSTA || statementType s == ASSIGNSTA = Statement{
          staLine =staLine s,
          statementUni =statementUni s,
          closedSta = False, 
          statementType = statementType s,
          expression = addEleToArray(expression s)
        } 
      | otherwise = error ("addComma " ++ statementToString s)

closeLastMapExp :: Expression -> Expression
closeLastMapExp e = ex 
  where 
    ex 
      | expressionType e == MAPEXP && length(fst(mapMap e)) /= length(snd(mapMap e)) = error ("invalid map diff length of keys and vals: " ++ expressionToString e)
      | expressionType e == MAPEXP && checkNestedListMap e== False = MapExpression{closedExp = True, nextItem = nextItem e, expLine = expLine e, expressionType = MAPEXP, mapMap = mapMap e}
      | expressionType e == MAPEXP = MapExpression{closedExp = False , nextItem = nextItem e, expLine = expLine e, expressionType = MAPEXP, mapMap = (fst(mapMap e), append (pop (snd (mapMap e))) (closeLastMapExp(last (snd (mapMap e)))))}
      | expressionType e == ASSIGNEXP = AssignExpression{closedExp = False,expLine =expLine e, expressionType = ASSIGNEXP, assignIdent =assignIdent e, assignExpression = closeLastMapExp(assignExpression e)}
      | expressionType e == ARRAYEXP = ArrayExpression{closedExp = False, expLine = expLine e, expressionType = ARRAYEXP, array = append (pop (array e)) (closeLastMapExp(last (array e))) }

parseRBrace :: Statement -> Statement 
parseRBrace s = sta 
  where 
    sta 
      | statementType s == RETSTA || statementType s == LETSTA || statementType s == ASSIGNSTA = Statement{
          staLine =staLine s,
          statementUni =statementUni s,
          closedSta = False, 
          statementType = statementType s,
          expression = closeLastMapExp(expression s)
        } 
      -- CHECK IF LAST STA IS CLOSED IN BODY  
      | statementType s == FUNCSTA = Statement{
        closedSta = False, 
        staLine =staLine s, 
        statementUni = FuncStatement{
            closedParams = True, 
            params = params (statementUni s),
            body = body (statementUni s) 
          },
        statementType = statementType s,
        expression = expression s 
        }
      | otherwise = error ("parseRBrace " ++ statementToString s) 

addElse :: ([Token], [Statement]) -> ([Token], [Statement])
addElse (t,s) = sta 
  where 
    sta 
      | typ (head t) /= LBRACE = error "no LBRACE in addElse"
      | statementType (last s) == IFSTA && closedCon (statementUni (last s)) == True && closedAlt(statementUni (last s)) == True = (removeFirst t, append (pop s) Statement{
          closedSta = False, 
          staLine = staLine (last s), 
          statementType = statementType (last s), 
          statementUni = IfStatement{
              closedCon = True, 
              con = con (statementUni (last s)),
              closedAlt = False,
              alt = []
            },
          expression = expression (last s)
        } 
        )
      | otherwise = error ("addElse " ++ statementToString (last s))

checkPrecedence :: (Token, Expression) -> Bool
checkPrecedence (t, e)= getPrecedence (typ t) > getPrecedence (typ (operator e)) 
