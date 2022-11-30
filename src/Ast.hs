module Ast where

import Token

data PrecedenceType = EQUAL | LESSGREATER | SUM | PRODUCT | PREFIX | CALL | LOWEST deriving (Eq, Show)

precedenceMap :: [(PrecedenceType, Int)]
precedenceMap =
  [ (LOWEST, 0),
    (EQUAL, 2),
    (LESSGREATER, 1),
    (SUM, 3),
    (PRODUCT, 4),
    (PREFIX, 5),
    (CALL, 6)
  ]

getPrecedenceTypeFromTokenType :: TokenType -> PrecedenceType
getPrecedenceTypeFromTokenType t = p
  where
    p
      | t == PLUS || t == MINUS = SUM
      | t == BANG = PREFIX
      | t == EQUALS = EQUAL
      | t == ASTERISK || t == SLASH = PRODUCT
      | t == GREATER_T || t == LESS_T = LESSGREATER
      | t == FUNCTION = CALL
      | otherwise = error "can't map tokentype to precedence"

getPrecedence :: TokenType -> Int
getPrecedence t = head [snd n | n <- precedenceMap, fst n == getPrecedenceTypeFromTokenType t]

hasPrecedence :: (TokenType, TokenType) -> Bool
hasPrecedence (t1, t2) = getPrecedence t1 > getPrecedence t2



data ExpressionType
  = OPERATOREXP
  | BOOLEXP
  | INTEXP
  | GROUPEDEXP
  | PREFIXEXP
  | EMPTYEXP
  | TFEXP
  | IDENTEXP
  | FUNCEXP 
  | ASSIGNEXP
  | CALLEXP
  | STRINGEXP
  | ARRAYEXP 
  | INDEXEXP
  | MAPEXP
  deriving (Eq, Show)

data Expression
  = OperatorExpression {closedExp :: !Bool, expLine :: !Int, expressionType :: !ExpressionType, leftOperator :: !Expression, operator :: !Token, rightOperator :: !Expression}
  | IntegerLiteralExpression {closedExp :: !Bool, expLine :: !Int, expressionType :: !ExpressionType, integerLiteral :: !Token}
  | ArrayExpression {closedExp :: !Bool, expLine :: !Int, expressionType :: !ExpressionType, array :: ![Expression]} 
  | MapExpression {closedExp :: !Bool, nextItem :: !MapType, expLine :: !Int, expressionType :: !ExpressionType, mapMap :: ([Expression], [Expression])}
  | IndexExpression {closedExp :: !Bool, expLine :: !Int, expressionType :: !ExpressionType, arrayIdent :: !Expression, arrayIndex :: ![Expression]}
  | StringExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, stringLiteral :: !Token}
  | GroupedExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, groupedExpression :: !Expression}
  | PrefixExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, prefixOperator :: !Token, prefixExpression :: !Expression}
  | BoolExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, leftBool :: !Expression, boolOperator :: !Token, rightBool :: !Expression}
  | TFExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, bool :: !TokenType}
  | Expression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType}
  | IdentExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, ident :: !Token}
  | AssignExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, assignIdent :: !Expression, assignExpression :: !Expression}
  | CallExpression {closedExp :: !Bool,expLine :: !Int, expressionType :: !ExpressionType, callParams :: ![Expression], callIdent :: !Expression}
  deriving (Eq, Show)


data MapType = KEY | VAL deriving (Eq, Show)

data Statement = Statement
  { staLine :: !Int, 
    closedSta :: !Bool, 
    statementType :: !StatementType,
    statementUni :: !StatementUni,
    expression :: !Expression
  }
  deriving
    ( Eq,
      Show
    )
data BlockType = CON | ALT | EXP | PAR | BOD | START | STOP | INC deriving(Eq, Show) 

data StatementType = 
  NOSTA 
  | LETSTA 
  | RETSTA 
  | IFSTA 
  | FUNCSTA 
  | ASSIGNSTA 
  | CALLSTA
  | FORSTA
  deriving (Eq, Show)

data StatementUni
  = LetStatement {identifier :: !String}
  | ReturnStatement {}
  | IfStatement {closedCon :: !Bool, con :: ![Statement], alt :: ![Statement], closedAlt :: !Bool}
  | FuncStatement {closedParams :: !Bool, closedBody :: !Bool, params :: ![Expression], body :: ![Statement]} 
  | ForStatement{start :: !Expression, stop ::  !Expression, inc :: !Expression ,forBody :: ![Statement]}
  | CallStatement {} 
  | AssignStatement{}
  | NoStatement {}
  deriving
    ( Eq,
      Show
    )
