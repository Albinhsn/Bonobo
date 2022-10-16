module TestPrecedence where

import Ast
import Token

testPlusMinus :: Bool  
testPlusMinus =
  hasPrecedence (MINUS, PLUS)

testCallPlus :: Bool 
testCallPlus =
  hasPrecedence (FUNCTION, PLUS)
