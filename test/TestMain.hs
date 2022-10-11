module Main where

import TestLexer
import TestParser
import TestPrecedence

main = do
  runLexerTests
  runPrecedenceTests
  runParserTests
