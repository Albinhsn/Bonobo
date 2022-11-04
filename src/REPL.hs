module REPL where

main = do
  line <- getLine 
  if null line 
    then return ()
    else do
      putStrLn "got" 
      main 
