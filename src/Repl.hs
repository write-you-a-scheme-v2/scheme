{-# LANGUAGE OverloadedStrings #-}

module Repl (
repl
) where

import Eval
import System.Console.Haskeline
import Data.Text as T 

{-
replInner :: IO ()
replInner = do 
  line <- getInputLine "> "
  evalTextExpr line
  replInner


repl :: IO ()
repl = do
  print "Hey, I just met you, and this is crazy, but lets use monads, we'll start with Maybe"
  replInner
-}
 
repl :: IO ()
repl = runInputT defaultSettings loop
  where 
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do 
                        outputStrLn "test"
        -- $ T.unpack $  evalTextExpr $#-} 
                        loop
        --Just input -> (x <- return $ evalTextExpr $ T.pack input) >> return loop
