{-# LANGUAGE OverloadedStrings #-}
module Cli (
  cliIface,
) where

import System.IO
import System.Environment --getArgs
import Eval
import Data.Text as T
import Control.Monad.Trans
import Options.Applicative


--http://book.realworldhaskell.org/read/io.html

process :: String -> IO ()
process = evalText $ T.pack str

cliIface :: IO ()
cliIface = do 
  args <- getArgs 
  let arg = args !! 0 -- make this more robust
  inh <- openFile arg ReadMode
  mainloop inh
  hClose inh

mainloop ::  Handle -> IO ()
mainloop inh  =  do 
  ineof <- hIsEOF inh
  if ineof
    then  putStr "empty file\n" >> return ()
      else do fileText <- T.pack $ readFile $ inh 
              process fileText

-- https://github.com/pcapriotti/optparse-applicative
