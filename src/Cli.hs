{-# LANGUAGE OverloadedStrings #-}
module Cli (
  cliIfaceSimple,
  cliIface
) where

import Eval
import System.IO
import System.Environment --getArgs
import Data.Text as T
import Control.Monad.Trans
import Options.Applicative
import Options.Applicative
--import Data.Semigroup ((<>))


--http://book.realworldhaskell.org/read/io.html

process :: T.Text -> IO ()
process txt = evalText txt

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
      else do fileText <- hGetContents $ inh 
              process $ T.pack fileText

-- https://github.com/pcapriotti/optparse-applicative
-- https://hackage.haskell.org/package/optparse-applicative

data LineOpts = LineOpts
  { script :: String
  , useRepl :: Bool 
  }

parseLineOpts :: Parser LineOpts
parseLineOpts = LineOpts
    <$> strOption
        ( long "script"
       <> short 's'
       <> metavar "SCRIPT"
       <> value "" --default to empty string so we don't need to specify in the case of using the REPL
       <> help "File containing the script you want to run")
    <*> switch
        ( long "repl"
       <> short 'r'
       <> help "Run as interavtive read/evaluate/print/loop")

schemeEntryPoint :: LineOpts -> IO ()
schemeEntryPoint (LineOpts script False) = putStrLn $ "Run script: " ++ script -- script
schemeEntryPoint (LineOpts "" True) = putStrLn $ "Run repl " --repl
schemeEntryPoint (LineOpts script True) = putStrLn $ "Run script " ++  script ++ " then go into repl " --script, then repl

cliIface :: IO ()
cliIface = execParser opts >>= schemeEntryPoint
  where
    opts = info (helper <*> parseLineOpts)
      ( fullDesc
     <> header "Executable binary for Write You A Scheme v2.0"
     <> progDesc "contains an entry point for both running scripts and repl" )



