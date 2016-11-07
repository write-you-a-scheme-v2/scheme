{-# LANGUAGE OverloadedStrings #-}
module Cli (
  cliIface
) where

import Eval -- evalFile :: T.Text -> IO ()
import Repl -- Repl.mainLoop :: IO ()
import System.IO
import System.Environment --getArgs
import Data.Text as T
import Control.Monad.Trans
import Options.Applicative
import Options.Applicative
--import Data.Semigroup ((<>))


--http://book.realworldhaskell.org/read/io.html


{-
cliIfaceSimple :: IO ()
cliIfaceSimple = do 
  args <- getArgs 
  let arg = args !! 0 -- make this more robust
  mainloop inh
-}
runScript ::  T.Text -> IO ()
runScript script =  do 
  inh <- openFile (T.unpack script) ReadMode
  ineof <- hIsEOF inh
  if ineof
    then  putStr "empty file\n" >> return ()
      else do fileText <- hGetContents $ inh 
              evalFile $ T.pack fileText
  hClose inh

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
       <> help "File containing the script you want to run")
    <*> switch
        ( long "repl"
       <> short 'r'
       <> help "Run as interavtive read/evaluate/print/loop")

schemeEntryPoint :: LineOpts -> IO ()
schemeEntryPoint (LineOpts _ True) = Repl.mainLoop --repl
schemeEntryPoint (LineOpts script False) = runScript $ T.pack script


cliIface :: IO ()
cliIface = execParser opts >>= schemeEntryPoint
  where
    opts = info (helper <*> parseLineOpts)
      ( fullDesc
     <> header "Executable binary for Write You A Scheme v2.0"
     <> progDesc "contains an entry point for both running scripts and repl" )



