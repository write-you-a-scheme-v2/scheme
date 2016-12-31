{-# LANGUAGE OverloadedStrings #-}

module Cli (
  cliIface
) where

import Eval -- evalFile :: T.Text -> IO ()
import Repl -- Repl.mainLoop :: IO ()
import System.Directory
import Data.Text.IO as TIO
import Options.Applicative

-- SOURCES
--http://book.realworldhaskell.org/read/io.html
-- https://github.com/pcapriotti/optparse-applicative
-- https://hackage.haskell.org/package/optparse-applicative

runScript ::  FilePath -> IO ()
runScript fname = do
  exists <- doesFileExist fname
  if exists
  then TIO.readFile fname >>= evalFile
  else TIO.putStrLn "File does not exist."

data LineOpts = LineOpts
  { script :: String
  , useRepl :: Bool }

parseLineOpts :: Parser LineOpts
parseLineOpts = LineOpts
    <$> strOption
        ( long "script"
       <> short 's'
       <> value ""
       <> metavar "SCRIPT"
       <> help "File containing the script you want to run")
    <*> switch
        ( long "repl"
       <> short 'r'
       <> help "Run as interavtive read/evaluate/print/loop")

schemeEntryPoint :: LineOpts -> IO ()
schemeEntryPoint (LineOpts _ True) = Repl.mainLoop --repl
schemeEntryPoint (LineOpts script False) = runScript script

cliIface :: IO ()
cliIface = execParser opts >>= schemeEntryPoint
  where
    opts = info (helper <*> parseLineOpts)
      ( fullDesc
     <> header "Executable binary for Write You A Scheme v2.0"
     <> progDesc "contains an entry point for both running scripts and repl" )
