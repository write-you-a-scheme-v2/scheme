module Cli (
  cliIface
) where

import Eval -- evalFile :: T.Text -> IO ()
import Repl -- Repl.mainLoop :: IO ()
import LispVal
import Parser
import ANorm
import System.Directory
import Data.Text.IO as TIO
import qualified Data.Text as T
import Options.Applicative

-- SOURCES
--http://book.realworldhaskell.org/read/io.html
-- https://github.com/pcapriotti/optparse-applicative
-- https://hackage.haskell.org/package/optparse-applicative

runScript ::  FilePath -> IO ()
runScript fname = do
  exists <- doesFileExist fname
  if exists
  then TIO.readFile fname >>= evalFile fname
  else TIO.putStrLn "File does not exist."



printTransform :: LispVal -> IO ()
printTransform x = do
 print "input"
 print $ show  x
 let dx = deSugar x
 print "desugar"
 print $ show  dx
 let ax = toAnf dx
 print "anf"
 print $ show  ax
 return ()

tmpReduce :: T.Text -> IO ()
tmpReduce x = either (print . show) (printTransform) $ readExpr x


compileScript :: FilePath -> IO ()
compileScript fname = do
  exists <- doesFileExist fname
  if exists
  then TIO.readFile fname >>= tmpReduce
  else TIO.putStrLn "File does not exist."



data LineOpts = UseReplLineOpts 
  | RunScriptLineOpts String
  | RunCompileLineOpts String

parseLineOpts :: Parser LineOpts
parseLineOpts = runScriptOpt <|> runReplOpt <|> runCompile
  where
    runScriptOpt =
      RunScriptLineOpts <$> strOption (long "script"
                                       <> short 's'
                                       <> metavar "SCRIPT"
                                       <> help "File containing the script you want to run")
    runCompile = 
      RunCompileLineOpts <$> strOption (long "compile"
                                       <> short 'c'
                                       <> metavar "COMPILE"
                                       <> help "File containing the script you want to compile")
    runReplOpt =
      UseReplLineOpts <$ flag' () (long "repl"
                                   <> short 'r'
                                   <> help "Run as interavtive read/evaluate/print/loop")

schemeEntryPoint :: LineOpts -> IO ()
schemeEntryPoint UseReplLineOpts = Repl.mainLoop --repl
schemeEntryPoint (RunScriptLineOpts script) = runScript script
schemeEntryPoint (RunCompileLineOpts script) = compileScript script

cliIface :: IO ()
cliIface = execParser opts >>= schemeEntryPoint
  where
    opts = info (helper <*> parseLineOpts)
      ( fullDesc
     <> header "Executable binary for Write You A Scheme v2.0"
     <> progDesc "contains an entry point for both running scripts and repl" )
