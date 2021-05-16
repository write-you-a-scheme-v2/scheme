{-# LANGUAGE OverloadedStrings #-}

module Repl (
  mainLoop,
) where

import Eval ( safeExec, evalText )
--import Eval ( runParseTest )
import Data.Text as T ( pack )

import Control.Monad.Trans ( MonadIO(liftIO) )
import System.Console.Haskeline
    ( defaultSettings, getInputLine, outputStrLn, runInputT, InputT )

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process input) >> repl
    --Just input -> (liftIO $ processToAST input) >> repl

process :: String -> IO ()
process str = do
  res <- safeExec $ evalText $ T.pack str
  either putStrLn return res

--processToAST :: String -> IO ()
--processToAST str = print $ runParseTest $ T.pack str
