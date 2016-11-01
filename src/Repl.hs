{-# LANGUAGE OverloadedStrings #-}

module Repl (
  mainLoop,
) where

import Eval
import Data.Text as T

import Control.Monad.Trans
import System.Console.Haskeline

type Repl a = InputT IO a

process :: String -> IO ()
process str = evalText $ T.pack str

processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str

repl :: Repl ()
repl = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> (liftIO $ process      input) >> repl
    --Just input -> (liftIO $ processToAST input) >> repl

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl
