{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Eval (
  evalText,
  evalFile,
  runParseTest,
  safeExec,
  -- testing
  runASTinEnv,
  basicEnv,
  fileToEvalForm,
  textToEvalForm,
  getFileContents
) where

import Prim
import Parser
import LispVal

import Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory

import Text.Parsec

import Control.Monad.Reader
import Control.Exception

funcEnv :: Map.Map T.Text LispVal
funcEnv = Map.fromList $ primEnv
          <> [("read" , Fun $ IFunc $ unop readFn),
             ("parse", Fun $ IFunc $ unop parseFn),
             ("eval", Fun $ IFunc $ unop eval),
             ("show", Fun $ IFunc $ unop (return . String . showVal))]

basicEnv :: EnvCtx
basicEnv = EnvCtx Map.empty funcEnv

readFn :: LispVal -> Eval LispVal
readFn (String txt) = lineToEvalForm txt
readFn  val         = throw $ TypeMismatch "read expects string, instead got: " val

parseFn :: LispVal -> Eval LispVal
parseFn (String txt) = either (throw . PError . show) return $ readExpr txt
parseFn val = throw $ TypeMismatch "parse expects string, instead got: " val


safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) ->
      case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                -> return $ Left (show eTop)
    Right val -> return $ Right val

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

lineToEvalForm :: T.Text -> Eval LispVal
lineToEvalForm input = either (throw . PError . show  )  eval $ readExpr input


evalFile :: FilePath -> T.Text -> IO () --program file
evalFile filePath fileExpr = (runASTinEnv basicEnv $ fileToEvalForm filePath fileExpr) >>= print

fileToEvalForm :: FilePath -> T.Text -> Eval LispVal
fileToEvalForm filePath input = either (throw . PError . show )  evalBody $ readExprFile filePath input

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

sTDLIB :: FilePath
sTDLIB = "lib/stdlib.scm"

endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [expr]
endOfList n _  = throw $ TypeMismatch  "failure to get variable: " n

parseWithLib :: T.Text -> T.Text -> Either ParseError LispVal
parseWithLib std inp = do
  stdlib <- readExprFile sTDLIB std
  expr   <- readExpr inp
  return $ endOfList stdlib expr


getFileContents :: FilePath -> IO T.Text
getFileContents fname = do
  exists <- doesFileExist fname
  if exists then TIO.readFile  fname else return "File does not exist."

textToEvalForm :: T.Text -> T.Text -> Eval LispVal
textToEvalForm std input = either (throw . PError . show )  evalBody $ parseWithLib std input

evalText :: T.Text -> IO () --REPL
evalText textExpr = do
  stdlib <- getFileContents sTDLIB
  res <- runASTinEnv basicEnv $ textToEvalForm stdlib textExpr
  print res

getVar :: LispVal ->  Eval LispVal
getVar (Atom atom) = do
  EnvCtx{..} <- ask
  case Map.lookup atom (Map.union fenv env) of -- lookup, but prefer functions
      Just x  -> return x
      Nothing -> throw $ UnboundVar atom
getVar n = throw $ TypeMismatch  "failure to get variable: " n

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom txt) = return  n
ensureAtom n = throw $ TypeMismatch "expected an atomic value" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
extractVar n = throw $ TypeMismatch "expected an atomic value" n

getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x:xs) = getEven xs


applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = bindArgsEval params args expr


bindArgsEval :: [LispVal] -> [LispVal] -> LispVal -> Eval LispVal
bindArgsEval params args expr = do
  EnvCtx{..} <- ask
  let newVars = zipWith (\a b -> (extractVar a,b)) params args
  let (newEnv, newFenv) =  Map.partition (not . isLambda) $ Map.fromList newVars
  local (const $ EnvCtx (newEnv <> env) (newFenv <> fenv)) $ eval expr


isLambda :: LispVal -> Bool
isLambda (List ((Atom "lambda"):_)) = True
isLambda _  = False


eval :: LispVal -> Eval LispVal
eval (List [Atom "dumpEnv", x]) = do
  EnvCtx{..} <- ask
  liftIO $ print $  toList env
  liftIO $ print $  toList fenv
  eval x
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil        = return Nil
eval n@(Atom _) = getVar n

eval (List [Atom "showSF", rest])      = return . String . T.pack $ show rest
eval (List ((:) (Atom "showSF") rest)) = return . String . T.pack . show $ List rest

eval (List [Atom "quote", val]) = return val

eval (List [Atom "if", pred, truExpr, flsExpr]) = do
  ifRes        <- eval pred
  case ifRes of
    (Bool True)  -> eval truExpr
    (Bool False) -> eval flsExpr
    _            -> throw $ BadSpecialForm "if's first arg must eval into a boolean"
eval args@(List ( (:) (Atom "if") _))  = throw $ BadSpecialForm "(if <bool> <s-expr> <s-expr>)"

eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest

eval (List [Atom "define", varExpr, defExpr]) = do --top-level define
  EnvCtx{..} <- ask
  varAtom <- ensureAtom varExpr
  evalVal <- eval defExpr
  bindArgsEval [varExpr] [defExpr] varExpr

eval (List [Atom "let", List pairs, expr]) = do
  EnvCtx{..} <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals  <- mapM eval       $ getOdd  pairs
  bindArgsEval atoms vals expr
eval (List (Atom "let":_) ) = throw $ BadSpecialForm "let function expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"


eval (List [Atom "lambda", List params, expr]) = do
  ctx <- ask
  return  $ Lambda (IFunc $ applyLambda expr params) ctx
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda function expects list of parameters and S-Expression body\n(lambda <params> <s-expr>)"


-- needed to get cadr, etc to work
eval all@(List [Atom "cdr", List [Atom "quote", List (x:xs)]]) =
  return $  List xs
eval all@(List [Atom "cdr", arg@(List (x:xs))]) =
  case x of
      -- proxy for if the list can be evaluated
      Atom  _ -> do val <- eval arg
                    eval $ List [Atom "cdr", val]
      _           -> return $ List xs


eval all@(List [Atom "car", List [Atom "quote", List (x:xs)]]) =
  return $  x
eval all@(List [Atom "car", arg@(List (x:xs))]) =
  case x of
      Atom _       -> do val <- eval arg
                         eval $ List [Atom "car", val]
      _            -> return $ x


eval all@(List ((:) x xs)) = do
  EnvCtx{..} <- ask
  funVar <- eval x
  xVal <- mapM eval xs
  --liftIO $ TIO.putStr $ T.concat ["eval:\n  ", T.pack $ show all,"\n  * fnCall:  ", T.pack $ show x, "\n  * fnVar  ", T.pack $ show funVar,"\n  * args:  ",T.concat (T.pack . show <$> xVal)    ,T.pack "\n"]
  case funVar of
      (Fun (IFunc internalFn)) -> internalFn xVal
      (Lambda (IFunc definedFn) (EnvCtx benv bfenv)) -> local (const $ EnvCtx benv fenv) $ definedFn xVal

      _                -> throw $ NotFunction funVar

eval x = throw $ Default  x --fall thru


updateEnv :: T.Text -> LispVal -> EnvCtx -> EnvCtx
updateEnv var e@(Fun _) EnvCtx{..} =  EnvCtx env $ Map.insert var e fenv
updateEnv var e@(Lambda _ _) EnvCtx{..} = EnvCtx env $ Map.insert var e fenv
updateEnv var e  EnvCtx{..} = EnvCtx (Map.insert var e env) fenv

evalBody :: LispVal -> Eval LispVal
evalBody x@(List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ updateEnv var evalVal ctx) $ eval rest

evalBody x@(List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ updateEnv var evalVal ctx) $ evalBody $ List rest

evalBody x = eval x
