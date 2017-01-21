{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

) where

import Prim
import Parser
import LispVal

import Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory

import Text.Parsec

import Control.Monad.Reader
import Control.Exception

basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList $ primEnv
          <> [("read" , Fun $ IFunc $ unop readFn),
             ("parse", Fun $ IFunc $ unop parseFn),
             ("eval", Fun $ IFunc $ unop eval),
             ("show", Fun $ IFunc $ unop (return . String . showVal))]

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


evalFile :: T.Text -> IO () --program file
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr) >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show )  evalBody $ readExprFile input

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

sTDLIB :: T.Text
sTDLIB = "lib/stdlib.scm"

endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [expr]
endOfList n _  = throw $ TypeMismatch  "failure to get variable: " n

parseWithLib :: T.Text -> T.Text -> Either ParseError LispVal
parseWithLib std inp = do
  stdlib <- readExprFile std
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
  stdlib <- getFileContents $ T.unpack  sTDLIB
  res <- runASTinEnv basicEnv $ textToEvalForm stdlib textExpr
  print res

getVar :: LispVal ->  Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
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
applyLambda expr params args = do
  env <- ask
  argEval <- mapM eval args
  local (const ((Map.fromList (zipWith (\a b -> (extractVar a,b)) params argEval)) <> env)) $ eval expr


eval :: LispVal -> Eval LispVal
eval (List [Atom "dumpEnv", x]) = do
  env <- ask
  liftIO $ print $  toList env
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

eval (List [Atom "define", varExpr, expr]) = do --top-level define
  env     <- ask
  varAtom <- ensureAtom varExpr
  evalVal <- eval expr
  local (const $ Map.insert (extractVar varAtom) evalVal env) $ return varExpr

eval (List [Atom "let", List pairs, expr]) = do
  env   <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals  <- mapM eval       $ getOdd  pairs
  local (const (Map.fromList (zipWith (\a b -> (extractVar a, b)) atoms vals) <> env))  $ evalBody expr
eval (List (Atom "let":_) ) = throw $ BadSpecialForm "let funciton expects list of parameters and S-Expression body\n(let <pairs> <s-expr>)"


eval (List [Atom "lambda", List params, expr]) = do
  envLocal <- ask
  return  $ Lambda (IFunc $ applyLambda expr params) envLocal
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
  env    <- ask
  funVar <- eval x
  xVal   <- mapM eval  xs
  --liftIO $ TIO.putStr $ T.concat ["eval:\n  ", T.pack $ show all,"\n  * fnCall:  ", T.pack $ show x, "\n  * fnVar  ", T.pack $ show funVar,"\n  * args:  ",T.concat (T.pack . show <$> xVal)    ,T.pack "\n"]
  case funVar of
      (Fun (IFunc internalFn)) -> internalFn xVal
      (Lambda (IFunc definedFn) boundenv) -> local (const (boundenv <> env)) $ definedFn xs

      _                -> throw $ NotFunction funVar

eval x = throw $ Default  x --fall thru

evalBody :: LispVal -> Eval LispVal
evalBody x@(List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  env     <- ask
  local (const $ Map.insert var evalVal env) $ eval rest

evalBody x@(List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  --liftIO $ print x
  evalVal <- eval defExpr
  env     <- ask
  local (const $ Map.insert var evalVal env) $ evalBody $ List rest

evalBody x = eval x
