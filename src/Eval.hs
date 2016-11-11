{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval (
   evalText
  , evalFile
  , runParseTest
) where

import Parser
import Text.Parsec
import LispVal

import Data.Text as T
-- key/val store for environment
import Data.Map as Map
import Data.Monoid
import System.Directory
import System.IO
import System.Environment
import Control.Monad.Except
import Control.Monad.Reader


testEnv :: Map.Map T.Text LispVal
testEnv = Map.fromList $ [("x", Number 42)] <> primEnv

runAppT :: EnvCtx -> Eval b -> ExceptT LispError IO b
runAppT code action = do
    res <- liftIO $ runExceptT $ runReaderT (unEval action) code
    ExceptT $ return $ case res of
      Left b -> Left b
      Right a  -> Right a

-- REPL
evalText :: T.Text -> IO ()
evalText textExpr =
  do
    out <- runExceptT $ runAppT testEnv (textToEvalForm textExpr)
    either (putStrLn . show) (putStrLn . show) out


textToEvalForm :: T.Text -> Eval LispVal
textToEvalForm input = either (\x -> throwError $ PError $ show $ x )  eval (readExpr input)

-- Run file as script
evalFile :: T.Text -> IO ()
evalFile fileExpr =
  do
    out <- runExceptT $ runAppT testEnv (fileToEvalForm fileExpr)
    either (putStrLn . show) (putStrLn . show) out

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (const $ throwError $ Default "parser error")  evalBody (readExprFile input)

-- for viewing AST
runParseTest :: T.Text -> T.Text
runParseTest input = either (T.pack . show) (T.pack . show) $ readExpr input

getVar :: LispVal ->  Eval LispVal
getVar n@(Atom atom) = do
  env <- ask
  case Map.lookup atom env of
      Just x -> return x
      Nothing -> throwError $ Default  $ "error on getVar: " ++ show n
getVar n = throwError $ Default $ "failure to get variable: " ++ show  n

ensureAtom :: LispVal -> Eval LispVal
ensureAtom (Atom n) = return $ Atom n

extractVar :: LispVal -> T.Text
extractVar (Atom n) = n
-- `getEven [] = []; getEven (x:xs) = x : getOdd xs; getOdd [] = []; getOdd (x:xs) = getEven xs'
-- halve [] = ([],[]); halve (x:xs) = (x:zs,ys) where (ys,zs) = halve xs
getEven :: [t] -> [t]
getEven x = (\a -> (x !! a)) <$> (Prelude.filter Prelude.even [0..(Prelude.length x - 1)])
getOdd :: [t] -> [t]
getOdd x = (\a -> (x !! a))  <$> (Prelude.filter Prelude.odd  [1..(Prelude.length x - 1)])

  --return  $ Lambda ( IFunc ( applyLambda params expr)) envLocal
applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args =
    do env <- ask
       argEval <- evalToList $ List args
       local (const (Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params argEval) <> env)) (eval expr)

eval :: LispVal -> Eval LispVal
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil      = return Nil
eval n@(Atom _) = getVar n

-- we are making write a special form
-- the arguments to write will not be evaluated
-- instead they will be converted to a string.
eval arg@(List [(Atom "write"), rest]) = do
  return $ String $ T.pack $ show $ rest
eval arg@(List ((:) (Atom "write") rest)) = do
  return $ String $ T.pack $ show $ List rest

-- textToEvalForm
eval (List [Atom "quote",val]) = return $ val
eval (List [Atom "if", pred,ant,cons]) =
    do ifRes <- eval pred
       case ifRes of
         (Bool True) ->   (eval ant)
         (Bool False) ->  (eval cons)
         _           -> throwError (Default "ifelse must by T/F")
-- global definition
eval (List [(Atom "begin"),rest]) = do
  liftIO $ putStrLn " begin 1 rest"
  evalBody $ rest
eval (List (Atom "begin" : rest)) = do
  liftIO $ putStrLn " begin many rest"
  evalBody $ List rest

eval (List [Atom "let", List pairs, expr]) =
    do env <- ask
       atoms <- mapM ensureAtom $ getEven pairs
       vals <- evalToList $ List $ getOdd pairs
       local (const (Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) atoms vals) <> env)) (eval expr)

eval (List [Atom "let1", List [Atom atom,val], expr]) =
  do evalVal <- eval val
     env <- ask
     local (const $ Map.insert atom val env) (eval expr)

eval (List [Atom "letb", List pairs, expr]) =
    do env <- ask
       atoms <- mapM ensureAtom $ getEven pairs
       vals <- evalToList $ List $ getOdd pairs
       local (const (Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) atoms vals) <> env)) (evalBody expr)

eval (List [Atom "lambda",List params, expr]) =
    do envLocal <- ask
       return  $ Lambda ( IFunc ( applyLambda expr params )) envLocal

eval (List [Atom fn, arg1, arg2]) =
  do
    fnVariable <- getVar $ Atom fn
    v1 <- eval arg1
    v2 <- eval arg2
    case fnVariable of
      (Fun (IFunc internalFn)) -> internalFn [v1,v2]
      (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) (internalfn [v1,v2])
      _                -> throwError $ NotFunction "function" "not found???"

eval (List list@((:) x xs)) =
  do
    fnVariable <- eval x
    xVal <- evalToList $ List xs
    case fnVariable of
      (Fun ( IFunc internalFn)) -> internalFn xVal
      (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) (internalfn xVal)
      _                -> throwError $ NotFunction "tried to evaluate non-function: " (show fnVariable)
--

eval x = throwError $ Default $ "expression could not be evaluated: " ++ (show x)


-- when no defines are left
evalBody :: LispVal -> Eval LispVal
evalBody (List list@[List ((:) (Atom "define") [Atom var,defExpr] ),rest]) =
  do evalVal <- eval defExpr
     env <- ask
     local (const $ Map.insert var evalVal env) (eval rest)
evalBody (List list@((:) (List ((:) (Atom "define") [Atom var,defExpr] )) rest)) =
-- when more than one sub expr is left
  do evalVal <- eval defExpr
     env <- ask
     local (const $ Map.insert var evalVal env) (evalBody $ List rest)
evalBody x = do
  liftIO $ putStrLn "evalBody fallthrough"
  eval x




evalToList :: LispVal -> Eval [LispVal]
evalToList (List expr) = do
  mapM eval expr
evalToList _ = throwError $ Default "internal error, check evalToList"

{-
letPairs :: [LispVal] -> Eval ()
letPairs x =
  case x of
    []        -> throwError (Default "let")
    [one]     -> throwError (Default "let")
    (z:zs)    -> bindVars $ Prelude.zipWith (\a b -> (x !! a, x !! b))  (Prelude.filter Prelude.even [0..(Prelude.length x - 1)]) $ Prelude.filter Prelude.odd [0..(Prelude.length x -1)]
-}

type Prim = [(T.Text, LispVal)]
primEnv :: Prim
primEnv = [   ("+"    , Fun $ IFunc $ binopFold (numOp    (+))  (Number 0) )
            , ("*"    , Fun $ IFunc $ binopFold (numOp    (*))  (Number 1) )
            , ("++"   , Fun $ IFunc $ binopFold (strOp    (<>)) (String ""))
            , ("-"    , Fun $ IFunc $ binop $    numOp    (-))
            , ("<"    , Fun $ IFunc $ binop $    numCmp   (<))
            , ("<="   , Fun $ IFunc $ binop $    numCmp   (<=))
            , (">"    , Fun $ IFunc $ binop $    numCmp   (>))
            , (">="   , Fun $ IFunc $ binop $    numCmp   (>=))
            , ("=="   , Fun $ IFunc $ binop $    numCmp   (==))
            , ("even?", Fun $ IFunc $ unop $     numBool   even)
            , ("odd?" , Fun $ IFunc $ unop $     numBool   odd)
            , ("pos?" , Fun $ IFunc $ unop $     numBool ((<) 0))
            , ("neg?" , Fun $ IFunc $ unop $     numBool ((>) 0))
            , ("eq?"  , Fun $ IFunc $ binop     (eqOp     (==)))
            , ("and"  , Fun $ IFunc $ binopFold (eqOp     (&&)) (Bool True))
            , ("or"   , Fun $ IFunc $ binopFold (eqOp     (||)) (Bool False))
            , ("cons" , Fun $ IFunc $ Eval.cons)
            , ("cdr"  , Fun $ IFunc $ Eval.cdr)
            , ("car"  , Fun $ IFunc $ Eval.car)
            , ("quote", Fun $ IFunc $ quote)
            , ("read" , Fun $ IFunc $ unop $ readFn)
            , ("file?" , Fun $ IFunc $ unop $ fileExists)
            , ("slurp" , Fun $ IFunc $ unop $ slurp)
            ]


type Unary = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

unop :: Unary -> [LispVal] -> Eval LispVal
unop op args@[x] = op x
unop op args     = throwError $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op args@[x,y] = case args of
                            [a,b] -> op a b
                            _ -> throwError $ NumArgs 2 args


readFn :: LispVal -> Eval LispVal
readFn x = do
  val <- eval  x
  case val of
    (String txt) -> textToEvalForm txt
    _            -> throwError $ TypeMismatch "read expects string, instead got: " val



fileExists :: LispVal -> Eval LispVal
fileExists x@(Atom lbl) = do
  val <- eval x
  fileExists val
fileExists (String txt) = do
  exists <- liftIO $ doesFileExist $ T.unpack txt
  case (exists) of
    True -> return $ Bool True
    _    -> return $ Bool False
fileExists val = throwError $ TypeMismatch "read expects string, instead got: " val

slurp :: LispVal -> Eval LispVal
slurp x = do
  val <- eval x
  case val of
    (String txt ) -> readTextFile txt
    _             -> throwError $ TypeMismatch "read expects string, instead got: " val
-- (slurp "test/let.scheme")
-- get this to work with "fileToEvalForm"
readTextFile ::  T.Text -> Eval LispVal
readTextFile script =  do
  inh <- liftIO $ openFile (T.unpack script) ReadMode
  ineof <- liftIO $ hIsEOF inh
  if ineof
    then  (liftIO $ putStr "empty file\n") >> (throwError $ Default "empty file")
      else do fileText <- liftIO $ hGetContents $ inh
              --liftIO $ hClose inh
              liftIO $ putStr "FileContents:\n"
              liftIO $ putStr fileText
              textToEvalForm $ T.pack fileText


-- drop "fargs" and use case statement to pull out first vs. rest of args?
-- this only dispatches when # args is > 2 or when there is =/= 2 args?
binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
                            [a,b] -> op a b
                            (a:as) -> foldM op farg args
                            []-> throwError $ NumArgs 2 args

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool op  x         = throwError $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp op x          (Number y) = throwError $ TypeMismatch "numeric op " x
numOp op (Number x)  y         = throwError $ TypeMismatch "numeric op " y
numOp op x           y         = throwError $ TypeMismatch "numeric op " (String $ T.pack $ show x ++ show y)
strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp op x          (String y) = throwError $ TypeMismatch "string op " x
strOp op (String x)  y         = throwError $ TypeMismatch "string op " y
strOp op x           y         = throwError $ TypeMismatch "string op " (String $ T.pack $ show x ++ show y)
-- (==) (||) (&&)
eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp op  x       (Bool y) = throwError $ TypeMismatch "bool op " x
eqOp op (Bool x)  y       = throwError $ TypeMismatch "bool op " y
eqOp op x         y       = throwError $ TypeMismatch "bool op " (String $ T.pack $ show x ++ show y)
-- (<) (>) (>=) (<=)
numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return $ Bool $ op x  y
numCmp op x          (Number y) = throwError $ TypeMismatch "numeric op " x
numCmp op (Number x)  y         = throwError $ TypeMismatch "numeric op " y
numCmp op x         y           = throwError $ TypeMismatch "numeric op " (String $ T.pack $ show x ++ show y)

-- better wa to check args?
cons :: [LispVal] -> Eval LispVal
cons [x,y@(List _)] = do
  xval  <- eval x
  lvals <- evalToList y
  return $ List $ x:lvals
cons [c] = do
  val <- eval c
  return $ List [c]
cons [] = return $ List []

car :: [LispVal] -> Eval LispVal
car [List []    ] = return $ Nil
car [List (x:_)] = return $ x

cdr :: [LispVal] -> Eval LispVal
cdr [List (x:xs)] = return $ List xs
--cdr [List [x]]     = return $ List [x]
cdr [(List [])]  = return $ Nil
cdr []           = return $ Nil
--
--
quote :: [LispVal] -> Eval LispVal
quote [List xs]  = return $ List ((Atom "quote"):xs)
quote [exp]      = return $ List [Atom "quote",exp]

-- default return to Eval monad (no error handling)
binopFixPoint :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> Eval LispVal
binopFixPoint f2 = binop $ (\x y -> return $ f2 x y)

numOpVal :: (Integer -> Integer -> Integer ) -> LispVal -> LispVal -> LispVal
numOpVal op (Number x) (Number y) = Number $ op x  y
