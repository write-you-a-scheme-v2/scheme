{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval (
   evalText
  , runParseTest
) where

import Parser
import Text.Parsec
import LispVal

import Data.Text as T
-- key/val store for environment
import Data.Map as Map
import Data.Monoid


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
    out <- runExceptT $ runAppT testEnv (textToEval textExpr)
    either print print out

textToEval :: T.Text -> Eval LispVal
textToEval input = either throwError eval (runParse_ input)

runParse_ :: T.Text -> Either LispError LispVal
runParse_ input = case (readExpr input) of
                (Right val) -> Right val
                (Left  err) -> Left $ Default "parser error"

runParseTest :: T.Text -> T.Text
runParseTest input = case (readExpr input) of
                (Right val) -> T.pack $ show val
                (Left  err) -> T.pack $ show err

evalParse :: T.Text -> IO ()
evalParse textExpr =
    print $ runParseTest textExpr

evalInEnv ::LispVal -> Map Text LispVal -> Eval LispVal
evalInEnv exp env = local (const env) (eval exp)

evalArgsExpEnv :: [LispVal] -> [LispVal] -> LispVal -> Eval LispVal
evalArgsExpEnv args params expr = 
    do bindVars $ Prelude.zipWith (,) params args
       eval expr
{-
printEnv :: EnvCtx -> Eval LispVal
printEnv env = 
 let pairs = toList $ showVal <$> Map.mapKeys showVal  env
 in return $ Atom $ showPairs pairs
-}


setLocal :: Text -> LispVal -> Map Text LispVal -> Eval LispVal
setLocal atom exp env = local (const $ Map.insert atom exp env) (eval exp)

setVar :: LispVal -> LispVal -> Eval LispVal
setVar n@(Atom atom) exp = do
  env <- ask
  case Map.lookup atom env of
      Just x -> setLocal  atom exp env
      Nothing -> throwError $ Default $ "setting an unbound var: " ++ show n
setVar _ exp =
  throwError $ Default "variables can only be assigned to atoms"

getVar :: LispVal ->  Eval LispVal
getVar n@(Atom atom) = do
  env <- ask
  case Map.lookup atom env of
      Just x -> return x
      Nothing -> throwError $ Default  $ "error on getVar: " ++ show n
getVar n =
  throwError $ Default $ "failure to get variable: " ++ show  n

defineVar :: LispVal -> LispVal -> Eval LispVal
defineVar n@(Atom atom) exp =
  do
    env <- ask
    setLocal atom exp env
defineVar _ exp = throwError $ Default "can only bind to Atom type valaues"

-- confirm this evals left to right
-- http://dev.stephendiehl.com/hask/#whats-the-point
bindVars :: [(LispVal, LispVal)] -> Eval ()
bindVars  = sequence_ . fmap (uncurry defineVar)

eval :: LispVal -> Eval LispVal
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil      = return Nil
eval n@(Atom _) = getVar n
--eval n@(Atom _) = return n
eval (List [Atom "quote",val]) = return $ val
eval (List [Atom "if", pred,ant,cons]) =
    do ifRes <- eval pred
       case ifRes of
         (Bool True) ->   (eval ant)
         (Bool False) ->  (eval cons)
         _           -> throwError (Default "ifelse must by T/F")
-- global definition
eval (List [Atom "def", Atom val, exp]) =
   defineVar (Atom val) exp
eval (List [Atom "define", Atom val, exp]) =
   defineVar (Atom val) exp

eval (List [Atom "let", pairs, expr]) = 
    do x <- evalToList pairs
       letPairs x
       eval expr

eval (List [Atom "lambda",params, expr]) = 
    do envLocal <- ask
       paramsEvald <- evalToList params
       return $ Lambda ( IFunc (\args -> (evalArgsExpEnv args paramsEvald expr))) envLocal 

eval (List [Atom fn, arg1, arg2]) = 
  do 
    fnVariable <- getVar $ Atom fn 
    v1 <- eval arg1
    v2 <- eval arg2
    case fnVariable of
      (Fun (IFunc internalFn)) -> internalFn [v1,v2]
      (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) (internalfn [v1,v2]) 
      _                -> throwError $ NotFunction "function" "not found???"
--
--List [Atom "+",Number 1,Number 2,Number 3]
--eval (List list@((Atom fn):args)   )   =
eval (List list@(x:xs)) = 
  do
    -- AW note: there should probably be a way to make sure
    -- this is actually a data constructor for Atom
    fnVariable <- eval x
    -- change this
    xVal <- evalToList $ List xs
    case fnVariable of
      (Fun ( IFunc internalFn)) -> internalFn xVal
      (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) (internalfn xVal) 
      _                -> throwError $ NotFunction "tried to evaluate non-function: " (show fnVariable)
--

evalToList :: LispVal -> Eval [LispVal]
evalToList (List expr) = do 
  mapM eval expr
evalToList _ = throwError $ Default "internal error, check evalToList"

letPairs :: [LispVal] -> Eval ()
letPairs x = 
  case x of
    []        -> throwError (Default "let")
    [one]     -> throwError (Default "let")
    (z:zs)    -> bindVars $ Prelude.zipWith (\a b -> (x !! a, x !! b))  (Prelude.filter Prelude.even [0..(Prelude.length x - 1)]) $ Prelude.filter Prelude.odd [0..(Prelude.length x -1)]


-- TODO: make internal form for primative
-- add two numbers
type Prim = [(T.Text, LispVal)]

primEnv :: Prim
primEnv = [   ("+" , Fun $ IFunc $ binopFold (numOp (+))        (Number 0) )
            , ("*" , Fun $ IFunc $ binopFold (numOp (*))        (Number 1) )
            , ("++", Fun $ IFunc $ binopFold (strOp (<>)) (String ""))
            , ("-" , Fun $ IFunc $ binop $ numOp (-))
            , ("cons", Fun $ IFunc $ Eval.cons)
            , ("cdr" , Fun $ IFunc $ Eval.cdr)
            , ("car" , Fun $ IFunc $ Eval.car)
            , ("quote", Fun $ IFunc $ quote)
            ]


type Unary = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

binop :: Binary -> [LispVal] -> Eval LispVal
binop op args@[x,y] = case args of
                            [a,b] -> op a b
                            _ -> throwError $ NumArgs 2 args

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
                            [a,b] ->  op a b
                            (a:as) -> do
                                        xVal <- evalToList $ List args 
                                        foldM op farg xVal
                            []-> throwError $ NumArgs 2 args

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp op _          _          = throwError $ TypeMismatch "+" (String "Number")

strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) =  return $ String $ op x y
strOp op _          _          =  throwError $ TypeMismatch "+" (String "String")

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

{-
 - List Comprehension

eval (List (Atom "cons"):rest) = 
    case rest of
        (x:xs) ->   do xval  <- eval x
                       xsval <- evalToList xs
                      case xsval of 
                           []       -> return $ List [xval]
                           [items]  -> return $ List xval:xsval
        [x]  -> throwError $ NumArgs 2 $ List []
        [] -> throwError $ NumArgs 2 $ List []

eval (List (Atom "car"):[arg]) = 
    do xval <- evalToList arg
       case xval of 
         x:_ -> return $ x
         [_]  -> return $ xval
         []         -> throwError (LispErr $ T.pack "car takes a list with one  or more items"

eval (List (Atom "cdr"):[arg]) = 
    do xval <- evalToList arg
       case xval of 
         _:xs   -> return $ List xs
         [_,y]  -> return $ List [y]
         _      -> throwError (LispErr $ T.pack "cdr takes a list with two  or more items"

 -}
