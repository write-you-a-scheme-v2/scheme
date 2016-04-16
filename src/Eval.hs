{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval (
evalTextExpr
) where

import Parser
import Text.Parsec
import LispVal

import Data.Text as T
-- key/val store for environment
import Data.Map as Map
-- monadic transforms
import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Either
--import Control.Monad.Error.Class

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               | LispErr T.Text deriving (Show)


readTextExpr :: Text -> Either LispError LispVal
readTextExpr input = case (readExpr input) of
                (Right val) -> Right val
                (Left  err) -> Left $ Parser err


-- TODO
-- Add pop/push environments to Reader's EnvCtx

-- Note: the use of IO here is limited to lisp functions that read/write to
-- files and catch exceptions. 
newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a
} deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadIO)


type EnvCtx = Map.Map T.Text LispVal

-- Basic Input from going from String -> Evalualated Lisp code
-- for a given input string, runTextToEval, then pass the result into runAppT along with the prim environment

textToEval :: T.Text -> Eval LispVal
textToEval input = either throwError return (readTextExpr input) 
--
-- action => EnvCtx
-- code => T.Txt of Lisp Text, ex: "(let [x 1])"
--runAppT :: MonadIO m => EnvCtx -> Eval b -> EitherT LispError IO b
runAppT :: EnvCtx -> Eval b -> EitherT LispError IO b
runAppT code action = do
    res <- liftIO $ runExceptT $ runReaderT (unEval action) code

    EitherT $ return $ case res of
      Left b -> Left b
      Right a  -> Right a


testEnv :: Map.Map T.Text LispVal 
testEnv = Map.fromList [("x", Number 42)]

evalTextExpr :: T.Text -> IO ()
evalTextExpr textExpr = 
  do
    out <- runEitherT $ runAppT testEnv (textToEval textExpr) 
    either print print out
      --Right x -> print x 
      --
-- helper function for setting ReaderT's local function
setLocal :: Text -> LispVal -> Map Text LispVal -> Eval LispVal
setLocal atom exp env = local (const $ Map.insert atom exp env) (eval exp)

-- set a defined var within the local environment
setVar :: LispVal -> LispVal -> Eval LispVal
setVar n@(Atom atom) exp = do
  env <- ask 
  case Map.lookup atom env of
      Just x -> setLocal  atom exp env
      Nothing -> throwError $ LispErr $ T.pack "setting an unbound var"
setVar _ exp = 
  throwError $ LispErr $ T.pack $ "variables can only be assigned to atoms"

--get a var within the local environment
getVar :: LispVal ->  Eval LispVal
getVar n@(Atom atom) = do
  env <- ask
  case Map.lookup atom env of
      Just x -> return x
      Nothing -> throwError $ LispErr $ T.pack $ "error on getVar"
getVar _  = 
  throwError $ LispErr $ T.pack $ "variables can only be assigned to atoms"

defineVar :: LispVal -> LispVal -> Eval LispVal
defineVar n@(Atom atom) exp = 
  do 
    env <- ask 
    setLocal atom exp env
defineVar _ exp = throwError $ LispErr $ T.pack "can only bind to Atom type valaues"

-- confirm this evals left to right
bindVars :: [(LispVal, LispVal)] -> Eval ()
bindVars  = sequence_ . fmap (uncurry defineVar) 

eval :: LispVal -> Eval LispVal
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval n@(Atom atom) = getVar n
eval (List [Atom "quote", val]) = return $ val
eval (List [Atom "if", pred,ant,cons]) = 
    do env <- ask
       ifRes <- eval pred
       case ifRes of
         (Bool True) -> (eval ant)
         (Bool False) ->  (eval cons)
         _           -> throwError (LispErr $ T.pack "ifelse must by T/F")
-- global definition
eval (List [Atom "define", (Atom val), exp]) = 
   defineVar (Atom val) exp
--eval (List (Atom func : args)) = apply func $ Prelude.map eval args



type Unary = LispVal -> Eval LispVal
-- ex: (numOp (+)) 
type Binary = LispVal -> LispVal -> Eval LispVal


binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x,y] = op x y 

--binop op :[]     = throwError $ NumArgs 2 [x,y] 
--

binopFixPoint :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> Eval LispVal
binopFixPoint f2 = binop $ (\x y -> return $ f2 x y)
  
numOp :: (Integer -> Integer -> Integer ) -> LispVal -> LispVal -> LispVal
numOp op (Number x) (Number y) = Number $ op x  y 


