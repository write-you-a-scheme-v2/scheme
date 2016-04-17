{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Eval (
   evalText
) where

import Parser
import Text.Parsec
import LispVal

import Data.Text as T
-- key/val store for environment
import Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Except

data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  | LispErr T.Text
  deriving (Show)

-- TODO
-- Add pop/push environments to Reader's EnvCtx

newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadIO)

type EnvCtx = Map.Map T.Text LispVal

testEnv :: Map.Map T.Text LispVal
testEnv = Map.fromList [("x", Number 42)]

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

runParse_ :: Text -> Either LispError LispVal
runParse_ input = case (Parser.readExpr input) of
                (Right val) -> Right val
                (Left  err) -> Left $ Parser err

setLocal :: Text -> LispVal -> Map Text LispVal -> Eval LispVal
setLocal atom exp env = local (const $ Map.insert atom exp env) (eval exp)

setVar :: LispVal -> LispVal -> Eval LispVal
setVar n@(Atom atom) exp = do
  env <- ask
  case Map.lookup atom env of
      Just x -> setLocal  atom exp env
      Nothing -> throwError $ LispErr $ T.pack "setting an unbound var"
setVar _ exp =
  throwError $ LispErr $ T.pack $ "variables can only be assigned to atoms"

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


-- TODO: make internal form for primative
-- prims :: [(T.Text, [LispVal] -> Eval LispVal)]
--          [("+", binop $ numOp (+))

type Unary = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

binop :: Binary -> [LispVal] -> Eval LispVal
binop op args@[x,y] = case args of
                            [a,b] -> op a b
                            _ -> throwError $  NumArgs 2 args

numOp :: (Integer -> Integer -> Integer ) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp op _          _          = throwError $ TypeMismatch "+" (String "Number")

-- default return to Eval monad (no error handling)
binopFixPoint :: (LispVal -> LispVal -> LispVal) -> [LispVal] -> Eval LispVal
binopFixPoint f2 = binop $ (\x y -> return $ f2 x y)

numOpVal :: (Integer -> Integer -> Integer ) -> LispVal -> LispVal -> LispVal
numOpVal op (Number x) (Number y) = Number $ op x  y
