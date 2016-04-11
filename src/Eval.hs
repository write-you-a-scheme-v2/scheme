{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (
) where

import Parser
import LispVal

import Data.Text as T
-- key/val store for environment
import Data.Map as Map
-- monadic transforms
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Either




-- TODO
-- Clean up Lisp error throwing
-- Figure out way to raed in values from Eval monad
-- Proper Throw/Catch mechanism
-- Bind multiple vars (using MapM?)
data LispError = LispErr T.Text

newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a
} deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadIO)


type EnvCtx = Map.Map T.Text LispVal

runAppT :: EnvCtx -> Eval a -> EitherT LispError  IO a
runAppT code action = do
    res <- liftIO $ runExceptT $ runReaderT (unEval action) code

    EitherT $ return $ case res of
      Left b -> Left b
      Right a  -> Right a

setVar :: LispVal -> LispVal -> Eval LispVal
setVar n@(Atom atom) exp = do
  env <- ask 
  case Map.lookup atom env of
      Just x -> local (const $ Map.insert atom exp env) (eval exp)
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
    case (Map.lookup atom env) of
         Just x  -> local (const $ Map.insert atom exp env) (eval exp)
         Nothing -> local (const $ Map.insert atom exp env) (eval exp)
defineVar _ exp = throwError $ LispErr $ T.pack "can only bind to Atom type valaues"
{-
bindVars :: [(LispVal, LispVal)] -> Eval ()
bindVars def = return $ liftM $ mapM_ (uncurry defineVar) def
-}
eval :: LispVal -> Eval LispVal
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval n@(Atom atom) = getVar n
eval (List [Atom "if", pred,ant,cons]) = 
    do env <- ask
       ifRes <- eval pred
       case ifRes of
         (Bool True) -> (eval ant)
         (Bool False) ->  (eval cons)
         _           -> throwError (LispErr $ T.pack "ifelse must by T/F")
eval (List [Atom "define", (Atom val), exp]) = 
   defineVar (Atom val) exp
eval (List (Atom func : args)) = apply func $ Prelude.map eval args


