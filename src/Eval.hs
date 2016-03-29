{-# LANGUAGE OverloadedStrings #-}

module Eval (
) where

import Parser
import LispVal

import Data.Text as T
-- key/val store for environment
import Data.Map as Map
-- monadic transforms
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Except

data LispError = ErrorPlaceHold


type Store = Map.Map T.Text LispVal

data EnvCtx = EnvCtx { stack :: [Store],
   -- try a stack for now
                       global :: Store }


data MyPasswordError = Hg
--type Check = MaybeT (ExceptT MyPasswordError IO) String

type Eval a = ReaderT EnvCtx (ExceptT String IO ) a



ctxStack :: EnvCtx -> [Store]
ctxStack ctx = stack ctx ++ [global ctx]

emptyStore :: Map.Map Text LispVal
emptyStore = Map.empty


-- TODO: get this to work with local/ask
{-
pushFrame :: Eval ()
pushFrame = do
 st <- ask
 local $ st { stack = emptyStore : (stack st) }

popFrame :: Eval ()
popFrame = do
 ctx <- ask
 local $ ctx { stack = case stack ctx of
                       (top:rest) -> rest
                       []     -> [] }
-}
