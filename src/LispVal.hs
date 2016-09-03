{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader

-- TODO make a pretty printer
data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  | LispErr T.Text
  deriving (Show)

--TODO decide if the return type should be Eval LispVal?
data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal } 

instance Show IFunc where
  show (IFunc f) = "internal function"


type EnvCtx = Map.Map T.Text LispVal
-- Add pop/push environments to Reader's EnvCtx
-- http://dev.stephendiehl.com/hask/#readert
newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a }
  -- Narrative: talk about generalized newtype deriving, monad trans, and IO/ExceptT complex
  -- http://dev.stephendiehl.com/hask/#newtype-deriving
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc Env T.Text
  -}
  | Bool Bool deriving (Show,Eq,Ord)


{-

--data Func = Func {
--        args :: [LispVal]
--      , body :: [LispVal]
--      , env :: Map.Map T.Text LispVal 
      }
     --, body :: [LispVal] -> Eval LispVal
--     , body :: [LispVal] -> undefined
-- use to get T.Text values, instead of typeclass show's String
showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) ->  "\"" ++ str ++ "\""
    (Number num) -> show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    (List contents) ->  "(" ++ unwordsList contents ++ ")"
    (DottedList head tail) ->  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
    (Fun _ ) -> "internal function"
    (Lambda _ ) -> "lambda function"
    (contents@[x:xs]) ->  "(" ++ unwordsList contents ++ ")"
showPairs :: [(LispVal,LispVal)] -> T.Text
showPairs val = concat $ (\x -> showVal (fst x) ++ " -> " ++ showVal (snd x) ++ "\n") <$> val

unwordsList :: [LispVal] -> T.Text
unwordsList = T.unwords . Prelude.map showVal

instance Show LispVal where
  show = T.unpack . showVal   
-}
