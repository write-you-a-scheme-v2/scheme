{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader


type EnvCtx = Map.Map T.Text LispVal
newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool

instance Show LispVal where
  show = T.unpack . showVal

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }


showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom)     -> atom
    (String str)    -> T.concat [ "\"" ,str,"\""]
    (Number num)    -> T.pack $ show num
    (Bool True)     -> "#t"
    (Bool False)    -> "#f"
     Nil            -> "Nil"
    (List contents) -> T.concat ["(", unwordsList contents, ")"]
    (Fun _ )        -> "(internal function)"
    (Lambda _ _)    -> "(lambda function)"


unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $  showVal <$> list

-- TODO make a pretty printer
data LispError
  = NumArgs Integer [LispVal]
  | LengthOfList String Int
  | ExpectedList T.Text
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  | PError String

instance Show LispError where
  show = T.unpack . showError

showError :: LispError -> T.Text
showError err =
  case err of
    (NumArgs int args)       -> "Error Number Arguments"
    (LengthOfList sts int)   -> "Error Length of List"
    (ExpectedList txt)       -> "Error Expected List"
    (TypeMismatch str val)   -> T.concat ["Error Type Mismatch: ", T.pack str, showVal val]
    (BadSpecialForm str val) -> "Error Bad Special Form"
    (NotFunction str str1)   -> "Error Not a Function"
    (UnboundVar str str1)    -> "Error Unbound Variable"
    (PError str)             -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    (Default str)            -> T.concat ["Error, Danger Will Robinson! ", T.pack str]
    _                        -> "I got 99 problems, most of which is the parser"
