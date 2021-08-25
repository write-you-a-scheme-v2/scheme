{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal (
  LispVal(..),
  Eval(..),
  IFunc(..),
  EnvCtx(..),
  LispException(..),
  showVal,
) where

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Exception
import Control.Monad.Reader

type ValCtx = Map.Map T.Text LispVal
type FnCtx  = Map.Map T.Text LispVal


data EnvCtx = EnvCtx
  { env :: ValCtx
  , fenv :: FnCtx
  } deriving (Eq)

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadIO)

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Eq)

instance Show LispVal where
  show = T.unpack . showVal

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }

instance Eq IFunc where
 (==) _ _ = False

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom)     -> atom
    (String txt)    -> T.concat [ "\"" , txt, "\""]
    (Number num)    -> T.pack $ show num
    (Bool True)     -> "#t"
    (Bool False)    -> "#f"
    Nil             -> "'()"
    (List contents) -> T.concat ["(", unwordsList contents, ")"]
    (Fun _ )        -> "(internal function)"
    (Lambda _ _)    -> "(lambda function)"


unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $  showVal <$> list

-- TODO make a pretty printer
data LispException
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String -- from show anyway
  | IOError T.Text

instance Exception LispException


instance Show LispException where
  show = T.unpack . showError

showError :: LispException -> T.Text
showError err =
  case err of
    (IOError txt)            -> T.concat ["Error reading file: ", txt]
    (NumArgs int args)       -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
    (LengthOfList txt int)   -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    (ExpectedList txt)       -> T.concat ["Error Expected List in funciton ", txt]
    (TypeMismatch txt val)   -> T.concat ["Error Type Mismatch: ", txt, showVal val]
    (BadSpecialForm txt)     -> T.concat ["Error Bad Special Form: ", txt]
    (NotFunction val)        -> T.concat ["Error Not a Function: ", showVal val]
    (UnboundVar txt)         -> T.concat ["Error Unbound Variable: ", txt]
    (PError str)             -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    (Default val)            -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]
