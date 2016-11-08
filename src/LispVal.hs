{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import qualified Data.Text as T
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.Reader


-- Add pop/push environments to Reader's EnvCtx
-- http://dev.stephendiehl.com/hask/#readert
  -- Narrative: talk about generalized newtype deriving, monad trans, and IO/ExceptT complex
  -- http://dev.stephendiehl.com/hask/#newtype-deriving

type EnvCtx = Map.Map T.Text LispVal
newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadIO)
-- do lists as car cadr, [a] -> foldl1 (cons) [a]
data LispVal
  = Atom T.Text
  | List [LispVal]
  -- | DottedList [LispVal] LispVal
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx 
  | Nil
  | Bool Bool -- deriving (Show)

instance Show LispVal where
  show = T.unpack . showVal   

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal } 
instance Show IFunc where
  show (IFunc f) = "internal function"
instance Eq IFunc where
  (==) (IFunc f) (IFunc g) = False 


showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) -> T.concat [ "\"" ,str,"\""]
    --(String str) ->  T.pack "\"") ++  str ++ $ T.pack "\""
    (Number num) -> T.pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil        -> "Nil"
    --(List contents) ->  "(" ++ (unwordsList contents) ++ ")"
    (List contents) -> T.concat ["(", unwordsList contents, ")"]
    --(DottedList head tail) ->  T.unwords ["(" ,unwordsList head, " . " , showVal tail , ")"]
    (Fun _ ) -> "(internal function)"
    (Lambda _ _) -> "(lambda function)"

--showPairs :: [(LispVal,LispVal)] -> T.Text
--showPairs val = concat  (\x -> showVal (fst x) ++ " -> " ++ showVal (snd x) ++ "\n") <$> val

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
  --deriving (Show)

instance Show LispError where
  show = T.unpack . showError

showError :: LispError -> T.Text
showError err = 
  case err of
    (NumArgs int args)     -> "Error Number Arguments"
    (LengthOfList sts int) -> "Error Length of List"
    (ExpectedList txt)     -> "Error Expected List"
    (TypeMismatch str val) -> T.concat ["Error Type Mismatch: ", T.pack str, showVal val]
    (BadSpecialForm str val)-> "Error Bad Special Form"
    (NotFunction str str1)  -> "Error Not a Function"
    (UnboundVar str str1)   -> "Error Unbound Variable"
    (PError str)            -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    (Default str)          -> T.concat ["Error, Danger Will Robinson! ", T.pack str]
    _                      -> "I got 99 problems, most of which is the parser"

