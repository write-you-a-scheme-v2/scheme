module LispVal where

import qualified Data.Text as T

data LispVal
  = Atom T.Text
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String T.Text
  {-   ADD ME BACK IN
  | Internal Func
  | Lambda Func Env
  -}
  | Bool Bool deriving (Show)


data Func = Func {
      args :: [LispVal]
     --, body :: [LispVal] -> Eval LispVal
--     , body :: [LispVal] -> undefined
     }
{-
use to get T.Text values, instead of typeclass show's String
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

unwordsList :: [LispVal] -> T.Text
unwordsList = T.unwords . Prelude.map showVal

instance Show LispVal where
  show = T.unpack . showVal   
-}
