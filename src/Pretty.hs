{-# LANGUAGE OverloadedStrings #-}

module Pretty (
) where

import Text.PrettyPrint.Leijen.Text
import Data.Text.Lazy (fromStrict)

import LispVal

instance Pretty LispVal where
  pretty expr = case expr of
    Atom str   -> text (fromStrict str)
    Number i   -> pretty i
    String s   -> dquotes (text (fromStrict s))
    Bool True  -> text "#t"
    Bool False -> text "#f"
    List ls    -> encloseSep lparen rparen space (fmap pretty ls)
