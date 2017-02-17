{-# LANGUAGE FlexibleContexts #-}

module Parser (
  readExpr,
  readExprFile
) where

import LispVal

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Data.Char (digitToInt)
import Control.Monad (mzero)

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
  Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = ";"
  , Tok.opStart = mzero
  , Tok.opLetter = mzero
  , Tok.identStart = letter <|> oneOf "!$%&*/:<=>?^_~"
  , Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
  }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
  where
  specialIdentifier :: Parser String
  specialIdentifier = Tok.lexeme lexer $ try $
    string "-" <|> string "+" <|> string "..."

-- | Parse a radix, returns the pair (base, parser for digits in base)
radix :: Parser (Integer, Parser Char)
radix =
  try (string "#b") *> return (2,bit) <|>
  try (string "#o") *> return (8,octDigit) <|>
  try (string "#d") *> return (10,digit) <|>
  try (string "#x") *> return (16,hexDigit) <|>
  return (10, digit)
  where
    bit = oneOf "01"

-- | Parse an integer, given a radix as output by @radix@.
-- Copied from Text.Parsec.Token
numberWithRadix :: (Integer, Parser Char) -> Parser Integer
numberWithRadix (base, baseDigit) = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

-- | Parse a sign, return either @id@ or @negate@ based on the sign parsed.
-- Copied from Text.Parsec.Token
sign :: Parser (Integer -> Integer)
sign = char '-' *> return negate
   <|> char '+' *> return id
   <|> return id

-- | Parses a scheme integer with arbitrary radix and sign
-- @Text.Parsec.Token.integer@ is insufficient for our purposes, for the
-- following reasons:
-- - Parsec puts sign before radix, Scheme puts radix before sign
-- - Parsec doesn't support binary integers
-- - Scheme uses #o, #x, ... instead of 0o, 0x, and Parsec's 0-prefix is
-- embedded in @integer@
integer :: Parser Integer
integer = Tok.lexeme lexer int <?> "integer"
  where
    int = do
      r <- radix
      f <- sign
      n <- numberWithRadix r
      return (f n)

textLiteral :: Parser T.Text
textLiteral = T.pack <$> Tok.stringLiteral lexer

boolean :: Parser Bool
boolean = try (string "#t") *> return True
  <|> try (string "#f") *> return False
  <?> "boolean"

nil :: Parser ()
nil = try (optional (char '\'') *> string "()") *> return () <?> "nil"

-- TODO: It would be nice to gather all the cases that begin with '#' together
-- and fork based on that, rather than having the cases split up in multiple
-- locations and having to `try` all over the place.

lispVal :: Parser LispVal
lispVal = Nil <$ nil
  <|> Bool <$> boolean
  <|> Number <$> try integer
  <|> Atom <$> identifier
  <|> String <$> textLiteral
  <|> _Quote <$> quoted lispVal
  <|> List <$> parens manyLispVal

manyLispVal :: Parser [LispVal]
manyLispVal = lispVal `sepBy` whitespace

_Quote :: LispVal -> LispVal
_Quote x = List [Atom "quote", x]

contents p = whitespace *> p <* whitespace <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents lispVal) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents (List <$> manyLispVal))
