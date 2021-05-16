{-# LANGUAGE FlexibleContexts #-}

module Parser (
  readExpr,
  readExprFile
) where

import LispVal ( LispVal(List, Bool, Nil, Number, String, Atom) )

import Text.Parsec
    ( char,
      digit,
      hexDigit,
      letter,
      octDigit,
      oneOf,
      string,
      eof,
      many1,
      sepBy,
      (<?>),
      (<|>),
      parse,
      try,
      ParseError,
      SourceName )
import Text.Parsec.Text ( Parser )
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Data.List ( foldl' )
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

lexeme :: Parser a -> Parser a
lexeme = Tok.lexeme lexer

quoted :: Parser a -> Parser a
quoted p = try (char '\'') *> p

identifier :: Parser T.Text
identifier = T.pack <$> (Tok.identifier lexer <|> specialIdentifier) <?> "identifier"
  where
  specialIdentifier :: Parser String
  specialIdentifier = lexeme $ try $
    string "-" <|> string "+" <|> string "..."

-- | The @Radix@ type consists of a base integer (e.g. @10@) and a parser for
-- digits in that base (e.g. @digit@).
type Radix = (Integer, Parser Char)

-- | Parse an integer, given a radix as output by @radix@.
-- Copied from Text.Parsec.Token
numberWithRadix :: Radix -> Parser Integer
numberWithRadix (base, baseDigit) = do
  digits <- many1 baseDigit
  let n = foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 digits
  seq n (return n)

decimal :: Parser Integer
decimal = Tok.decimal lexer

-- | Parse a sign, return either @id@ or @negate@ based on the sign parsed.
-- Copied from Text.Parsec.Token
sign :: Parser (Integer -> Integer)
sign = char '-' $> negate
   <|> char '+' $> id
   <|> return id

intRadix :: Radix -> Parser Integer
intRadix r = sign <*> numberWithRadix r

textLiteral :: Parser T.Text
textLiteral = T.pack <$> Tok.stringLiteral lexer

nil :: Parser ()
nil = try (char '\'' *> string "()") *> return () <?> "nil"

hashVal :: Parser LispVal
hashVal = lexeme $ char '#'
  *> (char 't' $> Bool True
  <|> char 'f' $> Bool False
  <|> char 'b' *> (Number <$> intRadix (2, oneOf "01"))
  <|> char 'o' *> (Number <$> intRadix (8, octDigit))
  <|> char 'd' *> (Number <$> intRadix (10, digit))
  <|> char 'x' *> (Number <$> intRadix (16, hexDigit))
  <|> oneOf "ei" *> fail "Unsupported: exactness"
  <|> char '(' *> fail "Unsupported: vector"
  <|> char '\\' *> fail "Unsupported: char")


lispVal :: Parser LispVal
lispVal = hashVal
  <|> Nil <$ nil
  <|> Number <$> try (sign <*> decimal)
  <|> Atom <$> identifier
  <|> String <$> textLiteral
  <|> _Quote <$> quoted lispVal
  <|> List <$> parens manyLispVal

manyLispVal :: Parser [LispVal]
manyLispVal = lispVal `sepBy` whitespace

_Quote :: LispVal -> LispVal
_Quote x = List [Atom "quote", x]

contents :: Parser a -> Parser a
contents p = whitespace *> lexeme p <* eof

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents lispVal) "<stdin>"

readExprFile :: SourceName -> T.Text -> Either ParseError LispVal
readExprFile = parse (contents (List <$> manyLispVal))
