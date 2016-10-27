{-# LANGUAGE OverloadedStrings #-}

module Parser (
  readExpr,
) where

import LispVal

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T

import Data.Functor.Identity (Identity)


lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = Tok.opLetter style <|> oneOf "_'"
  , Tok.opStart         = Tok.opLetter style
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = []
  , Tok.reservedNames   = []
  , Tok.caseSensitive   = True
  }


-- used for '(', ')', '.'
reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op)

-- Atom
symbol :: Parser T.Text
symbol = T.pack <$> Tok.identifier lexer

strLit :: Parser T.Text
strLit = T.pack <$> Tok.stringLiteral lexer

-- whole file
contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

-- "adam"
parseString :: Parser LispVal
parseString = do
  x <- strLit
  return $ String x

-- square
parseAtom :: Parser LispVal
parseAtom = do
  atom <- symbol
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    "define"-> Atom $ T.pack "define"
    "lambda"-> Atom $ T.pack "lambda"
    "let" -> Atom $ T.pack "let"
    _    -> Atom atom

-- list or dotted List
-- ( ... contents ...)
parseSexp :: Parser LispVal
parseSexp = do
  reservedOp "("
  x <- try parseList <|> parseDottedList
  reservedOp ")"
  return x

-- {expr}(1..n)
parseList :: Parser LispVal
parseList = List <$> many parseExpr

-- '(+ 1 2) => List [Atom quote, Atom "+", Number 1, Number 2)
parseQuoted :: Parser LispVal
parseQuoted = do
  reservedOp "'"
  x <- parseExpr
  return $ List [Atom "quote", x]

-- toss me
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- reservedOp "." >> spaces >> parseExpr
  return $ DottedList head tail

-- int
parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseSexp

-- parseExpr
--   => {Atom, String, Num, Quote, Sexp)
-- sexp
--   => {(list),(dottedLisp)}
-- list
--   => {parseExpr}(many)

readExpr :: T.Text -> Either ParseError LispVal
readExpr input = parse (contents parseExpr) "<stdin>" input
