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
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = Tok.opLetter style
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedOpNames = []
  , Tok.reservedNames   = []
  , Tok.caseSensitive   = True
  }


reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op)

symbol :: Parser T.Text
symbol = T.pack <$> Tok.identifier lexer

strLit :: Parser T.Text
strLit = T.pack <$> Tok.stringLiteral lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseString :: Parser LispVal
parseString = do
  x <- strLit
  return $ String x
{-
parseString = do
  reservedOp "\""
  x <- strLit
  reservedOp "\""
  return $ String x
-}
parseAtom :: Parser LispVal
parseAtom = do
  atom <- symbol
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseSexp :: Parser LispVal
parseSexp = do
  reservedOp "("
  x <- try parseList <|> parseDottedList
  reservedOp ")"
  return x

parseList :: Parser LispVal
parseList = List <$> many parseExpr

parseQuoted :: Parser LispVal
parseQuoted = do
  reservedOp "'"
  x <- parseExpr
  return $ List [Atom "quote", x]

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- reservedOp "." >> spaces >> parseExpr
  return $ DottedList head tail

parseNumber :: Parser LispVal
parseNumber = fmap (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> parseSexp

readExpr :: T.Text -> Either ParseError LispVal
readExpr input = parse (contents parseExpr) "<stdin>" input
