{-# LANGUAGE FlexibleContexts #-}

module Parser (
  readExpr,
  readExprFile,
  parseNumber,
  parseSpecialAtom,
  parseExpr,
  parseList
) where

import LispVal

import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Data.Functor.Identity (Identity)
import qualified Data.Text as T

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef {
  Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = ";"
  , Tok.opStart = oneOf "'@#+"
  , Tok.opLetter = oneOf "'@#"
  , Tok.identStart = letter <|>  oneOf "!$%&*/:<=>?^_~"
  , Tok.identLetter = digit <|> letter <|> oneOf "!$%&*/:<=>?^_~+-.@"
  , Tok.reservedOpNames = ["'", "+"]
  }

m_parens = Tok.parens lexer
m_identifier = Tok.identifier lexer

parseAtom :: Parser LispVal
parseAtom = do
  p <- m_identifier
  return $ Atom $ T.pack p

parseSpecialAtom :: Parser LispVal
parseSpecialAtom = do
  p <- lexeme (string "-") <|> lexeme (string "+") <|> lexeme (string "...")
  return $ Atom $ T.pack p
    where lexeme = Tok.lexeme lexer

parseText :: Parser LispVal
parseText = String . T.pack <$> Tok.stringLiteral lexer

parseNumber :: Parser LispVal
parseNumber = do
  f <- sign
  n <- decimal
  return $ Number (f n)
    where
    sign = (char '-' >> return negate) <|> (char '+' >> return id) <|> return id
    symbol = Tok.symbol lexer
    decimal = Tok.decimal lexer
        

parseList :: Parser LispVal
parseList = List . concat <$> Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n')

parseSExp :: Parser LispVal
parseSExp = List . concat <$> m_parens (Text.Parsec.many parseExpr `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser LispVal
parseQuote = do
  Tok.reservedOp lexer "\'"
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseReserved
  <|> try parseNumber
  <|> parseSpecialAtom
  <|> parseAtom
  <|> parseText
  <|> parseQuote
  <|> parseSExp

parseReserved :: Parser LispVal
parseReserved =
      (Tok.reservedOp lexer "Nil" >> return Nil)
  <|> (Tok.reservedOp lexer "#t" >> return (Bool True))
  <|> (Tok.reservedOp lexer "#f" >> return (Bool False))

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>"

readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"

fileToEvalForm :: Either ParseError LispVal -> Either ParseError LispVal
fileToEvalForm (Right (List list)) = Right (List (Atom "begin" : list ))
fileToEvalForm x = x

parseFile :: T.Text -> Either ParseError LispVal
parseFile = fileToEvalForm . readExprFile
