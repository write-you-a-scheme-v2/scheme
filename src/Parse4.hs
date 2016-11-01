{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T

import Data.Functor.Identity (Identity)


lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style 

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef { 
  Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.commentLine = "--"
  , Tok.opStart = Tok.opLetter style
  , Tok.opLetter = oneOf ":!#$%%&*+./<=>?@\\^|-~"
  , Tok.identStart = letter <|>  oneOf "+-/*"
  , Tok.identLetter = letter <|> oneOf "?"
  , Tok.reservedOpNames = [ "'", "\""]
  --, Tok.reservedNames = [ "true", "false", "let", "quote", "lambda", "Nil" ]
  }

Tok.TokenParser { Tok.parens = m_parens
           , Tok.identifier = m_identifier -- Tok.Identifer lexer
           , Tok.reservedOp = m_reservedOp
           , Tok.reserved = m_reserved
           , Tok.semiSep1 = m_semiSep1
           , Tok.whiteSpace = m_whiteSpace } = Tok.makeTokenParser style


reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op) 

parseAtom :: Parser LispVal
parseAtom = do p <- m_identifier 
               return $ Atom $ T.pack p

parseText :: Parser LispVal 
parseText = 
  do reservedOp "\""
     --p <- (m_identifier <|> many1 (noneOf "\""))
     p <- many1 (noneOf "\"")
     reservedOp "\"" 
     return $ (Str . T.pack)  p 

parseNumber :: Parser LispVal 
parseNumber = fmap (Num . read) $ many1 digit
        
parseList :: Parser LispVal 
parseList = List . concat <$> (many parseExpr `sepBy` char ' ')

parseSExp :: Parser LispVal 
parseSExp = List . concat <$> m_parens (many parseExpr `sepBy` char ' ')

parseQuote :: Parser LispVal
parseQuote = 
  do 
    reservedOp "\'" 
    x <- parseExpr
    return $ List [Atom "quote", x] 

parseExpr :: Parser LispVal 
parseExpr = parseReserved
      <|> parseAtom
      <|> parseText
      <|> parseNumber
      <|> parseQuote
      <|> parseSExp

parseReserved :: Parser LispVal 
parseReserved = 
  do 
    reservedOp "Nil" >> return Nil
    <|> (reservedOp "#t" >> return (Bin True))
    <|> (reservedOp "#f" >> return (Bin False))


contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof 
  return r



readExpr :: T.Text -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>" 


-------------------------
--  STAND ALONE TEST
--  --------------------

p pa inp = case parse pa "" inp of
             { Left err -> "err " ++ show err
             ; Right ans -> "ans " ++ show ans
             }


-- need a copy of LispVal for stand alone
data LispVal = Nil | Bin Bool | Atom T.Text | Num Int | Str T.Text | List [LispVal] deriving (Show)
main :: IO ()
main = 
  do 
    print "hello" 
    print $ p parseReserved "Nil"
    print $ p parseExpr  "#t"
    print $ p parseExpr  "#f"
    --print $ p parseExpr  "'Nil"
    print " "
    print $ p parseQuote  "'(1 2 3 4)"
    print $ p parseQuote  "'x"
    print $ p parseQuote  "'()"
    print " "
    print " "
    print $ p parseExpr "(1)"
    print $ p parseList  "a \"a\" \"a\""
    print $ p parseList  "x 1 2"
    print $ p parseSExp  "(a \"a\" \"a\")"
    print $ p parseSExp  "(1 2 3 4)"
    print " "
    print " "
    --print $ p (m_parens (many parseExpr `sepBy` char ' ')) "(lambda (fnName a b c) (body) )"
    print $ p parseSExp  "(lambda (fnName a b c) (body) )"
    print $ p parseSExp  "(a 1 b 2)"
    print $ p parseSExp  "(let (a 1 b 2) (fn a b) )"
    print $ p parseSExp  "(let (a (x 1 2) b (y 3 4)) (fn a b) )"
    print " "
    print " "
    print $ p parseExpr "x"
    print $ p parseExpr "1"
    print $ p parseExpr "\"a b c d\""
    print $ p parseExpr "(3 1)"
    print " "
    print $ p parseReserved  "#t"
    print $ p parseReserved  "#f"
    print $ p parseExpr "#t"
    print $ p parseExpr "#f"
    print $ p parseExpr "(eq? 1 2)"
    print $ p parseExpr "1"
    print " "
    print $ p parseExpr "(+ 1 2)"
    print $ p parseExpr "(- 1 2)"
    print $ p parseExpr "(* 1 2)"
    print $ p parseExpr "(/ 1 2)"
    print " "
