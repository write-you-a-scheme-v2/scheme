{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T

import Data.Functor.Identity (Identity)


lett :: String
lett = "abcdefghijklmnopqrstuvwxyz"

num :: String
num = "1234567890"

lexer :: Tok.GenTokenParser String () Identity
lexer = Tok.makeTokenParser style 

style :: Tok.GenLanguageDef String () Identity
style = Lang.emptyDef { 
  Tok.commentStart = "{-"
  , Tok.commentEnd = "-}"
  , Tok.identStart = letter <|> oneOf "+-/*"
  , Tok.identLetter = alphaNum <|> oneOf "_'"
  , Tok.reservedOpNames = [ "'", "\""]
  , Tok.reservedNames = [ "true", "false"
    , "let", "quote", "lambda"
    , "Nil" ]
  }


{-
Tok.TokenParser { parens = m_parens
           , identifier = m_identifier -- Tok.Identifer lexer
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser style
-}

reservedOp :: String -> Parser ()
--reservedOp op = Tok.reservedOp lexer (T.unpack op) 
reservedOp op = Tok.reservedOp lexer op 

parseAtom :: Parser LispVal
parseAtom = Atom <$> Tok.identifier lexer

parseString :: Parser LispVal 
parseString = 
  do reservedOp "\""
     p <- Tok.identifier lexer
     reservedOp "\"" 
     return $ Str p 

parseNumber :: Parser LispVal 
parseNumber = fmap (Num . read) $ many1 digit
        
parseList :: Parser LispVal
parseList = List . concat <$>  (many parseExpr `sepBy` char ' ')

{-
parseSExp1 :: Parser LispVal
parseSExp1 = List . concat <$>  Tok.parens (many parseExpr `sepBy` char ' ')
-}

parseSExp :: Parser LispVal
parseSExp = 
  do reservedOp "("
     p <- (many parseExpr `sepBy` char ' ')
     reservedOp ")"
     return $ List . concat $ p 

parseQuote :: Parser LispVal
parseQuote = 
  do 
    reservedOp "\'" 
    x <- parseExpr
    return $ List [Atom "quote", x] 

parseExpr :: Parser LispVal 
parseExpr = parseAtom
      <|> parseString
      <|> parseNumber
      <|> parseReserved
      <|> parseQuote
      <|> parseSExp


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



readExpr :: String -> Either ParseError LispVal
readExpr = parse (contents parseExpr) "<stdin>" 


-------------------------
--  STAND ALONE TEST
--  --------------------

p pa inp = case parse pa "" inp of
             { Left err -> "err " ++ show err
             ; Right ans -> "ans " ++ show ans
             }


-- need a copy of LispVal for stand alone
data LispVal = Nil | Bin Bool | Atom String | Num Int | Str String | List [LispVal] deriving (Show)
main :: IO ()
main = 
  do 
    print $ p parseReserved "Nil"
    print $ p parseExpr  "Nil"
    print $ p parseExpr  "'Nil"
    print " "
    print $ p parseQuote  "'(1 2 3 4)"
    print $ p parseQuote  "'x"
    print $ p parseQuote  "'()"
    print " "
    print " "
    print $ p (parseExpr) "(1)"
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
    print " "
    print $ p parseExpr "(eq? 1 2)"
    print $ p parseExpr "1"
