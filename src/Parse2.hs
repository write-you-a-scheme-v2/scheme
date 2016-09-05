import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Applicative ((<$>))
data Expr = Var String | Con Bool | Uno Unop Expr | Duo Duop Expr Expr
    deriving Show
data Unop = Not deriving Show
data Duop = And | Iff deriving Show
data Stmt = Nop | String := Expr | If Expr Stmt Stmt | While Expr Stmt
          | Seq [Stmt]
    deriving Show

def = emptyDef { commentStart = "{-"
              , commentEnd = "-}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "~&=:"
              , opLetter = oneOf "~&=:"
              , reservedOpNames = ["~", "&", "=", ":=", "'", "\""]
              , reservedNames = ["true", "false", 
                               --  "let", "quote", "lambda", 
                                 "Nil",
                                 "if", "then", "else", "fi",
                                 "while", "do", "od"]
              }

TokenParser { parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def
data LispVal = Nil | Bin Bool | Atom String | Num Int | Str String | List [LispVal] deriving (Show)

pAtom :: Parser LispVal
pAtom = do p <- m_identifier
           return $ Atom p

pStr :: Parser LispVal 
pStr = do m_reservedOp "\""
          p <- m_identifier
          m_reservedOp "\"" 
          return $ Str p 

pNum :: Parser LispVal 
pNum = fmap (Num . read) $ many1 digit
        
--pSexp = m_parens $ pList

pSexp  = m_parens $ List  <$> many pExpr

pQuote = do 
  m_reservedOp "\'" 
  x <- pExpr
  return $ List [Atom "quote", x] 

pExpr = pAtom
      <|> pStr
      <|> pNum
      <|> pReserved
      <|> pQuote
      <|> ppSexp


pReserved = do 
  m_reserved "Nil" >> return Nil
  <|> (m_reserved "#t" >> return (Bin True))
  <|> (m_reserved "#f" >> return (Bin False))

exprparser :: Parser Expr
exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "~" >> return (Uno Not))]
        , [Infix (m_reservedOp "&" >> return (Duo And)) AssocLeft]
        , [Infix (m_reservedOp "=" >> return (Duo Iff)) AssocLeft]
        ]
term = m_parens exprparser
       <|> fmap Var m_identifier
       <|> (m_reserved "true" >> return (Con True))
       <|> (m_reserved "false" >> return (Con False))


mainparser :: Parser Stmt
mainparser = m_whiteSpace >> stmtparser <* eof
    where
      stmtparser :: Parser Stmt
      stmtparser = fmap Seq (m_semiSep1 stmt1)
      stmt1 = (m_reserved "nop" >> return Nop)
              <|> do { v <- m_identifier
                     ; m_reservedOp ":="
                     ; e <- exprparser
                     ; return (v := e)
                     }
              <|> do { m_reserved "if"
                     ; b <- exprparser
                     ; m_reserved "then"
                     ; p <- stmtparser
                     ; m_reserved "else"
                     ; q <- stmtparser
                     ; m_reserved "fi"
                     ; return (If b p q)
                     }
              <|> do { m_reserved "while"
                     ; b <- exprparser
                     ; m_reserved "do"
                     ; p <- stmtparser
                     ; m_reserved "od"
                     ; return (While b p)
                     }


play :: String -> String
play inp = case parse mainparser "" inp of
             { Left err -> "err " ++ show err
             ; Right ans -> "ans " ++ show ans
             }


p pa inp = case parse pa "" inp of
             { Left err -> "err " ++ show err
             ; Right ans -> "ans " ++ show ans
             }

ppList = List . concat <$>  (many pExpr `sepBy` char ' ')
ppSexp = List . concat <$>  m_parens (many pExpr `sepBy` char ' ')

main :: IO ()
main = 
  do 

    print $ p pReserved "Nil"
    print $ p pExpr  "Nil"
    print $ p pExpr  "'Nil"
    print " "
    print $ p pQuote  "'(1 2 3 4)"
    print $ p pQuote  "'x"
    print $ p pQuote  "'()"
    print " "
    print " "
    print $ p (pExpr) "(1)"
    print $ p ppList  "a \"a\" \"a\""
    print $ p ppList  "x 1 2"
    print $ p ppSexp  "(a \"a\" \"a\")"
    print $ p ppSexp  "(1 2 3 4)"
    print " "
    print " "
    print $ p (m_parens (many pExpr `sepBy` char ' ')) "(lambda (fnName a b c) (body) )"
    print $ p ppSexp  "(lambda (fnName a b c) (body) )"
    print $ p ppSexp  "(a 1 b 2)"
    print $ p ppSexp  "(let (a 1 b 2) (fn a b) )"
    print $ p ppSexp  "(let (a (x 1 2) b (y 3 4)) (fn a b) )"
    print " "
    print " "
    print $ p pExpr "x"
    print $ p pExpr "1"
    print $ p pExpr "\"a b c d\""
    print $ p pExpr "(3 1)"
    print " "
    print $ p pReserved  "#t"
    print $ p pReserved  "#f"
    print $ p pExpr "#t"
    print $ p pExpr "#f"
    print " "
    print $ p pExpr "(eq? 1 2)"
    print $ p pExpr "1"





