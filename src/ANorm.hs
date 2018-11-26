{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ANorm (
  toAnf
  , ExprReduced(..)
  , anf
  , imm
  , immExpr
  , imm2
  , deSugar
  ) where

import Control.Monad.State (State, evalState, get, put)
import qualified Data.Text as T 
import qualified Data.Set as Set
import LispVal

-- toExprA :: Expr -> ExprA
-- https://ucsd-progsys.github.io/liquidhaskell-blog/2016/09/01/normal-forms.lhs/

-- We need to get our Expr, from src/LispVal.hs
-- into this ExprA. There are a number of considerations
-- 0) we will convert through the raw AST, just symbols, keywords, 
--    and lists
-- 1) {Lam,Fun} Data constructors will not be used, instead we'll have
--    to detect keywords 
-- 2) Expr.List -> EApp
-- 3) Something needs to be done about Nil
--     ? empty list ? special char ? 
--     ? define, begin


-- This will just get us started, but we need to figure out 
-- how to map these, along with other special Binops to LLVM
binOps :: Set.Set T.Text
binOps = Set.fromList $ ["+", "-", "*", "<", "<=", ">", ">=", "=="]
{- 
 - deSugar needs to do a few things
 - 0. Pass through Literal Values changed
 - 1. recognize and map binops from Atoms
 - 2. unwrap let/lambdas from a multiple binding, to a 
 -    single binding form
 -    (let (x 1 y 2) (body) =>
 -       (let (x 1) (let (y 2) (body)))
-}

isLambda :: LispVal -> Bool
isLambda ex@(List ((Atom "lambda"):xs)) = True
isLambda _ = False

deSugar :: LispVal -> ExprReduced
deSugar (Atom x) = EVar x
deSugar (Nil) = EVar "Nil"
deSugar (String s) = EVar "String"
deSugar (Fun _) = EVar "Fun"
deSugar (Lambda _ _) = EVar "Lambda"
deSugar (Number i) = ENumber i
deSugar (Bool b)  = EBool b
deSugar (List [Atom "let", List ((Atom var):binding:rest), body])
  | null rest = ELet var (deSugar binding) $ deSugar body
  | otherwise = ELet var (deSugar binding) $ deSugar (List [Atom "let", List rest, body]) 
deSugar (List [Atom "lambda", List ((Atom x):rest), body]) 
  | null rest = ELam x $ deSugar body
  | otherwise = ELam x $ deSugar $ List [Atom "lambda", List rest, body]
deSugar (List [Atom "if", pred, eTrue, eFalse]) = 
 let dTrue  = deSugar eTrue
     dFalse = deSugar eFalse
     dPred  = deSugar  pred
 in  EIf dPred dTrue dFalse
deSugar args@(List [Atom x, e1, e2]) 
  | Set.member x binOps = EBin x (deSugar e1) (deSugar e2)
  | otherwise = deSugar args
deSugar args@(List list@(x:xs)) 
 | isLambda x = foldl1 EApp (fmap deSugar list)
 | otherwise = EVar "misapp"
--ldeSugar (List []) = EVar "Nil"
deSugar _ = EVar "__fallthru__"

{- 
 - This is basically our 'Core' Lang
-}

data ExprReduced
  = EVar T.Text
  | ENumber Integer
  | EBool Bool
  | EBin T.Text ExprReduced ExprReduced -- T.Text -> Binary ops
  | EApp ExprReduced ExprReduced
  | ELet T.Text ExprReduced ExprReduced
  | ELam T.Text ExprReduced
  | EIf ExprReduced ExprReduced ExprReduced
  deriving (Show)
  -- https://www.microsoft.com/en-us/research/wp-content/uploads/2007/10/compilingwithcontinuationscontinued.pdf


type ImmExpr = ExprReduced -- This is our A Normal Form
type AnfExpr = ExprReduced

isImm :: ExprReduced -> Bool
isImm (EVar _) = True
isImm (ENumber _) = True
isImm _ = False

isAnf :: ExprReduced -> Bool 
isAnf (EVar _) = True
isAnf (ENumber _) = True
isAnf (EBool _) = True
isAnf (EBin _ e1 e2) = (isAnf e1) && (isAnf e2)
isAnf (EApp e1 e2) = (isAnf e1) && (isAnf e2)
isAnf (ELet _ var body)  = isAnf body && isAnf var
isAnf (ELam _ body) = isAnf body
isAnf (EIf pred e1 e2) = isAnf e1 && isAnf e2

type AnfM a = State Int a

fresh :: AnfM T.Text
fresh = do
  n <- get
  put (n+1)
  return $ T.concat ["anf", T.pack $ show n]
-- https://www.cs.uoregon.edu/Reports/AREA-201512-Maurer.pdf
anf :: ExprReduced -> AnfM AnfExpr
anf (EVar text) = return $ EVar text
anf (ENumber integer) = return $ ENumber integer
anf (EBool bool) = return $ EBool bool
anf (EBin var e1 e2) = do 
    (b1s, v1) <- imm e1
    (b2s, v2) <- imm e2
    return $ makeLets (b1s ++ b2s) $ EBin var v1 v2
anf (EApp e1 e2) = do
    (b1s, v1) <- imm e1
    (b2s, v2) <- imm e2
    return $ makeLets (b1s ++ b2s) $ EApp v1 v2
anf (ELet var e1 e2) = do
    (b1s, v1) <- imm e1
    (b2s, v2) <- imm e2
    return $ makeLets (b1s ++ b2s) $ ELet var v1 v2
anf (ELam var e1) = do 
    a <- anf e1
    return $ ELam var a
anf (EIf pred e1 e2) = do
    -- http://cristal.inria.fr/~fpottier/freshml/
    (b1s, v1) <- immExpr e1
    (b2s, v2) <- immExpr e2
    return $ EIf pred (makeLets b1s e1) (makeLets b2s e2)


makeLets :: [(T.Text, AnfExpr)] -> AnfExpr -> AnfExpr
makeLets [] e = e
makeLets ((x,e):bs) e' = ELet x e $ makeLets bs e'


imm :: ExprReduced -> AnfM ([(T.Text, AnfExpr)], ImmExpr)
imm (EVar text) = return ([], EVar text)
imm (EBool bool) = return ([], EBool bool)
imm (ENumber integer) = return ([], ENumber integer)
imm e@(ELet {}) = immExpr e
imm e@(ELam {}) = immExpr e
imm (EBin op e1 e2) = imm2 e1 e2 (EBin op)
imm (EApp e1 e2) = imm2 e1 e2 EApp
imm (EIf pred e1 e2) = imm2 e1 e2 (EIf pred)


immExpr :: ExprReduced -> AnfM ([(T.Text, AnfExpr)], ImmExpr)
immExpr e = do
  a <- anf e
  t <- fresh
  return ([(t, a)], EVar t)


imm2 :: ExprReduced -> ExprReduced -> 
        (ImmExpr -> ImmExpr -> ImmExpr) -> 
        AnfM ([(T.Text, AnfExpr)] , ImmExpr)
imm2 e1 e2 f = do 
  (bs1, v1) <- imm e1
  (bs2, v2) <- imm e2
  t         <- fresh
  let bs = bs1 ++ bs2 ++ [(t, f v1 v2)] -- fun app -> can we toss this?
  return (bs, EVar t)


toAnf :: ExprReduced -> AnfExpr
toAnf e = evalState (anf e) 0
