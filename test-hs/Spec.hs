{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import LispVal
import Parser
import Eval

import Test.Hspec


main :: IO ()
main = do
  hspec $ describe "parse" $ do
    it "Atom" $ do
      (readExpr "bb-8?") `shouldBe`  (Right $ Atom "bb-8?")
    it "Num Negative" $ do
      (readExpr "-2187") `shouldBe`  (Right $ Number $ 0 - 2187)
    it "Num Positive" $ do
      (readExpr "112233") `shouldBe`  (Right $ Number 112233)

    it "String" $ do
      (readExpr "\"General Liea Organa\"") `shouldBe`  (Right $ String "General Liea Organa")
    it "Bool True" $ do
      (readExpr "#t") `shouldBe`  (Right $ Bool True)
    it "Bool False" $ do
      (readExpr "#f") `shouldBe`  (Right $ Bool False)
    it "Nil" $ do
      (readExpr "Nil") `shouldBe`  (Right $ Nil)

    it "S-Expr: homogenous list" $ do
      (readExpr "(2 1 87)") `shouldBe`  (Right $ List [Number 2, Number 1,Number 87])
    it "S-Expr: homogenous list quoted" $ do
      (readExpr "'(2 1 87)") `shouldBe`  (Right $ List [Atom "quote",List [Number 2, Number 1,Number 87]])

    it "S-Expr: heterogenous list" $ do
      (readExpr "(stromTrooper \"Fn\" 2 1 87)") `shouldBe`  (Right $ List [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87])
    it "S-Expr: heterogenous list quoted" $ do
      (readExpr "'(stromTrooper \"Fn\" 2 1 87)") `shouldBe`  (Right $ List [Atom "quote", List [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87]])

    it "S-Expr: prim call: neg nums" $ do
      (readExpr "(- -42 -42)") `shouldBe` (Right $ List [Atom "-", Number (0 - 42), Number (0 - 42)])
    it "S-Expr: prim call: atoms" $ do
      (readExpr "(- you me)") `shouldBe` (Right $ List [Atom "-", Atom "you", Atom "me"])

    it "S-Expr: nested list" $ do
      (readExpr "(lambda (x x) (+ x x))") `shouldBe` (Right $ List [Atom "lambda", List[Atom "x", Atom "x"], List [Atom "+", Atom "x", Atom "x"]])
