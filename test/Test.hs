{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)

import Parser
import Eval

suite :: TestTree
suite = testGroup "Test Suite" [

    testGroup "Parser Tests"
      [ testCase "Nil"   $ parseTest "Nil"
      , testCase "False" $ parseTest "#f"
      , testCase "True"  $ parseTest "#t"
      , testCase "Quote"  $ parseTest "'(1 2 3 4)"
      , testCase "Var"  $ parseTest "x"
      , testCase "Num"  $ parseTest "1"
      , testCase "Let"  $ parseTest "(let (a 1 b 2) (fn a b) )"
      ],

    testGroup "Eval Tests"
      [
      ]

  ]


parseTest :: Text -> Assertion
parseTest s = case readExpr s of
  Left err -> assert False
  Right _  -> assert True

main :: IO ()
main = defaultMain suite

