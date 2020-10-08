{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import LispVal
import Eval

import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.Golden
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor ((<&>))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Golden Tests"
  [   tastyGoldenRun "add"           "test/add.scm"              "test/ans/add.txt"
    , tastyGoldenRun "if/then"       "test/if_alt.scm"           "test/ans/if_alt.txt"
    , tastyGoldenRun "let"           "test/let.scm"              "test/ans/let.txt"
    , tastyGoldenRun "lambda let"    "test/test_let.scm"         "test/ans/test_let.txt"
    , tastyGoldenRun "eval bool"     "test/eval_boolean.scm"     "test/ans/eval_boolean.txt"
    , tastyGoldenRun "eval bool ops" "test/eval_boolean_ops.scm" "test/ans/eval_boolean_ops.txt"
    , tastyGoldenRun "eval lambda"    "test/eval_lambda.scm"     "test/ans/eval_lambda.txt"
    , tastyGoldenRun "quote"          "test/test_quote.scm"      "test/ans/test_quote.txt"
    , tastyGoldenRun "car"            "test/test_car.scm"        "test/ans/test_car.txt"
    , tastyGoldenRun "cdr"            "test/test_cdr.scm"        "test/ans/test_cdr.txt"
    , tastyGoldenRun "cadadr"         "test/test_cadadr.scm"     "test/ans/test_cadadr.txt"
    , tastyGoldenRun "greater than"   "test/test_gt.scm"         "test/ans/test_gt.txt"
    , tastyGoldenRun "lexical scope 1" "test/test_scope1.scm"    "test/ans/test_scope1.txt"
    , tastyGoldenRun "lexical scope 2" "test/test_scope2.scm"    "test/ans/test_scope2.txt"
    , tastyGoldenFail "issue #23"      "test/test_scope3.scm"    "test/ans/test_scope3.txt"
    , tastyGoldenRun "issue #25"       "test/test_scope4.scm"    "test/ans/test_scope4.txt"
    , tastyGoldenRun "Recursion 1"    "test/test_fix.scm"        "test/ans/test_fix.txt"
    , tastyGoldenRun "Recursion 2"    "test/test_fix2.scm"       "test/ans/test_fix2.txt"
    , tastyGoldenRun "Mutual Recursion 2" "test/test_mutual_rec.scm" "test/ans/test_mutual_rec.txt"
    , tastyGoldenRun "fn args"         "test/test_args.scm"      "test/ans/test_args.txt"
    , tastyGoldenRun "fold"            "test/test_fold.scm"      "test/ans/test_fold.txt"
  ]

tastyGoldenRun :: TestName -> T.Text -> FilePath -> TestTree
tastyGoldenRun testName testFile correct =
  goldenVsString testName correct $
    evalTextTest "lib/stdlib.scm" testFile <&> C.pack . show

tastyGoldenFail :: TestName -> T.Text -> FilePath -> TestTree
tastyGoldenFail testName testFile correct =
  goldenVsString testName correct $
    (safeExec $ evalTextTest "lib/stdlib.scm" testFile) <&>
      either C.pack (C.pack . show)

evalTextTest :: T.Text -> T.Text -> IO LispVal --REPL
evalTextTest stdlib file= do
  stdlib' <- getFileContents $ T.unpack stdlib
  f <- getFileContents $ T.unpack file
  runASTinEnv basicEnv $ textToEvalForm stdlib'  f
