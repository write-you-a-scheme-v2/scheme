---
title: Testing our work
header: Testing our work with HSpec and Golden
date: June 28, 2017
author: Adam Wespiser
---

------------
> *Testing shows the presence, not the absence of bugs* **Dijkstra**


## Testing w/ Haskell
Within dynamically typed language like Scheme, we lose the safety of Haskell's type system and need an alternative guaranty of behavior.
This chapter is about writing tests to ensure our Scheme is behaving as we expect.
Fortunately, testing can be easily integrated into a Stack project, and Haskell's many frameworks satisfy a multitude of testing requirements.
We will be looking at a few testing options, and implementing both `HSpec`, and `Tasty.Golden`.
There are two files for testing, [test-hs/Spec.hs](https://github.com/write-you-a-scheme-v2/scheme/blob/master/test-hs/Spec.hs) which contains the parser tests and golden file tests, and [test-hs/Golden.hs](https://github.com/write-you-a-scheme-v2/scheme/blob/master/test-hs/Golden.hs), that contains just the golden file tests.

#### Haskell Testing Frameworks
Haskell has a few good testing frameworks, here are a few of them, and what they do:    
[HUnit](https://wiki.haskell.org/HUnit_1.0_User's_Guide). A Simple embedded DSL for unit testing.    
[HSpec](http://hspec.github.io/) is a straightfoward testing framework, and gives great mileage for its complexity.
HSpec is inspired by [RSpec](http://rspec.info/) a testing library for Ruby, and the two show remarkable similarity.    
[QuickCheck](http://hspec.github.io/quickcheck.html). Tests properties of a program using randomly generated values.   
[Tasty](http://documentup.com/feuerbach/tasty)  Test framework that includes `HSpec`, `Quickcheck`, `HUnit`, and `SmallChcek`, as well as others, including `Golden`, which we will use for testing against a value within a file.   

#### Testing Setup within Stack
To setup testing, the following is added to `scheme.cabal`
```haskell

test-Suite test
  type: exitcode-stdio-1.0
  main-is: Spec.hs
  hs-source-dirs: test-hs
  default-language: Haskell2010
  build-depends:
    base         >= 4.8 && < 5.0,
    text         >= 1.2 && <1.3,
    hspec        >= 2.2 && < 2.3,
```
This enables `stack test` and `stack build --test` to automatically run the `HSpec` tests found in  [test-hs/Spec.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test-hs/Spec.hs)
Running one of these commands will build the project, run the tests, and show the output.
Phew! All tests pass! (let me know if they don't)    

```haskell
test-Suite test-golden
  type: exitcode-stdio-1.0
  main-is: Golden.hs
  hs-source-dirs: test-hs
  default-language: Haskell2010
  build-depends:
    base         >= 4.8 && < 5.0,
    text         >= 1.2 && <1.3,
    tasty        >= 0.11 && <0.12,
    tasty-golden >= 2.3 && <2.5,
    bytestring   >= 0.10.8 && <0.11,
    scheme == 0.1
```
Now we can run `stack test --test-golden` to run the tests from [test-hs/Golden.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test-hs/Golden.hs)
Running `stack test` will perform both test suites.
Let's look further into how testing is done, and the libraries used.    


#### HSpec Setup
`HSpec`'s strength is its simplicity, and ability to compare the results of arbitrary functions against a known output.
We will use it to test internal components of our Scheme, particularly the parser, which contains a depth of logic orthogonal to the rest of the code base.    
 First, let's take a look at the general form of an `HSpec` test.    

```haskell
main :: IO ()
main = do
  hspec $ describe "This is a block of tests" $ do
    it "Test 1" $
      textExpr input1 `shouldBe` "result of test 1"
    it "Test 2" $
      textExpr input2 `shouldBe` "result of test 1"
```

 * `describe` gives a name to the block of tests, which is printed out when the tests are run.           
 * `it`  sets a specific test.    
 * `shouldBe`  states that a specific expression matches the test expression.    


#### HSpec Tests
 Two internal aspects of our Scheme will be tested in [test-hs/Spec.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test-hs/Spec.hs): The parser, and evaluation.
These two features lend themselves easily to testing, and together, cover ensure functionality meets expectations.  
Another view is that these tests allow us to modify the project without changing the features we worked so hard to implement, test driven development (TDD).
The `./test` folder in our project contains the Scheme expressions run during the tests.
Besides files containing expressions, we can also specify expressions as `T.Text`, and `define` blocks without loading the standard library.
All of the parsing logic, and evaluation of simple expressions, special forms, and features like lexical scope are included in the scheme expressions found in the test folder.

#### [Parser](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Parser.hs) Tests
The first set of tests ensures text is properly parsed into `LispVal` using `readExpr`.
To organize this set of tests, the `hspec` function is used, along with `describe` to give the set of tests an suitable description.
Many constructions of `LispVal` are tested, and here were divide that list into S-Expression and non-S-Expression values for simpler testing.    

```Haskell
hspec $ describe "src/Parser.hs" $ do
  it "Atom" $
    readExpr "bb-8?" `shouldBe` (Right $ Atom "bb-8?")

  it "S-Expr: heterogenous list" $
    readExpr "(stromTrooper \"Fn\" 2 1 87)" `shouldBe`
      (Right $ List [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87])
```

#### [Eval](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Eval.hs)  Tests
Alright, on to evaluation.
Our task here is ensuring there are no errors, bugs, or unspecified behavior in our Scheme...
If there were only a way to incorporate a system that protects us from invalid programs...
Type systems be damned! *We are all that is Scheme!*  
To test evaluation, we are going to either: read and parse from a file or inline text, then run with or without loading the standard library.
This way, we have flexibility over testing conditions, especially considering the standard library will be subject to the majority of iterative testing and revision efforts.

```Haskell
hspec $ describe "src/Eval.hs" $ do
   wStd "test/add.scm"              $ Number 3
   wStd "test/if_alt.scm"           $ Number 2
   runExpr Nothing "test/define.scm"        $ Number 4
   runExpr Nothing "test/define_order.scm"  $ Number 42
```
This is all fine, but requires a lot of helper functions to work, specifically the following:

```Haskell
wStd :: T.Text -> LispVal -> SpecWith ()
wStd = runExpr (Just "test/stdlib_mod.scm")

-- run expr w/o stdLib
tExpr :: T.Text -> T.Text -> LispVal -> SpecWith ()
tExpr note expr val =
    it (T.unpack note) $ evalVal `shouldBe` val
    where evalVal = (unsafePerformIO $ runASTinEnv basicEnv $ fileToEvalForm "" expr)


runExpr :: Maybe T.Text -> T.Text -> LispVal -> SpecWith ()
runExpr  std file val =
    it (T.unpack file) $ evalVal  `shouldBe` val
    where evalVal = unsafePerformIO $ evalTextTest std file

evalTextTest :: Maybe T.Text -> T.Text -> IO LispVal --REPL
evalTextTest (Just stdlib) file= do
  stdlib <- getFileContents $ T.unpack  stdlib
  f      <- getFileContents $ T.unpack file
  runASTinEnv basicEnv $ textToEvalForm stdlib  f

evalTextTest Nothing file = do
  f <- getFileContents $ T.unpack file
  runASTinEnv basicEnv $ fileToEvalForm (T.unpack file) f
```

What's troublesome here is the use of `unsafePerformIO` to read file contents and shed the `IO` monad.
Stepping back, we are coding a test within a specific file, evaluating it with or without the standard library, then comparing it to a value compiled into the testing file.
If we can admit `HSpec` is good at testing internals like the parser, its also fair to say its not great at this process of "golden tests" for our Scheme language.
Fortunately there is a better way that allows us to run a test Scheme file and compare the result against a 'golden' value in a stored file!

#### Tasty Golden Tests
The package [Tasty.Golden](https://hackage.haskell.org/package/tasty-golden-2.3.1.1/docs/Test-Tasty-Golden.html) gives us a function:

```Haskell
goldenVsString :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO LBS.ByteString -- ^ action that returns a string
  -> TestTree -- ^ the test verifies that the returned string is the same as the golden file contents
```

This allows us to run the tests located in [./test](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test) and compare the results to the likewise named files in [./test/ans](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test/ans).    

Looking in [test-hs/Golden.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test-hs/Golden.hs), we can see a drastic simplification compared to `HSpec`!

```Haskell
import Test.Tasty
import Test.Tasty.Golden
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Golden Tests"
  [   tastyGoldenRun "add"           "test/add.scm"              "test/ans/add.txt"
    , tastyGoldenRun "if/then"       "test/if_alt.scm"           "test/ans/if_alt.txt"
    , tastyGoldenRun "let"           "test/let.scm"              "test/ans/let.txt"
    ...
  ]

tastyGoldenRun :: TestName -> T.Text -> FilePath -> TestTree
tastyGoldenRun testName testFile correct = goldenVsString testName correct  (evalTextTest (Just "lib/stdlib.scm") (testFile) >>= (return . C.pack .  show))

```

Where the `evalTextTest` function from `HSpec` is used again.


#### Let's Wrap Things Up !
[home](home.html)...[back](08_stdlib.html)...[next](10_conclusion.html)
