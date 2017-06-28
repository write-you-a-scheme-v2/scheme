---
title: Testing our work
date: June 28, 2017
author: Adam Wespiser
---

------------
> *Testing shows the presence, not the absence of bugs* **Dijkstra**


## Testing w/ HSpec
By implementing a language like Scheme, we lose the safety of Haskell's type system and need an alternative guaranty of behavior.
This chapter is about writing tests to ensure our dynamically typed Scheme is behaving as we expect. 
Testing can be easily integrated into a Stack project, and Haskell's many frameworks satisfy a multitude of testing requirements. 
We will be looking at a few testing options, `HSpec`, and `Tasty.Golden`, for "golden", or file based tests. 

#### Testing Setup
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
This enables `stack test` and `stack build --test` to automatically run the tests found in  [test-hs/Spec.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test-hs/Spec.hs)
Running one of these commands will build the project, run the tests, and show the output.
Phew! All tests pass! (let me know if they don't)    

```
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



#### Haskell Testing Frameworks
Haskell has a few good testing frameworks, here are two of them we will use, and what they do:    
[Tasty](http://documentup.com/feuerbach/tasty)  Test framework that includes HSpec, Quickcheck, HUnit, and SmallChcek, as well as others.   
[HSpec](https://wiki.haskell.org/HUnit_1.0_User's_Guide). A Simple embedded DSL for unit testing.


##### Hspec
[HSpec](http://hspec.github.io/) is a straightfoward testing framework, and gives great mileage for its complexity.
HSpec is inspired by [RSpec](http://rspec.info/) a testing library for Ruby, and the two show remarkable similarity.
However, Hspec also includes the ability to include to test properties using [QuickCheck](http://hspec.github.io/quickcheck.html).
To describe tests, we denote a block of tests, then the testing expression along with the expected output.
```Haskell
main :: IO ()
main = do
  hspec $ describe "This is a block of tests" $ do
    it "Test 1" $
      textExpr input1 `shouldBe` "result of test 1"
    it "Test 2" $
      textExpr input2 `shouldBe` "result of test 1"
```   
`describe` gives a name to the block of tests, which is printed out when the tests are run.           
`it`  sets a specific test.    
`shouldBe`  states that a specific expression matches the


[lifted functions  for HSpec](http://hackage.haskell.org/package/hspec-expectations-lifted-0.8.2/docs/Test-Hspec-Expectations-Lifted.html)

#### Our Tests
 Two aspects of our Scheme will be tested in [test-hs/Spec.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test-hs/Spec.hs): The parser, and evaluation.
These two features lend themselves easily to testing, and together, cover ensure functionality meets expectations.  
Another view is that these tests allow us to modify the project without changing the features we worked so hard to implement, test driven development (tdd).
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

#### [Eval](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Parser.hs)  Tests
Alright, on to evaluation.
Our task here is ensuring there are no errors, bugs, or unspecified behavior in our Scheme...
If there were only a way to incorporate a system that protects us from invalid programs...
Type systems be damned! *We are all that is Scheme!*  
To test evaluation, we are going to either: read and parse from a file or inline text, then run with or without loading the standard library.
This way, we have flexibility over testing conditions, especially considering the standard library will be subject to the majority of iterative testing and revision efforts.
We will
** test / define evalargs fix  **  
** operators / special forms **

#### Let's Make Some Tests!
[home](home.html)...[back](08_stdlib.html)...[next](10_conclusion.html)
