---
title: Towards a Stan
date: November 28, 2016
author: Adam Wespiser
---

----
> *Testing shows the presence, not the absence of bugs **Dijkstra**

#### WIP :: TODO
- [ ] try move to tasty?
- [ ] add survey of Haskell testing frameworks
- [ ] talk about unsafePerformIO / use Test.HSpec.Lifted 
- [ ] write tests for Exceptions for LispException 
- [ ] include tests that what use on quickcheck
- [ ] walk through code, give explanation
- [ ] think about reorganizing code to show automatic spec finding?


## Testing w/ HSpec
By implementing a language like Scheme, we lose the safety of Haskell's type system and need an alternative gaurenty of behaviour.
This chapter is not only about writing tests, ensuring our dynamically typed Scheme is behaving like we want.  
Testing is easily integrated into a Stack project, and Haskell's many frameworks satisfy a multitude of testing requirements.  

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
Phew! All tests pass! 
Let's look further into how testing is done, and the libraries used.

####Haskell Testing Frameworks
Haskell has a few good testing frameworks
Here's a few differnt ones and what they do:    
Quickcheck.    
[Tasty](http://documentup.com/feuerbach/tasty)  Test framework that includes HSpec, Quickcheck,HUnit, and SmallChcek, as well as others.   
[HSpec](https://wiki.haskell.org/HUnit_1.0_User's_Guide). 


#####Hspec
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
Two aspects of our Scheme will be tested: The parser, and evaluation.  
These two features lend themselves easily to testing, and together, cover most of the functionality needed to ensure the project is doing what we think it is.  
Another view is that these tests allow us to modify the project without changing the features we worked so hard to implement, test driven development (tdd).   
The `./test` folder in our project contains the Scheme expressions run during the tests.   
Besides files containing expressions, we can also specify expressions as `T.Text`, and `define` blocks without loading the standard library.  
All of the parsing logic, and evaluation of simple expressions, special forms, and featurs like lexical scope are included in the scheme expressions found in the test folder.  










