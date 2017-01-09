---
title: Towards a Standard Library: Fold and Unfold
date: November 28, 2016
author: Adam Wespiser
---

- [ ] Fix up 03_eval -- should we include monadic evaluation?
- [ ] Correct src/Eval to include proper functions: run file w/ std lib, run file w/o std lib,
run expr w/ std lib, run expression w/o standard lib
- [ ] Clean up helper functions in test-hs/Spec.hs
- [ ] Finish tests for all of std lib
- [ ] File dependency of `test/stdlib_mod.scm`
- [ ] Origin of `test/stdlib_mod.scm`




----
> *Cry 'Havoc!', and let slip the dogs of war* **William Shakespeare**

## Standard Library
Good programming languages will have a collection of functions that provide a basic functionality for the language.
Defining these functions can happen in three places: as special forms like `let`, in the primitive environment like `+`, or as an external file written in the programming language that is automatically loaded before code is run.
The standard library is the third of these choices.
It defines functions in Scheme using `define` and `lambda` using the special forms to control evaluation and fallback to functions in the primitive environment to ultimately control dynamic dispatch on overloaded calls.

#### Our Standard Library
We define the standard library, `test/stdlib_mod.scm` with a multitude of functions, including recursive functions like `foldl`.
To evaluate a function with the standard library we must first parse the std. lib. into list of `LispVal`s, append our expressions to that list, and finally wrap within a `begin` block which is subsequently evaluated.

#### Scheme Code
`test/stdlib_mod.scm` show definitions car/cadr special case.    
recursive function examples

#### Running Standard Library
[`src/Eval.hs`]() examples that show File -> LispVal -> LispVal + Stdlib -> Eval LispVal

#### Why we take this approach
`e -> a` functionality of `ReaderT` monad.  
Alternative approach w/ `StateT` that can provide
