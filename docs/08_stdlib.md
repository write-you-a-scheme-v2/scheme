---
title: Towards a Standard Library
header: Towards a Standard Library
date: November 28, 2016
author: Adam Wespiser
---

----
> *Cry 'Havoc!', and let slip the dogs of war*  **William Shakespeare**

## Standard Library
We define Scheme functions in three places: as special forms like `let`, in the primitive environment like the `+` operator, or within the body of an external Scheme program.
The standard library is the third of these choices.
Its functions are defined using building blocks of both primitive and special forms.
If a function does not need a special evaluation strategy, or mapping to an internal operator, it belongs in the library.
Successful programming languages will have a collection of functions that provide extensive capability and empower users to complete their tasks.
When building a language, the standard library is often the first real "program" written in that language.
When we write the library, we are not only providing the user with convenient shortcuts, but proving many features of the language work!


#### Our Standard Library
We build the standard library, [`test/stdlib_mod.scm`](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test/stdlib_mod.scm) with a multitude of functions. 
Importantly, we allow recursive functions, like `fold` and `reduce` which enable efficient functional programming.

#### Composing `car` and `cdr` into `c({ad}^n)r`
Our standard library defines functions that are combinations of `car` and `cdr`, for example `cdar` is `define cdar (lambda (pair) (cdr (car pair))))`.
 We can see these examples in the first defines of the standard library.
For this to work, `car` and `cdr` must accept both quoted and unquoted lists.
In other words, to chain these functions, they must accept S-Expressions without evaluating the arguments.
This makes these functions specials forms.     
In [`Eval.hs`](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Eval.hs) we add the special forms:       
```haskell
eval all@(List [Atom "cdr", List [Atom "quote", List (x:xs)]]) =
  return $  List xs
eval all@(List [Atom "cdr", arg@(List (x:xs))]) =  
  case x of
      Atom  _ -> do val <- eval arg
                    eval $ List [Atom "cdr", val]
      _           -> return $ List xs

eval all@(List [Atom "car", List [Atom "quote", List (x:xs)]]) =
  return $  x
eval all@(List [Atom "car", arg@(List (x:xs))]) =  
  case x of
      Atom _       -> do val <- eval arg
                         eval $ List [Atom "car", val]
      _            -> return $ x
``` 
This feels like a hack, and if the `cadr` family of functions wasn't so essential, we could drop `car` and `cdr` as special forms.    

#### Running Standard Library 
[`Eval.hs`](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Eval.hs) contains the functions that run text within the context of the standard library.
Because the reader monad does not return the input context, we must wrap the expression and the standard library together as `LispVals`, then evaluate.
This is done with the `endOfList` function. 

Define the standard library file. 
```haskell
sTDLIB :: T.Text
sTDLIB = "lib/stdlib.scm" 
```

Parse the input function as an S-Expression, then the  library file as a list of S-Expressions. Append the parsed input expression to the end of the  list.
```Haskell
endOfList :: LispVal -> LispVal -> LispVal
endOfList (List x) expr = List $ x ++ [expr]
endOfList n _  = throw $ TypeMismatch  "failure to get variable: " n


parseWithLib :: T.Text -> T.Text -> Either ParseError LispVal
parseWithLib std inp = do
  stdlib <- readExprFile std
  expr   <- readExpr inp
  return $ endOfList stdlib expr

getFileContents :: FilePath -> IO T.Text
getFileContents fname = do
  exists <- doesFileExist fname
  if exists then TIO.readFile  fname else return "File does not exist."
```

Final monadic evaluation of both standard library and expression.
The key here is the `evalBody`, which accepts an S-Expression consisting of a series of `define` statements to be sequentially evaluated.
This is the same evaluation strategy used in both `let` and `lambda` expressions. 

```Haskell
textToEvalForm :: T.Text -> T.Text -> Eval LispVal
textToEvalForm std input = either (throw . PError . show )  evalBody $ parseWithLib std input

evalText :: T.Text -> IO () --REPL
evalText textExpr = do
  stdlib <- getFileContents $ T.unpack  sTDLIB
  res <- runASTinEnv basicEnv $ textToEvalForm stdlib textExpr
```

## Conclusion
The standard library is the third and final place we define capability in our Scheme, after special forms and the primitive environment.
The idea behind the standard library is that we have a relatively easy to write collection of utility and helper functions needed to get things done. 
If you look into the library, functions like `reduce`, `fold` and the `cadr` family are pretty useful. 

However the `e -> a` functionality of `ReaderT` monadic action limits our approach.
We cannot evaluate the file containing the library and get the modified environment back again.
This somewhat complicates things, and requires us to take our approach via syntax manipulation.
Although its not ideal, its also not very complex and lets us keep the simplistic lexical scoping via reader monad function `local` we established earlier.
Alternatively, we can approach evaluation with `StateT` to run the monad, and get a modified `state`.

#### [ Understanding Check ]
Add a new standard library function to generate the first 'n' Fibonacci numberers using a recursive function.     
Define a pair of co-recursive functions, `even-co` and `odd-co`, that determine if a number is even or odd, in the standard library.
[`test/test_fix2.scm`](https://github.com/write-you-a-scheme-v2/scheme/tree/master/test/test_fix2.scm) contains a standard library that defines a Y-combinator.
However, when we set this as the standard library then run the commented out expression, our expression fails
to terminate. Why is this?     
Create a new primitive `Vector` data type. Modify the parser to read `Vector` as numbers between brackets. In [`Prim.hs`](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Prim.hs) put the basic operations like add, multiply, then in standard library put operation like dot production, l2-distance. Include an auto-quote facility for the `Vector`special form.        

#### Let's Make Some Tests!
[home](home.html)...[back](07_io.html)...[next](09_test.html)
