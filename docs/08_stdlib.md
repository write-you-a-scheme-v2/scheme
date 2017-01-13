---
title: Towards a Standard Library: Fold and Unfold
date: November 28, 2016
author: Adam Wespiser
---

----
> *Cry 'Havoc!', and let slip the dogs of war*  **William Shakespeare**



## Standard Library
Defining these functions can happen in three places: as special forms like `let`, in the primitive environment like `+`, or as an external file written in the programming language.
The standard library is the third of these choices.
It defines functions using both primitive and special forms.
If a function does not need a special evaluation strategy, or mapping to an internal operator, it belongs in the library.
Good programming languages will have a collection of functions that provide a extended functionality for the language.
The standard library will contain the functions that empower users to complete their tasks.
When building a language, the standard library is often the first real "program" written in that language.
When we write the standard library, we are not only providing the user with functionality, but proving many features of the language work!


#### Our Standard Library
We define the standard library, `test/stdlib_mod.scm` with a multitude of functions. Importantly, we allow recursive functions, like `fold` and `reduce` which enable efficient functional programming.

#### Composing `car` and `cdr`
Our standard library defines functions that are combinations of `car` and `cdr`, for example `cdar` is `define cdar (lambda (pair) (cdr (car pair))))`
For this to work, `car` and `cdr` must accept both quoted and unquoted listed. In other words, to chains these functions, they must accept lists without evaluated the arguments. This makes these functions specials forms.     
In `src/Eval.hs`    
```Haskell
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
#### Running Standard Library
[`src/Eval.hs`]() examples that show File -> LispVal -> LispVal + Stdlib -> Eval LispVal
The following Haskell code evaluates an expression within the context of the standard library.
Because the reader monad does not return the input context, we must wrap the expresion and the standard library together.
This is done with the `endOfList` function


Define the standard libary file.
```Haskell
sTDLIB :: T.Text
sTDLIB = "test/stdlib_mod.scm"
```


Parse the input function, and the standard library as a list. Concat the parsed input function to the end of the standard library list.
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
The key here is the `evalBody`, which accepts an S-Epression consisting of a series of `define` statements to be sequentially evaluated.
This is the same evaluation strategy used in both `let` and `lambda` expressions.

```Haskell
textToEvalForm :: T.Text -> T.Text -> Eval LispVal
textToEvalForm std input = either (throw . PError . show )  evalBody $ parseWithLib std input

evalText :: T.Text -> IO () --REPL
evalText textExpr = do
  stdlib <- getFileContents $ T.unpack  sTDLIB
  res <- runASTinEnv basicEnv $ textToEvalForm stdlib textExpr
  print res
```

#### Why we take this approach
The standard library is the third and final place we define functionality in our language, after special forms and the primitive environment.
The idea behind the standard library is that we have a place
`e -> a` functionality of `ReaderT` monad.  
Alternative approach w/ `StateT` that can provide the ability to run the monad, and get a modified `state`.

## [ Understanding Check ]
Add your standard library function to generate the the first n Fibonacci numberers using a recursive function.
Create a new primitive `Vector` data type. Modify the parser to read `Vector` as numbers between brackets. In `src/Prim.hs` put the basic operations like add, multiply, then in standard library put operation like dot production, l2-distance.

#### Let's Make Some Tests!
[home](home.html)...[back](07_io.html)...[next](09_test.html)
