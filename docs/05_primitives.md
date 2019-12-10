---
title: Primitive Environment
header: Primitive Environment
date: November 28, 2016
author: Adam Wespiser
---
------------

## Primitive Strategy
The primitive environment is defined in
[Prim.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Prim.hs)  
Our basic strategy is to create a list of tuples, `[(T.Text,LispVal)]`, where the text is the name of the primitive, and the `LispVal` is a `Fun` representing the internal function.  We can use `Map.fromList` to convert this to a `Map` which is our environment for evaluation.  To create our `Fun`, we must map an internal Haskell function of some type to a `[LispVal] -> Eval LispVal`.  In the process, we must pattern match to get the corresponding `LispVal`s of the correct types, extract the values, and apply our Haskell function.  To help with this, we have created `binop`, `binopFold`, and `unop`.  This process is complicated to talk our way through, so I will go through an example in the subsequent sections to show how the type signatures reduce.    

## Full Definition
First, we'll take a look at what everything looks like all together. What we see is that a Haskell function is wrapped with a function, like `numOp` or `numCmp`, which pattern matches on `LispVal` to get the internal Haskell values.  This is then wrapped again by `binopFold` or `unop`, which convert the type of the argument to `[LispVal] -> Eval Lisp`, regardless of the number of arguments in the function `numCmp` or `numOp`.


```haskell
mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv = [   ("+"     , mkF $ binopFold (numOp    (+))  (Number 0) )
            , ("*"     , mkF $ binopFold (numOp    (*))  (Number 1) )
            , ("++"    , mkF $ binopFold (strOp    (<>)) (String ""))
            , ("-"     , mkF $ binop $    numOp    (-))
            , ("<"     , mkF $ binop $    numCmp   (<))
            , ("<="    , mkF $ binop $    numCmp   (<=))
            , (">"     , mkF $ binop $    numCmp   (>))
            , (">="    , mkF $ binop $    numCmp   (>=))
            , ("=="    , mkF $ binop $    numCmp   (==))
            , ("even?" , mkF $ unop $     numBool   even)
            , ("odd?"  , mkF $ unop $     numBool   odd)
            , ("pos?"  , mkF $ unop $     numBool (< 0))
            , ("neg?"  , mkF $ unop $     numBool (> 0))
            , ("eq?"   , mkF $ binop   eqCmd )
            , ("bl-eq?", mkF  $ binop $ eqOp     (==))
            , ("and"   , mkF $ binopFold (eqOp     (&&)) (Bool True))
            , ("or"    , mkF $ binopFold (eqOp     (||)) (Bool False))
            , ("cons"  , mkF   Prim.cons)
            , ("cdr"   , mkF   Prim.cdr)
            , ("car"   , mkF   Prim.car)
            , ("file?" , mkF $ unop  fileExists)
            , ("slurp" , mkF $ unop  slurp)
            ]
```
## Primitive Creation
Lets go through an individual example to see how all the types mash!    

#### Function Definition

```haskell
type Binary = LispVal -> LispVal -> Eval LispVal

(+) :: Num a => a -> a -> a

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
```

#### Function Reduction

```haskell
numOp (+) :: LispVal -> LispVal -> Eval LispVal
numOp (+) :: Binary

binopFold (numOp (+)) :: LispVal -> [LispVal] -> Eval LispVal
binopFold (numOp (+)) (Number 0) :: [LispVal] -> Eval LispVal

IFunc $ binopFold (numOp (+)) (Number 0) :: IFunc
mkF $ binopFold (numOp (+)) (Number 0) :: LispVal
```

Alright, so it's a complicated transformation, but as you can see the types do work out.  The engineering principle at play here is the use of the function `numOp`, and similar functions, for as many operators as possible. This reduces the amount of code needed to be written.  Further, `binop` and `unop` can be re-used for most functions.  Varags would have to be handled differently, possibly by entering each pattern match individually.     

## Helper Functions

```Haskell
type Prim   = [(T.Text, LispVal)]
type Unary  = LispVal -> Eval LispVal
type Binary = LispVal -> LispVal -> Eval LispVal

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x]    = op x
unop _ args    = throw $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x,y]  = op x y
binop _  args   = throw $ NumArgs 2 args

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
                            [a,b]  -> op a b
                            (a:as) -> foldM op farg args
                            []-> throw $ NumArgs 2 args
```
So the `binop`, `unop`, and `binopFold` are basically unwrapping functions that take a `[LispVal]` and an operator and apply the arguments to the operator. `binopFold` just runs the `foldM`, while taking an additional argument. It should be noted that `binopFold` requires the operator to work over monoids.    

## IO Functions   

```haskell
fileExists :: LispVal  -> Eval LispVal
fileExists (Atom atom)  = fileExists $ String atom
fileExists (String txt) = Bool <$> liftIO (doesFileExist $ T.unpack txt)
fileExists val  = throw $ TypeMismatch "expects str, got: " val

slurp :: LispVal  -> Eval LispVal
slurp (String txt) = liftIO $ wFileSlurp txt
slurp val          =  throw $ TypeMismatch "expects str, got:" val

wFileSlurp :: T.Text -> IO LispVal
wFileSlurp fileName = withFile (T.unpack fileName) ReadMode go
  where go = readTextFile fileName


readTextFile :: T.Text -> Handle -> IO LispVal
readTextFile fileName handle = do
  exists <- hIsEOF handle
  if exists
  then (TIO.hGetContents handle) >>= (return . String)
  else throw $ IOError $ T.concat [" file does not exits: ", fileName]


```
These are the basic file handling, `slurp`, which reads a file into a string, and `fileExists` which returns a boolean whether or not the file exists.

## List Comprehension
```haskell
cons :: [LispVal] -> Eval LispVal
cons [x,y@(List yList)] = return $ List $ x:yList
cons [c]                = return $ List [c]
cons []                 = return $ List []
cons _  = throw $ ExpectedList "cons, in second argumnet"

car :: [LispVal] -> Eval LispVal
car [List []    ] = return Nil
car [List (x:_)]  = return x
car []            = return Nil
car x             = throw $ ExpectedList "car"

cdr :: [LispVal] -> Eval LispVal
cdr [List (x:xs)] = return $ List xs
cdr [List []]     = return Nil
cdr []            = return Nil
cdr x             = throw $ ExpectedList "cdr"
```
Since the S-Expression is the central syntactical form of Scheme, list comprehension operators are a big part of the primitive environment.  Ours are not using the `unop` or `binop` helper functions, since there are a few cases which varargs need to be support.  A alternative approach would be to implement these as special forms, but since special forms are differentiated by their non-standard evaluation of arguments, they rightfully belong here, as primitives.      


## Unary and Binary Function Handlers
```Haskell
numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
numBool op (Number x) = return $ Bool $ op x
numBool op  x         = throw $ TypeMismatch "numeric op " x

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op (Number x) (Number y) = return $ Number $ op x  y
numOp op x          (Number y) = throw $ TypeMismatch "numeric op " x
numOp op (Number x)  y         = throw $ TypeMismatch "numeric op " y
numOp op x           y         = throw $ TypeMismatch "numeric op " x

strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp op x          (String y) = throw $ TypeMismatch "string op " x
strOp op (String x)  y         = throw $ TypeMismatch "string op " y
strOp op x           y         = throw $ TypeMismatch "string op " x

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp op  x       (Bool y) = throw $ TypeMismatch "bool op " x
eqOp op (Bool x)  y       = throw $ TypeMismatch "bool op " y
eqOp op x         y       = throw $ TypeMismatch "bool op " x

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return . Bool $ op x  y
numCmp op x          (Number y) = throw $ TypeMismatch "numeric op " x
numCmp op (Number x)  y         = throw $ TypeMismatch "numeric op " y
numCmp op x         y           = throw $ TypeMismatch "numeric op " x


eqCmd :: LispVal -> LispVal -> Eval LispVal
eqCmd (Atom   x) (Atom   y) = return . Bool $ x == y
eqCmd (Number x) (Number y) = return . Bool $ x == y
eqCmd (String x) (String y) = return . Bool $ x == y
eqCmd (Bool   x) (Bool   y) = return . Bool $ x == y
eqCmd  Nil        Nil       = return $ Bool True
eqCmd  _          _         = return $ Bool False
```
These are the re-used helper functions for wrapping Haskell functions, and pattern matching the LispVal arguments.  Further, the pattern matching can be used to dynamically dispatch the function at runtime depending on the data constructor of the arguments.  This is a defining feature of dynamically typed programming languages, and one of the many reasons why their performance is slow compared to statically typed languages! Another key feature within these functions is the throwing of errors for incorrect types, or mismatching types, being passed to functions.  This prevents type errors from being thrown in Haskell, and allows us to handle them in a way that allows for verbose error report.  Example: `(+ 1 "a")` would give an error!


## Conclusion
In summary, we make the primitive environment by wrapping a Haskell function with a helper function that pattern matches on `LispVals` and extracts the internal values that the Haskell function can accept.  Next, we convert that function to be of type `[LispVal] -> Eval LispVal`, which is our type `IFunc`, the sole argument for the `LispVal` data constructor `Fun`.  We can now map a `Text` value to a `LispVal` representing a function.  This is our primitive environment, which should stay minimal, and if possible, new functions moved into the standard library if they can be defined from existing functions.

## [Understanding Check]
Implement a new primitive function `nil?` which accepts a single argument, a list, returns true if the list is empty, and false under all other situations.    
Use the `Lambda` constructor instead of `Fun` for one of the primitive functions.    
Create a `Division` primitive function which works for `Number`, returning a `Number` by dividing then rounding down.     
Write a new function,
```
binopFold1 :: Binary -> [LispVal] -> Eval LispVal
```
that uses the first value of the list as the identity element.  More information, see [here](https://wiki.haskell.org/Fold).    


#### Next, Let's make a REPL!
[home](home.html)...[back](04_errors.html)...[next](06_repl.html)
