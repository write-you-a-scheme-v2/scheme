Evaluation, Part 1
------------
#### Evaluation Context
![img](../img/WYAS-Eval-If-Statement.png)
LispVal.hs defines our key data structure for evaluation:   

"#haskell" ski, notes:
in the first `begin' case, you could call `evalBody [rest]', and in the second case, you could call `evalBody rest' (and then perhaps rename one or both of those `rest's, to avoid confusion)
(oh, and `lambda` also ought to allow multiple forms in the body)

## Eval Monad
```Haskell
newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadI
```
This code will be the form used to control evaluation. Specifically, it allows for lexical scoping, user input, error throwing, error catching, and will contain the return `LispVal` for any valid expression. If your asking yourself how such a complex data structure is possible, then welcome to the world of monad transformers. These wonderful abstractions allow programmers to combine monads and automatically generate the correctly lifted functions. For instance, our `Eval`'s `IO` would need its functions to be lifted, but since we derived `MonadIO`, this automatically happens for us.    
This 'magic' is made possible through `{-# LANGUAGE GeneralizedNewtypeDeriving #-}` pragma. I will not be discusses this further, but its really a powerful technique that enables wonderfully powerful yet succinct abstractions to serve at the heart of our Lisp!    

## Running The Eval Monad
Getting the `EnvCtx`    
```Haskell
basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList $ primEnv
          <> [("read" , Fun $ IFunc $ unop $ readFn)]
```
This is our primitive environment, which will be detailed in the next chapter on [Prim.hs](../src/Prim.hs). Recall that we defined `EnvCtx` in [LispVal.hs](../src/LispVal.hs) as:
```Haskell
type EnvCtx = Map.Map T.Text LispVal
```

```Haskell
evalFile :: T.Text -> IO () --program file
evalFile fileExpr = do
  out <- runExceptT $ runASTinEnv basicEnv $ fileToEvalForm fileExpr
  either (putStrLn . show) (putStrLn . show) out

runASTinEnv :: EnvCtx -> Eval b -> ExceptT LispError IO b
runASTinEnv code action = do
  res <- liftIO $ runExceptT $ runReaderT (unEval action) code
  ExceptT $ return res

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input =
  either lVal evalBody $ readExprFile input
  where lVal = (throwError . PError . show )  evalBody $ readExprFile input
```
From [Parser.hs](../src/Parser.hs) we have
```Haskell
readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
```
`evalFile` is used from the `main :: IO ()` loop to run a program file.    
`readExprFile` runs the parser on the program text to return a `LispVal` or `ParseError`.    
`fileToEvalForm` runs the parser, if an error occurs, it converts it into a `LispError`, `PError`, else, it evaluates it using `evalBody`.     
`runASTinEnv` executes the the `evalBody :: LispVal -> Eval Body` with the `EnvCtx`, essentially running our program! Here be dragons.    


## eval
Using our aptly named `Eval` structure, we will define the `eval` function within Eval.hs as follows:
```
eval :: LispVal -> Eval LispVal
```
Given our type signature, we can think back to our Scheme semantics, and recall we need to handle a few special cases:       
**LispVal**    
**begin**    
**define**    
**write**    
**if**    
**let**    
**lambda**    
These will be known as our "Special Forms", and they will not be functions defined in the primitive environment or standard library, but instead be implemented by pattern matching on the argument (`LispVal`) of the `eval` function.    


## [Understanding Check]
Implement a delay function as a special form that returns its argument un-evaluated.    
You careful read the R5RS standard and are upset to realize we have implemented `let` incorrectly. Instead of `(let (x 1 y 2) (+ x y))` the standard calls for `(let ((x 1) (y 2)) (+ x y))`. Make this change.    
GADTs are sometimes used to implement `LispVal`. How would you go about doing this?    
Implement a `case` statement.  Check the [R5RS](../docs/R5RS.pdf) standard for more information.    
[Bonus] Find a unique difference to R5RS that this project can easily implement and put in a PR.    

####Additional Resources:
http://catamorph.de/documents/Transformers.pdf    
https://github.com/justinethier/husk-scheme/tree/master/hs-src/Language/Scheme  

#### Danger Will Robinson, on to Errors!
[home](00_overview.md)...[back](03_evaluation.md)...[next](04_errors.md)
