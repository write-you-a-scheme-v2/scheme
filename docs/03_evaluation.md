Evaluation, Part 1
------------
#### Evaluation Context
![img](../img/WYAS-Eval-If-Statement.png)
LispVal.hs defines our key data structure for evaluation:   


## Eval Monad

```haskell
newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadI
```

This code will be the form used to control evaluation. Specifically, it allows for lexical scoping, user input, error throwing, error catching, and will contain the return `LispVal` for any valid expression. If your asking yourself how such a complex data structure is possible, then welcome to the world of monad transformers. These wonderful abstractions allow programmers to combine monads and automatically generate the correctly lifted functions. For instance, our `Eval`'s `IO` would need its functions to be lifted, but since we derived `MonadIO`, this automatically happens for us.    
This 'magic' is made possible through `{-# LANGUAGE GeneralizedNewtypeDeriving #-}` pragma. I will not be discusses this further, but its really a powerful technique that enables wonderfully powerful yet succinct abstractions to serve at the heart of our Lisp!    

## Running The Eval Monad
Getting the `EnvCtx`    

```haskell
basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.fromList $ primEnv
          <> [("read" , Fun $ IFunc $ unop $ readFn)]
```

This is our primitive environment, which will be detailed in the next chapter on [Prim.hs](../src/Prim.hs). Recall that we defined `EnvCtx` in [LispVal.hs](../src/LispVal.hs) as:

```haskell
type EnvCtx = Map.Map T.Text LispVal
```
Our environment is a collection of bindings between names and entities referenced by the names. For now, we must only concern ourselves with the fact that the names in the environment comes from `LispVal's` `Atom` data constructor.  This data structure is going to be the basis of our lexical scoped variable look up.    
```haskell
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

```haskell
readExprFile :: T.Text -> Either ParseError LispVal
readExprFile = parse (contents parseList) "<file>"
```
There is a lot of movement here (possibly dragons), and the functions above do the following things:    
* `evalFile` is used from the `main :: IO ()` loop to run a program file.    
* `readExprFile` runs the parser on the program text to return a `LispVal` or `ParseError`.    
* `fileToEvalForm` runs the parser, if an error occurs, it converts it into a `LispError`, `PError`, else, it evaluates it using `evalBody`.     
* `runASTinEnv` executes the the `evalBody :: LispVal -> Eval Body` with the `EnvCtx`, essentially running our program by first unwrapping `Eval` with `unEval` (the data accessor to `Eval`), then using the `runReaderT` and `runExceptT` functions on the transformed monad.

## eval function: rationale     
Using our aptly named `Eval` structure, we will define the `eval` function within [Eval.hs]](../src/Eval.hs) as follows:

```haskell
eval :: LispVal -> Eval LispVal
```

Given our type signature, we can think back to our Scheme semantics, and recall we need to handle a few special cases:       

* **LispVal**    For primitive types, like Number or String, these values will evaluate to themselves. This is known as the autoquote facility of Lisp.    
* **begin**    The begin function takes a series (one or more) S-Expressions, starting with 0 or more `define` statements, and evaluates each form in order.  The argument to `begin` will be known ad a "body" or "body-expression".    
* **define**    Binds an evaluated `LispVal` to an `Atom`.    
* **write**    Takes its argument, un-evaluated, and returns a `String` whose value is the result of running `showVal`.    
* **if**    Evaluates its first form, if true, evaluates second, else, evaluates the third.  The same as every other language!    
* **let**    Let takes a list of alternating atoms and S-Expressions and an S-Expression, which is a body expression.  The first argument of alternating values and S-Expressions is bound to a local environment which the body is evaluated in. No order of evaluation on the first argument can be assumed, thus variables to be bound cannot appear in the S-Expression bound to another variable.    
* **lambda**    This is how we will create anonymous functions.  Thinking back to our `LispVal`, recall the data constructor, `Lambda IFunc EnvCtx` where essentially `IFunc :: LispVal -> Eval LispVal`. Two arguments are accepted, a list of atomic values to serves as the function parameters, and a body-expression, which can be evaluated in an environment with the incoming arguments bound to the parameters from the first argument.  The `EnvCtx`, and thus lexical scope, can be stored be getting the local environment via the `reader` monad function, `ask`.  Lambda is really powerful, so incredibly powerful, that we could use it with `Atom` to implement everything else. It's a neat idea called [Lambda Calculus](http://dev.stephendiehl.com/fun/lambda_calculus.html)!         

These will be known as our "Special Forms", different from functions defined in the primitive environment or standard library, as they are implemented via pattern matching on the argument (`LispVal`) of the `eval` function.  All other functions will either be primitives or from the standard library, and have their argument evaluated before being passed into the function.  This difference is what determines what must be implemented as an `eval` pattern match, and what can be done elsewhere.            

## eval function: implementation
```Haskell
eval :: LispVal -> Eval LispVal
```
The eval function is the heart of our interpreter, and must be able to pattern match every possible valid syntax, as well as the special forms. This is a pretty tall order, so we are going to approach this by going through the eval function piece by piece along with the helper functions needed to run that code. It's a little disjoint, but the simplest way to explain exactly how we are going to implement the syntax and semantics of Scheme in Haskell.  As always, to see it all together, see [Eval.hs](../src/Eval.hs). As we go through the code you will see some `throwError`, which are covered in the next chapter, for now, recognize that `throwError $ LispErrorConstructor "message-1"` returns `Eval LispVal`.          



#### autoquote      
```Haskell
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil        = return Nil
```
autoquote returns self for Number, Bool, String, Nil. () => Nil, 
#### write     
```Haskell
eval (List [Atom "write", rest])      = return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) = return . String . T.pack . show $ List rest
```
write does not evaluate argument

#### quote     
```Haskell
eval (List [Atom "quote", val]) = return val
```
quote return un-evaluated value

#### Atom
```Haskell
eval n@(Atom _) = getVar n

getVar :: LispVal ->  Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
      Just x  -> return x
      Nothing -> throwError $ UnboundVar atom
getVar n = throwError $ TypeMismatch  "failure to get variable: " n
```
search environment for binding, return associated value


#### if    
```Haskell
eval (List [Atom "if", pred, ant, cons]) = do
  ifRes <- eval pred
  case ifRes of
      (Bool True)  -> eval ant
      (Bool False) -> eval cons
      _            -> throwError $ BadSpecialForm "if"
eval args@(List ( (:) (Atom "if") _))  = throwError $ BadSpecialForm "if"
```
std. if/else/then function


#### let
```Haskell
eval (List [Atom "let", List pairs, expr]) = do
  env   <- ask
  atoms <- mapM ensureAtom $ getEven pairs
  vals  <- mapM eval       $ getOdd  pairs
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
  in local (const env')  $ evalBody expr

eval (List (Atom "let":_) ) = throwError $ BadSpecialForm "let"

getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x:xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return  n
ensureAtom n = throwError $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
```
bind pairs: check even for atoms, eval odds, make new env, eval expr in new env



#### begin, define & evalBody
```Haskell
eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest

eval (List [Atom "define", varExpr, expr]) = do
  varAtom <- ensureAtom varExpr
  evalVal <- eval expr
  env     <- ask
  let envFn = const $ Map.insert (extractVar varAtom) evalVal env
  in local envFn $ return varExpr

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  env     <- ask
  local (const $ Map.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  env     <- ask
  let envFn = const $ Map.insert var evalVal env
  in local envFn $ evalBody $ List rest
evalBody x = eval x
```
* create diagram for eval body



#### lambda & applyLambda
```Haskell
eval (List [Atom "lambda", List params, expr]) = do
  envLocal <- ask
  return  $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throwError $ BadSpecialForm "lambda"

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
  env <- ask
  argEval <- mapM eval args
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params argEval) <> env
  in local (const env' ) $ eval expr

```
get env, return function w/ applylambda that takes expr and params
to evaluate lambda, see applyLambda:
  eval input args in calling env, create new environment with input args bound, eval expr w/ new Environment

#### application
```Haskell
eval (List ((:) x xs)) = do
  funVar <- eval x
  xVal   <- mapM eval  xs
  case funVar of
      (Fun (IFunc internalFn)) -> internalFn xVal
      (Lambda (IFunc internalfn) boundenv) -> local (const boundenv) $ internalfn xVal
      _                -> throwError $ NotFunction funVar
```
find var, eval args, case on func, (internal func, or, lambda)

## Conclusion
That was a lot! If `LispVal` defines the syntax, then `Eval` defines the semantics. Now is a good time to read [Eval.hs](../src/Eval.hs) and see it work all together, since that's all that defines the mechanism of evaluation.  Our implementation is possible by monad transformers, specifically the integration of `ReaderT`, which we use to implement lexical scope.  Without monads, we would have to pass in an extra argument to every `eval`, as well as some of the helper functions, not to mention the complication of handling the other functionality of monads composed within our `Eval`. For a simple interpreter, its hard to do much better. For a faster interpreter, we would need to compile.      
Anyway, we have the the basis for our language and could start work on proving theoretical properties. We won't do this, and instead move towards gaining the next thing needed to be practical, a collection of basic operations to manipulate data.  But before moving on to primitives, we quickly cover errors, which show up all over `eval` function.  If you just can't wait to define some functions and get the REPL up and running, the basic message is, when bad things happen, throw an error that gives the user enough information to fix the problem.        

## [Understanding Check]
Implement a delay function as a special form that returns its argument as the body of a lambda expression that accepts no arguments. (delay x) => (lambda () x)    
You careful read the R5RS standard and are upset to realize we have implemented `let` incorrectly. Instead of `(let (x 1 y 2) (+ x y))` the standard calls for `(let ((x 1) (y 2)) (+ x y))`. Make this change.    
GADTs are sometimes used to implement `LispVal`. How would evaluation change?        
Implement one of (`case`, `letrec`, `let*`, `sequence`, `do`, `loop`) as a special form.  Check the [R5RS](../docs/R5RS.pdf) standard for more information.    
[Bonus] Find a unique difference in the implementation of special forms between this project and R5RS, implement the special form, then submit a new PR.        

#### Additional Resources:

* http://catamorph.de/documents/Transformers.pdf    
* https://github.com/justinethier/husk-scheme/tree/master/hs-src/Language/Scheme  

#### Danger Will Robinson, on to Errors!
[home](00_overview.md)...[back](03_evaluation.md)...[next](04_errors.md)
