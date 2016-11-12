Evaluation, Part 1
------------
#### Evaluation Context
![img](../img/WYAS-Eval-If-Statement.png)
LispVal.hs defines our key data structure for evaluation:   

ski, notes:
in the first `begin' case, you could call `evalBody [rest]', and in the second case, you could call `evalBody rest' (and then perhaps rename one or both of those `rest's, to avoid confusion)
(oh, and `lambda' also ought to allow multiple forms in the body)


```
newtype Eval a = Eval { unEval :: ReaderT EnvCtx (ExceptT LispError IO ) a }
  deriving (Monad, Functor, Applicative, MonadReader EnvCtx, MonadError LispError, MonadI
```
This seemingly complex code will be the data structure used to control evaluation. Specifically, it allows for lexical scoping, user input, error throwing, error catching, and will contain the return `LispVal` for any valid expression. If your asking yourself how such a complex data structure is possible, then welcome to the wonderful world of monad transformers. ThThese wonderful abstractions allow programmers to combine monads and automatically generate the correctly lifted functions. For instance, our `Eval`'s `IO` would need its functions to be lifted, but since we derived `MonadIO`, this automatically happens for us.    
This 'magic' is made possible through `{-# LANGUAGE GeneralizedNewtypeDeriving #-}` pragma. I will not be discusses this further, but its really a powerful technique that enables wonderfully powerful yet succint abstractions to serve at the heart of our Lisp!    
Using our aptly named `Eval` structure, we will define the `eval` function within Eval.hs as follows:
```
eval :: LispVal -> Eval LispVal
```
Outline:
environment/scoping issues, variable set/get
Primitive Environment
eval function
special cases for eval function:
  let, lambda, define, def, quote, List
eval function handlers


####Sauces:
http://catamorph.de/documents/Transformers.pdf    
https://github.com/justinethier/husk-scheme/tree/master/hs-src/Language/Scheme  

#### Danger Will Robinson, on to Errors!
[home](00_overview.md)...[back](03_evaluation.md)...[next](04_errors.md)
