Error Checking and Exceptions
------------
## Errors Everywhere!
Errors are a big type of any kind of large system, especially one interacting with the outside world.  For our system, we must accept user input, determine that it is valid Scheme syntax, then compute that syntax into a final value.  During this process we may interact with the file system or network.  It is especially important for programming languages to report and describe the nature of the error.     
Thus, there are three types of errors exist in our implementations of Scheme: **Parsing, Evaluation, and IO**. Each of these originate in a distinct type of activity the parser or interpreter is undergoing, but all of them are end up going through the `Eval` monad and are caught and displayed in the same place. (see [Eval.hs](../src/Eval.hs))
```Haskell
someFun :: GoodType -> Eval LispVal
someFun (BadMatch x) = return $ throwError $ lispErrorConstructor "message we send"
```

```Haskell
throwError :: e -> m a
```

## Defining an error
An error will be defined as an internal misuse of a function, a user error, or the lack of an external resource. Errors are thrown in the IO monad and caught in the ExceptT monad, which is convenient for us, because they are all part of our monadic transformer stack. If you look at the return of evaluation, we will get ` runAppT :: EnvCtx -> Eval b -> ExceptT LispError IO b`, which keeps LispError and integral part of our stack. Let's take a look at some of the error's that we will be generating, as defined in [LispVal.hs](../src/LispVal.hs):

```haskell

-- TODO make a pretty printer
data LispError
  = NumArgs Integer [LispVal]
  | LengthOfList T.Text Int
  | ExpectedList T.Text
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | Default LispVal
  | PError String -- from show anyway
  | IOError T.Text

```

Each of these data constructors serve to distinguish the source of their error, and provide useful information to the user who must decide how to debug their program after a `LispError` is returned. Though not as descriptive as a fully featured language, we do have the capacity to provide a fair amount of information, including the a custom message and `LispVal` that are not compatible.

```haskell
instance Show LispError where
  show = T.unpack . showError
unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $  showVal <$> list

showError :: LispError -> T.Text
showError err =
  case err of
    (IOError txt)          -> T.concat ["Error reading file: ", txt]
    (NumArgs int args)     -> T.concat ["Error Number Arguments, expected ", T.pack $ show int, " recieved args: ", unwordsList args]
    (LengthOfList txt int) -> T.concat ["Error Length of List in ", txt, " length: ", T.pack $ show int]
    (ExpectedList txt)     -> T.concat ["Error Expected List in funciton ", txt]
    (TypeMismatch txt val) -> T.concat ["Error Type Mismatch: ", txt, showVal val]
    (BadSpecialForm txt)   -> T.concat ["Error Bad Special Form: ", txt]
    (NotFunction val)      -> T.concat ["Error Not a Function: ", showVal val]
    (UnboundVar txt)       -> T.concat ["Error Unbound Variable: ", txt]
    (PError str)           -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    (Default val)          -> T.concat ["Error, Danger Will Robinson! Evaluation could not proceed!  ", showVal val]

```

 Similar to our `showVal`, from [Chapter 1](01_introduction.md), we override the `show` Typeclass to give a custom message for the type.  The showError has a special case for PError, which uses  `String` and just wraps the error message from the parser. The next source `IO`, can also be tricky. Although we have the ability to throw an `IOError`, if there is an unchecked exception, it will fall through.  Our approach using `ExceptT` has recently come under fire, [Exceptions Best Practices ](https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell), although I think it is an appropriate solution for our situation, as we do not handle significant amounts of IO , network or async data, and further have a good handle on the situations which throw exceptions.  That said, proper error messages are a tragically under looked aspect of many programming languages that makes learning the language as a novice nearly impossible.  We owe our users the best possible experience.   



## Conclusion
Accidents will happen, and so we have error handling for IO, parsing, and evaluation errors via our `LispError`. Errors are realized everywhere we have functions that evaluate, so handling them is composed into our `Eval` monad. Verbose error messages are vital to usability, and we must report enough information to pinpoint the user's error.  Our main liability with the current monad transformer stack, is that `IO` can throw an error that is not caught, unchecked exception. However, we are only using `IO` to read files, not maintaining open connections for long periods of time, and our exposure is minimal.        


## Alternative Exceptions
We are handling errors in a very basic way. The use of `IO` causes some trickiness that we won't be able to handle.  Here's why:    
* **ExceptT someErrorType IO a** considered bad [Exceptions Best Practices](https://www.schoolofhaskell.com/user/commercial/content/exceptions-best-practices). The authors list three reasons why this is considered an anti pattern:  1) Its non-composable, we see this when we have to do add in the Parser error, and the information in the parser error is not completely congruent with the information we pass to other error messages. 2) It gives the implication that only `LispError` can be thrown. This is true, during `slurp` or any `IO` operation, an error cna be thrown that will not be caught. 3) We haven't limited the possible exceptions, we've just added `throwError` or `liftIO . throwIO`.
* **enclosed exceptions**](https://github.com/jcristovao/enclosed-exceptions)  [FP complete's Catching All Exceptions. ](https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions).  The goal is to catch all exceptions that arise from code.
* a plethero of options for error/exception handling in hasell:    
[Control-Monad-Trans-Except](https://hackage.haskell.org/package/transformers-0.5.0.0/docs/Control-Monad-Trans-Except.html)    
 [Control-Monad-Error](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Error.html)    
[Control-Monad-Catch](https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/Control-Monad-Catch.html)    
[Control-Monad-Except](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html)     
[Control-Exception](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception.html)   
[UnexpectionIO](https://hackage.haskell.org/package/unexceptionalio)     


## [ Understanding Check ]
Where would you implement more, and specific `LispVal` values in the error messages to better help the user debug their expressions?        
Go through Eval.hs, find an `LispError` used in a few places and replace it with a new error that is more specific, or merge two `LispError` that are better served by a single constructor. Include support for `showError`.     
Many programming languages have information like, "line 4, column 10: variable x is not bound". How would you go about adding line and column information to messages passed to `throwError` in Eval.hs?     
We are taking a basic approach to format error messages: `T.concat` and `show`. [Text.PrettyPrint](https://hackage.haskell.org/package/pretty-1.1.3.4/docs/Text-PrettyPrint.html) offers a rich way to display messages using pretty print combinators. Implement a PrettyPrint interface for `LispError` that provides a uniform interface.        

#### Next, Let's make some functions!

[home](00_overview.md)...[back](03_evaluation.md)...[next](05_primitives.md)
