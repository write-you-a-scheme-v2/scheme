Error Checking and Exceptions
------------
## Some clarification
When the user enters some incorrect input, we can say they have made an error, and people understand what we mean, generally, however there is a technical difference between errors and exceptions in Haskell. Errors, refer to situations where our project cannot handle itself, and we must change the source code to remedy the situation. These are unexpected situations. Exceptions, on the other hand, represent expected, but still irregular situations that we can control. Exceptions can represent problems with a potential Scheme program, like an error parsing, or a bad special form. [Haskell Wiki](https://wiki.haskell.org/Error_vs._Exception).    
The sources for errors and exceptions in Haskell are as follows:    
Exceptions: `Prelude.catch`, `Control.Exception.catch`, `Control.Exception.try`, `IOError`, `Control.Monad.Error`    
Errors: `error`, `assert`, `Control.Exception.catch`, `Debug.Trace.trace`     
Try to keep these in mind, but expect to see only exceptions, unless I've made an error in my assertion, in which case I'll have to trace through and catch my mistake.    

## Exceptions Everywhere!
Undefined, unexpected, and generally out of control situations are a big type of any kind of large system, especially one interacting with the outside world or dealing with use input as complex and complicated as a programming language. Control is an illusion.  For our system, we must accept user input, determine that it is valid Scheme syntax, then compute that syntax into a final value.  During this process we may interact with the file system or network.  It is especially important for programming languages to report and describe the nature of the irregularity.     
Thus, there are three types of exceptions that exist in our implementations of Scheme: **Parsing, Evaluation, and IO**. Each of these originate in a distinct type of activity the parser or interpreter is undergoing, but all of them are end up going through the `Eval` monad and are caught and displayed in the same place. (see [Eval.hs](../src/Eval.hs))
```Haskell
someFun :: GoodType -> Eval LispVal
someFun (BadMatch x) = return $ throwM $ LispExceptionConstructor "message we send"
```

```Haskell
throwError :: e -> m a
```



## Defining an Exception
```
safeExec :: IO a -> IO (Either String a)
safeExec m = do
  result <- Control.Exception.try m
  case result of
    Left (eTop :: SomeException) ->
      case fromException eTop of
        Just (enclosed :: LispException) -> return $ Left (show enclosed)
        Nothing                -> return $ Left (show eTop)
    Right val -> return $ Right val
```
An exception will be defined as an internal misuse of a function, a user deviation from accepted syntax or semantics, or the request of an unavailable external resource. Exceptions are thrown in the monad transformer stack, `Eval`, and caught with the `safeExec` function, which is convenient for us, because we can throw exceptions from any function return `Eval LispVal`, which is most of our evaluation code!

```haskell

-- TODO make a pretty printer
data LispException
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

Each of these data constructors serve to distinguish the source of their error, and provide useful information to the user who must decide how to debug their program after a `LispException` is returned. Though not as descriptive as a fully featured language, we do have the capacity to provide a fair amount of information, including the a custom message and `LispVal` that are not compatible.

```haskell
instance Show LispException where
  show = T.unpack . showError
unwordsList :: [LispVal] -> T.Text
unwordsList list = T.unwords $  showVal <$> list

showError :: LispException -> T.Text
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

 Similar to our `showVal`, from [Chapter 1](01_introduction.md), we override the `show` Typeclass to give a custom message for the type.  The showError has a special case for PError, which uses  `String` and just wraps the error message from the parser. The next source `IO`, can also be tricky. Although we have the ability to throw an `IOError`, if there is an unchecked exception, it will fall through.  









## Conclusion
Accidents will happen, and so we have exception handling for IO, parsing, and evaluation exceptions via our `LispException`. Exceptions are realized everywhere we have functions that evaluate, so handling them is composed into our `Eval` monad. Verbose exception messages are vital to usability, and we must report enough information to pinpoint the user's misuse of proper syntax, semantics, or resource request.  Our main liability with the current monad transformer stack, is that `IO` can throw an error that is not caught, an unchecked exception. However, we are only using `IO` to read files, not maintaining open connections for long periods of time, and our exposure is minimal.  If this is a major concern for you, read the section on "Alternative Exceptions", which discusses other ways to handle exceptions in Haskell.             

## [ Understanding Check ]
Where would you implement more, and specific `LispVal` values in the error messages to better help the user debug their expressions?        
Go through Eval.hs, find an `LispException` used in a few places and replace it with a new error that is more specific, or merge two `LispException` that are better served by a single constructor. Include support for `showError`.     
Many programming languages have information like, "line 4, column 10: variable x is not bound". How would you go about adding line and column information to messages passed to `throwError` in Eval.hs?     
We are taking a basic approach to format error messages: `T.concat` and `show`. [Text.PrettyPrint](https://hackage.haskell.org/package/pretty-1.1.3.4/docs/Text-PrettyPrint.html) offers a rich way to display messages using pretty print combinators. Implement a PrettyPrint interface for `LispException` that provides a uniform interface.   

## Alternative Exceptions (skippable)
We are handling errors in a very basic way. The use of `IO` causes some trickiness that we won't be able to handle.  Here's why:    
* **ExceptT someErrorType IO a** considered bad [Exceptions Best Practices](https://www.schoolofhaskell.com/user/commercial/content/exceptions-best-practices). The authors list three reasons why this is considered an anti pattern:  1) Its noncomposable, we see this when we have to do add in the Parser error, and the information in the parser error is not completely congruent with the information we pass to other error messages. 2) It gives the implication that only `LispException` can be thrown. This is true, during `slurp` or any `IO` operation, an error cna be thrown that will not be caught. 3) We haven't limited the possible exceptions, we've just added `throwError` or `liftIO . throwIO`.
* **enclosed exceptions**](https://github.com/jcristovao/enclosed-exceptions)  [FP complete's Catching All Exceptions. ](https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions).  The goal is to catch all exceptions that arise from code.
* a plethora of options for error/exception handling in hasell:    
[Control-Monad-Trans-Except](https://hackage.haskell.org/package/transformers-0.5.0.0/docs/Control-Monad-Trans-Except.html)    
 [Control-Monad-Error](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Error.html)    
[Control-Monad-Catch](https://hackage.haskell.org/package/exceptions-0.8.0.2/docs/Control-Monad-Catch.html)    
[Control-Monad-Except](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html)     
[Control-Exception](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception.html)   
[UnexpectionIO](https://hackage.haskell.org/package/unexceptionalio)     




#### Next, Let's make some functions!

[home](00_overview.md)...[back](03_evaluation.md)...[next](05_primitives.md)
