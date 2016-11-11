Error Checking and Exceptions
------------
## Errors Everywhere!
Three types of errors exist in our implementations of Scheme: Parsing, IO, and Evaluation. Each of these originate in a distinct type of activity the interpreter is undergoing, but all of them are end up going through the `Eval` monad and are caught and displayed in the same place.
```Haskell
someFun :: GoodType -> Eval LispVal
someFun (BadMatch x) = return $ throwError $ lispErrorConstructor "message we send"
```

## Defining an error
An error will be defined as an internal misuse of a function, a user error, or the lack of an external resource. Errors are thrown in the IO monad and caught in the ExceptT monad, which is convenient for us, because they are all part of our monadic transformer stack. If you look at the return of evaluation, we will get ` runAppT :: EnvCtx -> Eval b -> ExceptT LispError IO b`, which keeps LispError and integral part of our stack. Let's take a look at some of the error's that we will be generating:
```Haskell
data LispError
  = NumArgs Integer [LispVal]
  | LengthOfList String Int
  | ExpectedList T.Text
  | TypeMismatch String LispVal
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String
  deriving (Show)
```
These will have a pretty print message, and if we think about it really hard, a way to describe the environment in which the function failed and defaulted to a LispError instead of an Eval LispVal return. If you look at the errors, they are the result of the user doing something that breaks the language specification.


```Haskell
instance Show LispError where
  show = T.unpack . showError

showError :: LispError -> T.Text
showError err =
  case err of
    (NumArgs int args)       -> "Error Number Arguments"
    (LengthOfList sts int)   -> "Error Length of List"
    (ExpectedList txt)       -> "Error Expected List"
    (TypeMismatch str val)   -> T.concat ["Error Type Mismatch: ", T.pack str, showVal val]
    (BadSpecialForm str val) -> "Error Bad Special Form"
    (NotFunction str str1)   -> "Error Not a Function"
    (UnboundVar str str1)    -> "Error Unbound Variable"
    (PError str)             -> T.concat ["Parser Error, expression cannot evaluate: ",T.pack str]
    (Default str)            -> T.concat ["Error, Danger Will Robinson! ", T.pack str]
    _                        -> "I got 99 problems, most of which is the parser"
```
 Similar to our `showVal`, from [Chapter 1][01_introduction], we override the `show` typeclass to give a custom message for the type. 
