Error Checking and Exceptions
------------


## Defining an error
An error will be defined as an internal misuse of a function, which will be the result of the user improperly using calling a function with , say, the wrong number of arguments. Errors are thrown in the IO monad and caught in the ExceptT monad, which is convenient for us, because they are all part of our monadic transformer stack. If you look at the return of evalutation, we will get ` runAppT :: EnvCtx -> Eval b -> ExceptT LispError IO b`, which keeps LispError and integral part of our staack. Let's take a look at some of the error's that we will be generating:
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
These will have a pretty print message, and if we think about it really hard, a way to describe the environoment in which the function failed and defaulted to a LispError instead of an Eval LispVal return. If you look at the errros, they are the result of the user doing someething that breaks the language specificication. But wahat about if they try to do something the computer cannot do? likke
```bash
echo 'a' > ./test
rm test
```
```Haskell
fileOpen test
```
This type of error isn't reallly the users fault, as far as they know, they are correct. So this gives us two classes of errors, errors from syntax and usage, and errors from the system. (We should figure out a way to reconcile these)/ 
