---
title: Read Eval Print Loop Repeat
date: November 28, 2016
author: Adam Wespiser
---
------------

> *Galileo alone had risked asserting the truth about our planet, and this made him a great man... His was a genuine career as I understand it.*  **Yevgeny Yevtushenko**

[Repl.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Repl.hs) defines the code we use for our REPL loop.   Our strategy will be to have the user enter text, parse, interpret, then display the result, then allow the user to enter another line of text.  If an exception is thrown, we will catch and display the exception, then return to our normal mode.    

```Haskell
type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl
```
Here we define out `Repl` type using `InputT` from the mtl library to wrap `IO`. `mainLoop` will run the REPL with default settings, and is the top-level function exported by [Repl.hs.](https://github.com/write-you-a-scheme-v2/scheme/tree/master/src/Repl.hs) that will be loaded into [Main.hs](https://github.com/write-you-a-scheme-v2/scheme/tree/master/exec/Main.hs) and used to create the executable.        

```haskell
repl :: Repl ()
repl = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process input) >> repl
    --Just input -> (liftIO $ processToAST input) >> repl
```
`repl` is a recursive function which will get a line of texted wrapped in the `Maybe` context, if `Nothing` then the function terminates, if `Just input` then we evaluate the text using the helper function `process`.  Commented out is an alternative processing line, which will display the abstract syntax tree, very useful for debugging.    


```Haskell
process :: String -> IO ()
process str = do
  res <- safeExec $ evalText $ T.pack str
  either putStrLn return res

processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str
```
`process` takes a `String` input, packs it into `Text`, evaluates it, then handles errors using `safeExec`.  Recall that `safeExec` return type `IO (Either String a)`.  This catches thrown exceptions and allows for the REPL to display the error, then allow the user to subsequently enter a fixed expression.    


## Conclusion
The REPL here is pretty basic, nothing fancy, just connect the user to the underlying interpreter and let them play! It takes input, runs the evaluator, then displays either the computed `LispVal` or the `LispException`.    


## [ Understanding Check ]
Every input is evaluated independently, that is, with a fresh `EnvCtx`. Can you figure out a way to thread the `EnvCtx`, so variables bound in one inputted expression can be used in subsequent inputs?    
REPL keywords like ":quit" ":help" ":clear" or ":ast" can be entered to allow for the REPL to do different tasks, like show the AST, information about commands, or quit out.  Write a quick `Parsec` parser to extract these keywords.    


#### Next?
[home](home.html)...[back](05_primitives.html)...[next](07_io.html)
