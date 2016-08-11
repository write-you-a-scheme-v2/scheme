First Steps: Compiling and running
------------

```bash
# Example Bash code
$ stack build
```

```haskell
-- Example Haskell code
main :: IO ()
main = return ()
```

## Version 2 changes
We are the version 2.0 of https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/
https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf
-- Monad Transformers for language evaluation, lexical scoping, IO, error handling    
`
-- Removal of 'IORef' and 'read' function    
-- Pretty Print for viewing parsed language structure    


## Organization
####Main.hs    
currently runs the Repl, should convert into file evaluator
####Repl.hs
defines the repl 
####Parser.hs    
parsec parsing
####Eval.hs      
contains evaluation function, environmental setting fns  
####LispVal.hs    
defines LispVal
#### Prims.hs
defines the std lib, depends Eval
####Pretty.hs    
pretty printer, pdoc?


##Chapters, v1.0
01_introduction.md    
02_parsing.md    
03_evaluation.md    
04_errors.md    
05_primitives.md    
06_repl.md    
07_mutation.md    
08_closures.md    
09_io.md    
10_stdlib.md    
11_conclusion.md    


##Chapters, v2.0
#### in progress
01_introduction.md    
02_parsing.md    
03_evaluation.md    
04_primitives.md    
05_repl.md    
06_errors.md    

#### future
07_mutation.md    
08_closures-and-scopes.md    
09_io.md    
10_stdlib.md    
11_conclusion.md    


