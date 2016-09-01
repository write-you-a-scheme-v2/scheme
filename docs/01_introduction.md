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

## 1.1 Lisp and its lists
Lisp is a list processing language. That means every expression is essentially a list. 
Each list, say `(+ 1 2)` is evaluated, and reduced. Whenever a list is encounter, it
is evaluated. This doesn't always make sense, say `(1 2 3)` where encountered, evaluating
it would be non-sensical. For this reason, `quote` or `'` is used. Now if we evaluate
something like `'(1 2 3)` or `(quote (1 2 3))` we get `(1 2 3)`. Essentially, this just delays evaluation, 
and lets us use lists for internal representation of data structures. 

## 1.2 List manipulation
There are three basic functions for manipulating these structures.    
`car`  ... `(car '(1 2 3))` => `(1)`   
`cadr` ... `(cadr '(1 2 3))` => `(2 3)`    
`cons` ... `(cons 1 '(2 3))` => `(1 2 3)`    

## 1.3 Internal representation, welcome to LispVal
The Haskell type we are going to use to represent lisp values is aptly named `LispVal`.  They
are the bricks, and our monadic evaluator the mortar. Together, they will build our language.
`data LispVal = DataConstructor InternalHaskellType` is the pattern used to define primitive
lisp values, like `Double` or `String`, the `List` data structure, {keywords like `lambda`
or `define`) and importantly variable or `Atom` names, like `x`.  The `LispVal` type is going 
to be central to our Scheme, bridging user input into an internal representation, then acting
like a tag system for monadic evaluation. We can start by first defining `LispVal`, and creating
the parser which maps the language's syntacital structure.    

## Define LispVal


## Next, Parsers!

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


