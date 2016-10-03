First Steps: Compiling and running
------------

## Version 2 changes
We are the version 2.0 of https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/
https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf
-- Monad Transformers for language evaluation, lexical scoping, IO, error handling    


## Organization of the project
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

## 1.0 What are we going to do?
We are going to make a very basic, but robust, programming language that is very extendible. 
I encourage you to take the language we build and use it for whatever you want. I want to see
more Haskell in industry, and language development is where we can really make a difference, today, 
not tomorrow. 

## How are we going to do this?
To make a programming language, we must take user inputted text, turn that text into
a format understandable by the interpreter, then evaluate that format into a result. 
Before we go further, you should familairize yourself with Scheme, basically it uses
parens to deliniate function calls. `(fn 1 2)` would call the function `fn` with args
1 and 2. There are a few keywords, like `let`, `lambda`, and `define` that must be 
parsed into special forms. Scheme has two basic utilities, or general functions, `eval`
and `apply`. In general terms, `eval` is called to reduce Scheme expressions, and `apply`
specifically called to on functions and their arguments. 


## 1.0.1 A contextual view of our approach. 
Lisp is a dynamic language, like Python ((as compared to a static language)[https://pythonconquerstheuniverse.wordpress.com/2009/10/03/static-vs-dynamic-typing-of-programming-languages/]. Dynamic languages are far easier to write, but allow for some preventable errors that would be impossible in a static language. For am example from our Scheme, `(+ 'a' 1)` is valid syntax widly open to interpretation at rutime. If you are interested in building a typed language in Haskell, (this)[http://okmij.org/ftp/Haskell/AlgorithmsH.html#teval] guide shows how type inference make language engineering signficantly more complex if we want to make assertions on typed languaged 
We are also building an interpreter language, an alternative to compiling down to assembly language, (LLVM)[https://en.wikipedia.org/wiki/LLVM] or using a virtual machine like Java's JVM.  This tradeoff makes our language much easier to write, but our users will pay later with slower performance. Lisp In Small Pieces walks you through over 30 interpreted and 
2 compiled versions of Scheme, written in Scheme. You can find the program code (here)[https://pages.lip6.fr/Christian.Queinnec/WWW/LiSP.html]. If you want to write a language with high performance, you'll want to use LLVM. Right now Stephen Diehl is working on how you would create a (Haskell)[http://dev.stephendiehl.com/fun/) for yourself.    
Type systems are extremely complex to build, and balancing programming productivitiy versus performance gains is a difficult balance. For instance, Guy Steele has worked on successful languages like Common Lisp and Java, but spent most of the 8 years building Fortress getting the type system right. Btw, Steele cited issues with the complexity of the type system when winding down development on Fortress. Although type systems are complex, its theoretical possible to create a type system so advanced that programs can have provable properties and abstractions as powerful as those in mathematics. This is far beyond the scope of this tutorial, and the majority of production code written is done in a dynamic language. However, If your are a novice, then this tutorial is a great way to get involved in a very exciting movement that will shape the way of things to come for industry programming.  

## 1.1 Lisp and its lists
Lisp is a list processing language, and is old enough to join the AARP and has a very storied history. 
Scheme is an official Lisp implementation, although we will be taking many liberties for the sake of simplicity. 
For Lisp, means every expression is essentially a list. 
Each list, say `(+ 1 2)` is evaluated, and reduced. Whenever a list is encounter, it
is evaluated, and if it contains a functions, `apply` is used.
This doesn't always make sense, say `(1 2 3)` where encountered, evaluating
it would be non-sensical. For this reason, `quote` or `'` is used. Now if we evaluate
something like `'(1 2 3)` or `(quote (1 2 3))` we get `(1 2 3)`. Essentially, this just delays evaluation, 
and lets us use the Haskell list data structure for internal representation of data structures. (See Haskell wiki)

## 1.2 List manipulation
There are three primitive functions for manipulating these structures in our Scheme.
We will be implementing them later as part of the standard library and discussing the 
tradeoffs of other implementations. 
`car`  ... `(car '(1 2 3))` => `(1)`   
`cadr` ... `(cadr '(1 2 3))` => `(2 3)`    
`cons` ... `(cons 1 '(2 3))` => `(1 2 3)`    

## Stages of interpretation
User Inputs code and data in text format.    
Conversion of text files to manipulatable representation in Haskall. (Code == LispVal)         
Evaluate or "run" the internal structure. (Data == LispVal)    
Return the value to the user.  (some string)    
We will be using pure Haskell functions to do this, but also be adding functions that can read 
files from your computer or internet. Because the realworld is not immutable, we must account
for possible internal runtime errors. Your users will thank you later.

## 1.3 Internal representation, welcome to LispVal

When creating a programming language, we need a way to represent the structure of a program 
that can be manipulated within Haskell. Haskell has been gifted with a great type system that allows for pattern
matching on data constructors. 
When we go to evaluate our internal language structure, one function can be easily be dispatched different ways for the same type. 
The Haskell type we are going to use to represent lisp values is aptly named `LispVal`, they represent Lisp
S-Expressions, variable names, and primitive types like Number or String. 
Since Lisp represents code and data with the same structures, we can display code with the same functions we use to communicate the result of the comnputation. 
Alternatively, if we built a language like C, or C++ we would have to make two different objects, one for the code, 
and one for the data it returns. Another complication is symbol reuse between right-values and left-values.
This would make parsing `*ptr = memory_ptr + (sizeof(element)) * num_elemnts` or `&val = ptr & TRUE` valid syntax we 
would have to handle. 
Using a monadic evaluation strategy, we will build our language.  This is the first true "advanced" Haskell feature
we will be using, and if you want to learn how monadic evalutation works, read (this)[http://dev.stephendiehl.com/hask/#mtl-transformers] elegant explanation of monadic transformers. 
`data LispVal = DataConstructor InternalHaskellType` is the pattern used to define primitive
lisp values, like `Double` or `String`, the `List` data structure, {keywords like `lambda`
or `define`) and importantly variable or `Atom` names, like `x`.  
For our Scheme, a LispVal will represent a Scheme S-Expression before, and after evaluation. 
This simplifies implementation, and lets us do things like delay evaluation and allow 
users to define functions that can be evaluated into Scheme functions for later use, 
since data and code are represented using the same internal structure and Haskell type.
However, we need to be super careful about when things get evaluated and when they dont 
when environmental variables are involved. The benefit is extreme a concise evalutator
and  flexibility: programs come in as a LispVal, and results are returned as a LispVal. 
Structure Interpretation of Computer Programs is a great source for showing how this
approach can do complex tasks through a simple system.  Once we can reckon 
homoiconicity with the Haskell type system, the program and its result will always be the same type of object. 

To provide a basic development tool, `showVal` is a function that maps LispVals to 
Text string for showing the user internal values. The `show` typeclass is also 
derivided for `LispVal`, and will be an important tool when debugging the parser and displaying the computed result
to the user.

To be useful, `LispVal` must map from user inputted text to an abstract syntax tree(its internal representation). 
This process is called parsing,  and is the first source of `LispVal` types. The next source of `LispVal` types are 
those derived from other `LispVal`s, and are generated by `eval`. 
`LispVal`s also represent saved variables that can be recalled. 
This changing set of saved `LispVal`s is the environment, and a variables availability is called its `scope`.
One feature of our language will be lexical scoping, or the ability to use variable available at the time a function created that would otherwise be unavailable. 

When things go wrong (like the user tries to read a non-existent file) we evalutating `LispVal` gets tricky. To handle this a another data type, `LispError`, is generated to describe what went wrong. We will write a couple `LispError` and a formatting function that lets the user know what type of things went wrong, and give as much information as possible to allow for them to fix the problem. Again, `LispError` will be part of our monadic evaluator, which we will get to defining later. For now, just know we will have one data type to handle every possible part of interpretation. 


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


