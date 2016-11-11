Introduction: The Bolts and Nuts of Scheme Interpreters in Haskell
------------
## 1.0 What are we going to do?
We are going to make a very basic, but robust, programming language that is both simple to use and highly extendible. I encourage you to take the language we build and add functionality is useful to purposes important to you. For instance, you could use this language useful for running jobs on a High Performance Computing Cluster, or analyzing financial data. I want to see more Haskell in industry, and language development is where we can really make a difference, today, not tomorrow.
## 1.1 Scheme Syntax
Lisp is a list processing language, and is old enough to join the AARP and has a very storied history.  Scheme is an official Lisp implementation, although we will be taking many liberties for the sake of simplicity.  For Lisp, means every expression is essentially a list. This parenthesized list has a prefix operator and is known as an S-Expression. Whenever a S-Expression is encounter, it is evaluated in the same way, minus a handful of special forms. That's it, Scheme is known for its minimalism, and the S-Expression to represent both code and data is the pinnacle of minimal form.
## Scheme semantics
Similar to Haskell, Scheme is a functional programming language. Everything in our Scheme in an object, for instance numbers, strings, functions, variables, and booleans. Scheme  This is our Haskell type, `LispVal`. These objects are stored in a single environment, which is queried to resolve the value of an evaluated variable. In Scheme, no object is ever destroyed, however, the scope of a variable is limited to its lexical context. Further, arguments in scheme are passed by value, so when a function is called, all of its arguments are evaluated before being passed into the function. Contrast Haskell, which is a lazy language, our Scheme is strict in its evaluation.
## Scheme Type System   
Scheme is a dynamic language, like Python, Ruby, Perl, or (as compared to a static language)[https://pythonconquerstheuniverse.wordpress.com/2009/10/03/static-vs-dynamic-typing-of-programming-languages/]. Dynamic languages are simpler to implement, but allow for some preventable errors that would be impossible in a static language. For an example from our Scheme, `(+ 'a' 1)` is valid syntax widely open to interpretation at runtime.(Try it on the REPL!) If you are interested in building a typed language in Haskell, (this)[http://okmij.org/ftp/Haskell/AlgorithmsH.html#teval] guide shows how type inference makes language engineering significantly more complex.
#### Interpreted
We are building an interpreted language, an alternative to compiling down to assembly language, (LLVM)[https://en.wikipedia.org/wiki/LLVM] or using a virtual machine like Java's JVM. Most generally this means that we have a program that actively runs to evaluate a program in our Scheme. This approach yields slower performance due to the higher overhead, but we will be able to finish in the project in a single weekend. For the motivated, Lisp In Small Pieces walks you through over 30 interpreted and  2 compiled versions of Scheme, written in Scheme. You can find the program code (here)[https://pages.lip6.fr/Christian.Queinnec/WWW/LiSP.html]. If you want to write a language with performance in mind, you'll want to use LLVM. Right now Stephen Diehl is working on how you would create a (Haskell)[http://dev.stephendiehl.com/fun/) for yourself, if you are interested in writing a functional programming language that compiles to LLVM.
#### Blah on Type System Complexity, Cautionary Tail
(AW Note: Should I bounce this????)
Type systems are extremely complex to build, and balancing programming productivity versus performance gains is a difficult balance. For instance, Guy Steele has worked on successful languages like Common Lisp and Java, but spent most of the 8 years building Fortress getting the type system right. Btw, Steele cited issues with the complexity of the type system when winding down development on Fortress. Although type systems are complex, its theoretical possible to create a type system so advanced that programs can have provable properties and abstractions as powerful as those in mathematics. This is far beyond the scope of this tutorial, and the majority of production code written is done in a dynamic language. However, If your are a novice, then this tutorial is a great way to get involved in a very exciting movement that will shape the way of things to come for industry programming.  

## Scheme Examples
To get a feel for our Scheme, here is the evaluation of some functions and their arguments. Keep in mind that we must build the abstracts that are capable of evaluating these forms. Both the right and left hand side of the form are represented with `LispVal`.    
#### list processing
There are three primitive functions for manipulating these structures in our Scheme. We will be implementing them later as part of the standard library and discussing the tradeoffs of other implementations.
`car`  ... `(car '(1 2 3))`  => `(1)`   
`cadr` ... `(cadr '(1 2 3))` => `(2 3)`    
`cons` ... `(cons 1 '(2 3))` => `(1 2 3)`    
#### math
Mathematical functions can take 2 or more functions.
`(* 10 2 3)` => `60`    
`(+ 1 2 3))` => `6`    
#### quote
`quote` is a special form that delays evaluation on its argument.
`(quote (1 2 3 4))` => `(1 2 3 4)`    
`'(1 2 3 4)` => `(1 2 3 4)`    
#### if
The `if` statement acts like it does in any language.
`(if (< 4 5) #f 42)` => `#f`    
#### lambda
`lambda` is used to create an anonymous function.
`((lambda (y) (+ y 2)) 40)` => `42`    
#### let
`let` takes two arguments. Its first is a paired list of variables and values. These variables are set the to corresponding values, which are then in scope for the evaluation of the second argument.
`(let (x 42) x)` => `42`   
`(let (x 2 y 40) (+ x y))` => `42`
#### begin
`begin` is an important function. It takes 1 or more S-Expressions and evaluates them subsequently. This allows for one expression to modify the environment using `define`, then the next to take advantage of this. Further, when running a Scheme script, the script is essentially wrapped in a single begin function.    
`(begin (define x 413000) (define y (+ x 281)) (+ x y))` => `413281`
#### the rest
Although Scheme is a minimal language, this list is not complete. There are two files that contain the rest of the internally defined functions: special forms in `src/Eval.hs`, and the primitives in `src/Prim.hs`. For a full Scheme specification, see [R5RS](sources/r5rs.pdf). It's not the most modern, but its complete enough to work.

#### [Understanding Check]
What form does Scheme use to represent data? what about code?
How would you create a function in Scheme? How about set a variable?
If Scheme is a Dynamically-Typed Interpreted Functional Language? What does this make C, or your favorite programming language?


## How are we going to do this?
[](img/WYAS-Lisp-Interpreter-Steps.png)
To make any programming language, we must take user inputed text, turn that text into
tokens, parse that into an abstract syntax tree, then evaluate that format into a result. Fortunately, we can use the same structure, `LispVal`, for both the abstract syntax tree, returned by the parser, and the return result of the interpreter. Homoiconicity for the win! Fortunately, the lexer and parser is contained in a single library, Parsec, which does most of the work for us. Once we have parsed into LispVal, we have to evaluate that `LispVal` to get the result of the computation. Evaluation must be done for all the different configurations of S-Expressions, including specials forms like `begin` and `define`. During that computation we need to have an environment for keeping track of bound variable, an IO monad for reading or writing files during execution, and Except monad for throwing/catching different errors. We will also need a way to convert Haskell functions to internal Scheme functions, and a collection of these functions stored in the Scheme environment. Finally, a suitable user interface, including Read/Evaluate/Print/Loop, way to run read files/run scripts, and standard library of functions loaded at runtime defined in Scheme is needed.    
This may seem like a lot. But don't worry, all these things, and more, are already available in this project. Together, we'll go through line by line and make sense out of how these Haskell abstraction coalesce to implement a Scheme!



## Project Road Map: What do we have?
[](img/WYAS-Dependency-Tree.png)
####Main.hs    
Handles the creation of the binary executable, parsing of command line options
####Repl.hs
Read Evaluate Print Loop code.
####Parser.hs    
Lexer and Parser using Parsec code. Responsibility for the creation of LispVal object from Text input.
####Eval.hs      
Contains the evaluation function, `eval`. Patten matches on all configurations of S-Expressions and computes the resultant `LispVal`.
####LispVal.hs    
defines LispVal, evaluation monad, LispError, and report printing functions.
####Prims.hs
Creates the primitive environment functions, which themselves are contained in a map. These primitive functions are Haskell functions mapped to `LispVal`s.
####Pretty.hs    
Pretty Printer, used for formatting error messages. Might drop this, what do you think?



## Internal representation, welcome to LispVal

We need a way to represent the structure of a program that can be manipulated within Haskell. Haskell's type system that allows for pattern matching on data constructors, which will allow our `eval` function to differentiate different forms of S-Expressions.

All objects in our Scheme are of Haskell type `LispVal`
http://dev.stephendiehl.com/hask/#mtl-transformers]



## Define LispVal


## Next, Parsers!
