---
title: Write You A Scheme, Version 2.0
date: November 28, 2016
author: Adam Wespiser
---

##Write You A Scheme, Version 2.0
> A programming language is for thinking about programs, not for expressing programs you've already thought of. It should be a pencil, not a pen **Paul Graham**    


## Welcome
Welcome to Write You a Scheme, Version 2.0. ([version 1](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/
https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf)) You may be familiar with the original Write You a Scheme, and this is a much needed upgraded version.  We use as much modern, industry ready Haskell to implement a Scheme on its way to being ready for production.  This series will teach how to create a programming language by walking the reader through the components of a Scheme variant Lisp in Haskell.  This should take about a weekend of study/programming for a beginner who might have to look up a few new concepts to really internalize the material.   The ideal reader with have some experience in Haskell, and eager to see how all the pieces come together in a medium to large sized project.

We won't have time to go into a lot of detail on things like monad transformers or interpreter theory. Instead, links to further information will be provided when needed. If you are looking for a good Haskell intro, the concepts from [Learn You a Haskell for Great Good](http://learnyouahaskell.com) should be internalized. A great reference while reading this book is [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask). Specifically for industry, FP-Complete's [Haskell Syllabus](https://www.fpcomplete.com/haskell-syllabus) is a great guide. Finally, the [HaskellWiki](https://wiki.haskell.org/Learning_Haskell) has a comprehensive list of resources available for learning Haskell.    

This a community project, and if you have improvements, please contact me over Github or on Twitter [\@wespiser](http://www.twitter.com/wespiser). Better yet, submit a PR! We've had to balance language features, tutorial breadth, and level of complexity to provide beginners with a robust implementation that will not take long to understand.  If you have any ideas on how I can better meet these goals, please let us know! We advocate open source languages, although it is possible to use our Scheme to build a vendor-specific language, [this](https://www.stickyminds.com/article/hey-vendors-give-us-real-scripting-languages?page=0%2C0) article spells out why that is a bad idea. However, if you need an interpreter for commercial purposes, [Abstract Definition Interpreters](../sources/AbstractDefinitionalInterpreters.pdf) contains the conceptual underpinnings and "invents" the kind of interpreter we will be making.         

## Roadmap
Here's the overview of what we will be doing and where we will go:    

* [00_overview.md](../docs/00_overview.md) What you are reading right now.      
* [01_introduction.md](../docs/01_introduction.md) Introduces Scheme syntax and semantics, as well as the Haskell implementation.    
* [02_parsing.md](../docs/02_parsing.md) Parsing of text into abstract syntax tree.    
* [03_evaluation.md](../docs/03_evaluation.md) Evaluation of abstract syntax tree using monad transformers.       
* [04_errors.md](../docs/04_errors.md) Error messages used throughout project. Creation of error messages.    
* [05_primitives.md](../docs/05_primitives.md) Primitive functions that are loaded into the environment.    
* [06_repl.md](../docs/06_repl.md) Read Eval Print Loop.   
* [07_io.md](../docs/09_io.md) Reading and writing to files for both Scheme commands and the reading of program files.    
* [08_stdlib.md](../docs/10_stdlib.md) Creation of Scheme standard library from primitive functions.    
* [09_conclusion.md](../docs/11_conclusion.md) We conclude the project.        

## Why Lisp ?
Although the majority of modern programming follows C/Algo, Lisp syntax is simple, using the same syntax to represent code and data, the list.  This is called homoiconicity, a feature which makes both parsing and evaluation much simpler compared to other languages. Scheme, a dialect of Lisp, is a straight-forward lisp.  We won't strictly follow the Scheme standard for the sake of brevity, but aim to include as many useful features as possible.  If you are interested in what a fully fledged Scheme looks like, Haskell-Scheme (github) is a fully featured language, in Haskell, and Chicken-Scheme implements in C. The scheme we will write will contain the basic elements of these more featured languages. The Scheme Programming Language is a great book to teach the function of an industrial strength reference Scheme, and is a great source to explain language features. Another great book to learn Scheme and interpretation is Structure and Interpretation of Computer Programs (SICP), which discusses how to build a meta-circular evaluator in Lisp.  Lisp has a history as an educational language, and its simplicity is mostly why we will be following the tradition here.    

While on the topic of Lisp, Clojure Programming Language is a modern, functional approach to Lisp targeting the JVM. If you are interested in designing your own Lisp, Clojure is a great example of how you can take adopt Lisp to the modern programming environment. It is probably the most supported modern Lisp community,  and is a great source of information and ideas.  The other industry relevant Lisp is EmacsLisp, which is a Domain Specific Language for programmers that don't know vim!  

## Why Haskell ?
First off, why not? I love Haskell! Haskell is a purely functional, typed, compiled, and lazy language with many sophisticated and advanced features. With over two decades of research level and industrial development, Haskell has been the focus of numerous computer science research projects and implements these ideas with a production worthy compiler. Who cares? Well, these features give the programmer expressive power above and beyond todays status quo. From its test bed has emerged a set of very powerful and safe abstractions. With great power, comes great responsibility, and there are many complex and under-adopted features that take a while to learn. Fortunately, if you only use a subset of advanced features, Haskell can be an effective production language. It may be attractive to include advanced type theory extensions and over engineer your project with all the latest advances in type theory. If your background is in academic Haskell research or you have been hacking around in Haskell for years, there is no problem understanding, or learning how. If you are trying to hire developers to
work on your project with you, finding people who can be trained with less cost than the performance gains will be a serious hinderance when proposing your project to weary business type folks. For this reason, we take a conservative approach on what features and abstract concepts are used in this Scheme. When in doubt, we pick the simplest possible abstraction that is needed for functionality, and avoid extra language pragma and type theory extensions at all cost. If you use Haskell for work, your colleagues job will go a lot easier and less technical debt will emerge.  

## Project Tool Chain

What you need to run the project is in `Readme.md`, and if you are excited to start, skip to Chapter 1!

Haskell is not an island unto itself, and we must manage the libraries required to build the project. I recommend using Ubuntu, version 14.04 or 16.04, (any Linux distribution should work, contact me if you have problems) and the build tool Stack. The library versions are determined for Stack in `scheme.cabal`, while `stack.yaml` is the version of Stack's dependency resolver, and `Build.hs` is Haskell code for generating documents from these markdown files. More info on stack, here: https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal. The `Readme.md` contains full instructions on how to build the project source code and documentation, which I encourage you to do. The best way to learn is to modify, break, fix, and finally improve the source code. Two included scripts, `build` which will monitor for file changes then build upon updates, and `run`, which will drop you into an interactive REPL, were invaluable in the development of this project. Please feel free to contact with me with any great ideas, modifications, improvements, or vaguely related but interesting concepts. I made this project for you, use it however you please.



## What are we going to do?
We are going to make a very basic, but robust, programming language both simple to use and highly extendible. You are encouraged to take the language we build and add more functionality. For instance, you could use this language for running jobs on a High Performance Computing Cluster, or analyzing financial data. Language development is really a "killer app" for Haskell, and the approach we take in this tutorial is the basis you'll need to create a domain specific language for industrial purposes.    

## Scheme Syntax
Lisp is a list processing language, and is old enough to join the AARP with a very storied history.  Scheme is an fully specified Lisp implementation, although we will be taking many liberties for the sake of simplicity.  For Scheme, this means every expression is a list. This parenthesized list has a prefix operator and is known as an S-Expression. Whenever a S-Expression is encountered, it is evaluated in the same way, minus a handful of special forms. Data is also represented as an S-Expression, and there is no syntactical difference between code and data.  Scheme is well known for this minimalism.

## Scheme semantics
Similar to Haskell, Scheme is a functional programming language. Everything in our Scheme in an object, for instance numbers, strings, functions, variables, and booleans. This is our Haskell type, `LispVal`.  These objects are stored in a single environment, `EnvCtx` which is queried to resolve the value of an evaluated variable. In Scheme, no object is ever destroyed, however, the scope of a variable is limited to its lexical context.  Further, arguments in scheme are passed by value, so when a function is called, all of its arguments are evaluated before being passed into the function. Contrast this to Haskell, where evaluation is lazy, and values are not computed until they are needed, which is sometimes never. The environment to find and store variables is the same environment is used to find primitive functions, so when `(file? "tmp")` is evaluated, `file?` is a variable with a corresponding value (a function) in the environment.

## Scheme Type System   
Scheme is a dynamic language, like Python, Ruby, Perl, or [as compared to a static language](https://pythonconquerstheuniverse.wordpress.com/2009/10/03/static-vs-dynamic-typing-of-programming-languages/) like C++ or Java. Dynamic languages are simpler to implement, but allow for some preventable errors that would be impossible in a static language.  For an example from our Scheme, `(+ 'a' 1)` is valid syntax wildly open to interpretation at runtime.  (Try it on the REPL!) If you are interested in building a typed language in Haskell, [this](http://okmij.org/ftp/Haskell/AlgorithmsH.html#teval) guide shows how type inference makes language engineering significantly more complex.  Dynamic languages are not all doom and gloom, they give the user tremendous flexibility.  The R Programming Language is an excellent example of a dynamic language that excels at statistical computing by giving the user incredible flexibility and choice over how to implement ideas.  A concept called Dynamic Dispatch allows functions to be determined, at runtime, by the types of the arguments passed in, so `(+ 1 1)` and `(+ "a" "b")` could use different versions of the `+` function.  This is a key feature is dynamically typed programming languages, and we will be implementing this feature in our Scheme.    

## Interpreted
We are building an interpreted language, an alternative to compiling down to assembly language, LLVM or using a virtual machine like Java's JVM.  Most generally this means that we have a program that actively runs to evaluate a program in our Scheme.  This approach yields slower performance due to the higher memory and processor overhead, but we will be able to finish in the project in a single weekend.  For the motivated, Lisp In Small Pieces walks you through over 30 interpreted and  2 compiled versions of Scheme, written in Scheme.  You can find the program code  [here](https://pages.lip6.fr/Christian.Queinnec/WWW/LiSP.html). If you want to write a language with performance in mind, you'll want to use an [LLVM backend](http://stephendiehl.com/llvm).  I warn you, there be dragons!    

#### On Type System Complexity, Cautionary Tail
Type systems are extremely complex to build, and balancing programming productivity versus performance gains is a difficult balance.  For instance, Guy Steele has worked on successful languages like Common Lisp and Java, but spent most of the 8 years building Fortress getting the type system right.  Steele cited issues with the complexity of the type system when winding down development on Fortress.  Although type systems are complex, its theoretical possible to create a type system so advanced that programs can have provable properties and abstractions as powerful as those in mathematics.  This is far beyond the scope of this tutorial, and the majority of production code written is done in a dynamic language. However, If your are a novice, then this tutorial is a great way to get involved in a very exciting movement that will shape the way of things to come for industry programming.   For now, the best we get is an industrial language that if it compiles, it runs.      

## Scheme Examples
To get a feel for our Scheme, here is the evaluation of some functions and their arguments.  Keep in mind that we must build the abstracts that are capable of evaluating these forms.  Both the right and left hand side of the form are represented with `LispVal`.    

**List Processing**    

There are three primitive functions for manipulating these structures in our Scheme.  We will be implementing them later as part of the standard library and discussing the tradeoffs of other implementations.    
`car`  ... `(car '(1 2 3))`  => `(1)`    
`cadr` ... `(cadr '(1 2 3))` => `(2 3)`     
`cons` ... `(cons 1 '(2 3))` => `(1 2 3)`    

**Mathematics**     
Mathematical functions can take 2 or more functions.    
`(* 60 9)` => `69`    
`(+ 10 30 2))` => `42`    

**Quote**    
`quote` is a special form that delays evaluation on its argument.    
`(quote (1 2 3 4))` => `(1 2 3 4)`    
`'(1 2 3 4)` => `(1 2 3 4)`    

**Conditional Statements**    
The `if` statement acts like it does in any language.    
`(if (< 4 5) #f 42)` => `#f`    

**Lambdas & Anonymous functions**    
`lambda` is used to create an anonymous function.    
`((lambda (y) (+ y 2)) 40)` => `42`    

**Let Statements**    
`let` takes two arguments. Its first is a paired list of variables and values. These variables are set the to corresponding values, which are then in scope for the evaluation of the second argument.    
`(let (x 42) x)` => `42`    
`(let (x 2 y 40) (+ x y))` => `42`    

**Begin**    
`begin` evaluates a series of one or more S-Expressions in order.  S-Expressions can modify the environment using `define`, then subsequent expressions may access to the variable.  Further, when running a Scheme program, its S-Expressions are essentially wrapped in a single begin function.  More on this when we go over [Eval.hs](../src/Eval.hs).     
`(begin (define x 413000) (define y (+ x 281)) (+ x y))` => `413281`    

**The Rest**    

Although Scheme is a minimal language, this list of functions is not complete.  There are two files that contain the rest of the internally defined functions: special forms in [Eval.hs](../src/Eval.hs), and the primitives in [Prim.hs](src/Prim.hs). For a full Scheme specification, see [R5RS](../sources/r5rs.pdf). It's not the most modern, but its complete enough to work.    

#### [Understanding Check]
What form does Scheme use to represent data? what about code?    
How would you create a function in Scheme? How about set a variable?    
If Scheme is a Dynamically-Typed Interpreted Functional Language? What does this make C, or your favorite programming language?    
Can you rearrange `let` expressions into `lambda`? What about `lambda` into `let`?    
Write out an explanation and example that demonstrates lexical scope using a `lambda` expression.

#### Next, Introduction to our implementing Scheme
[next](01_introduction.md)
