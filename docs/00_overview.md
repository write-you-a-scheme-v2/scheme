Write You A Scheme, Version 2.0
------------

## Welcome
Welcome to Write You a Scheme, Version 2.0. ([version 1](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/
https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf)) You may be familiar with the original Write You a Scheme, and this is a much needed upgraded version. We use as much modern, industry ready Haskell needed to implement a Scheme. This series will teach one how to create a programming language by walking the reader through the components of a Scheme variant Lisp in Haskell. This should take about a weekend of study/programming for a beginner who might have to look up a few new concepts to really internalize the material.

We won't have time to go into a lot of detail on things like monad transformers or interpreter theory. We will try to link to material that does provide further information. If you are looking for a good Haskell intro, we suggest [Learn You a Haskell for Great Good](http://learnyouahaskell.com) or [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask) for a more comprehensive guide. Specifically for industry, FP-Complete's [Haskell Syllabus](https://www.fpcomplete.com/haskell-syllabus) is a great guide.

This a community project, and if you have improvements, please contact me over Github or on Twitter [\@wespiser](http://www.twitter.com/wespiser) . We've had to balance language features, tutorial breadth, and level of complexity to provide beginners with a robust implementation they will not take long to understand. If you have any ideas on how I can better meet these goals, please let us know! We advocate open source languages, although it is possible to use our Scheme to build a vendor-specific language, [this](https://www.stickyminds.com/article/hey-vendors-give-us-real-scripting-languages?page=0%2C0) article spells out why that is a bad idea. However, if you need an interpreter for commercial purposes, [Abstract Definition Interpreters](../sources/AbstractDefinitionalInterpreters.pdf) contains the conceptual basis for much of this project.     

## Roadmap
Here's the overview of what we will be doing:

* [00_overview.md](../docs/00_overview.md) What you are reading right now.      
* [01_introduction.md](../docs/01_introduction.md)Introduces Scheme syntax and semantics, as well as the Haskell implementation.    
* [02_parsing.md](../docs/02_parsing.md) Parsing of text into abstract syntax tree.    
* [03_evaluation.md](../docs/03_evaluation.md) Evaluation of abstract syntax tree using monad transformers.       
* [04_errors.md](../docs/04_errors.md) Error messages used throughout project. Creation of error messages.    
* [05_primitives.md](../docs/05_primitives.md) Primitive functions that are loaded into the environment.    
* [06_repl.md](../docs/06_repl.md) Read Eval Print Loop.   
* [07_io.md](../docs/09_io.md) Reading and writing to files for both Scheme commands and the reading of program files.    
* [08_stdlib.md](../docs/10_stdlib.md) Creation of Scheme standard library from primitive functions.    
* [09_conclusion.md](../docs/11_conclusion.md) We conclude the project.        

## Why Lisp ?
Although the majority of modern programming follows C/Algo, Lisp syntax is simple &mdash; using the same syntax to represent code and data &mdash; the list. This is called homoiconicity, a feature which makes both parsing and evaluation much simpler compared to other languages. Scheme, a dialect of Lisp, is a straight-forward Lisp. I won't strictly follow the Scheme standard for the sake of brevity, but aim to include as many useful features as possible. If you are interested in what a fully fledged Scheme looks like, Haskell-Scheme (github) is a fully featured language in Haskell, and Chicken-Scheme implements in C. The scheme we will write will contain the basic elements of these more featured languages. The Scheme Programming Language is a great book to teach the function of an industrial strength reference Scheme, and is a great source to explain language features. Another great book to learn Scheme and interpretation is Structure and Interpretation of Computer Programs (SICP), which discusses how to build a meta-circular evaluator in Lisp.

While on the topic of Lisp, Clojure Programming Language is a modern, functional approach to Lisp targeting the JVM. If you are interested in designing your own Lisp, Clojure is a great example of how you can take adopt Lisp to the modern programming environment. It is probably the most supported modern Lisp community and is a great source of information and ideas.  

## Why Haskell ?
First off, why not? I love Haskell! Haskell is a purely functional, typed, compiled, and lazy language with many sophisticated and advanced features. With over two decades of research level and industrial development, Haskell has been the focus of numerous computer science research projects and implements these ideas with a production worthy compiler. Who cares? Well, these features give the programmer expressive power above and beyond today's status quo. From its test bed has emerged a set of very powerful features. With great power, comes great responsibility, and there many complex and under-adopted features that take a while to learn. Fortunately, if you only use a subset of advanced features, Haskell can be an effective production language. It may be attractive to include advanced type theory extensions and over engineer your project with all the latest advances in type theory. If your background is in academic Haskell research or you have been hacking around in Haskell for years, there is no problem understanding, or learning how. If you are trying to hire developers to work on your project with you or finding people who can be trained with less cost, then the performance gains will be a serious hinderance when proposing your project to weary business type folks. For this reason, we take a conservative approach on what features and abstract concepts are used in this Scheme. When in doubt, we pick the simplest possible abstraction that is needed for functionality and avoid extra language pragma and type theory extensions at all cost. If you use Haskell for work, your colleagues' job will go a lot easier and less technical debt will emerge.  

## Project Tool Chain

What you need to run the project is in `Readme.md`, and if you are excited to start, skip to Chapter 1!

Haskell is not an island unto itself, and we must manage the libraries required to build the project. I recommend using Ubuntu, version 14.04 or 16.04, (any Linux distribution should work, contact me if you have problems) and the build tool Stack. The library versions are determined for Stack in `scheme.cabal`, while `stack.yaml` is the version of Stack's dependency resolver, and `Build.hs` is Haskell code for generating documents from these markdown files. More info on stack, here: https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal. The `Readme.md` contains full instructions on how to build the project source code and documentation, which I encourage you to do. The best way to learn is to modify, break, fix, and finally improve the source code. Two included scripts, `build` which will monitor for file changes then build upon updates, and `run`, which will drop you into an interactive REPL, were invaluable in the development of this project. Please feel free to contact with me with any great ideas, modifications, improvements, or vaguely related but interesting concepts. I made this project for you so use it however you please.


#### Next, Introduction to our implementing Scheme
[next](01_introduction.md)
