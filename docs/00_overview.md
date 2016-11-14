Write You A Scheme, Version 2.0
------------

## Welcome
Welcome to Write You a Scheme, Version 2.0. ([version 1](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/
https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf)) You may be familiar with the original Write You a Scheme, and this tutorial is designed as an upgraded version to reflect modern Haskell. It uses as much modern, industry ready Haskell needed to implement a Scheme. This series will teach readers how to create a programming language by walking the reader through the components of a Scheme variant Lisp in Haskell. This should take about a weekend of study/programming for a beginner who might have to look up a few new concepts to really internalize the material. I won't have time to go into a lot of detail on things like monad transformers or interpreter theory. I will try to link to material that does provide further information. If you are looking for a good Haskell intro, may I suggest Learn You a Haskell for Great Good or Stephen Diehls "What I Wish I Knew When Learning Haskell"  for a more comprehensive guide, or "Learn You A Haskell For Great Good". I want this to be a community project, if you have improvements, please contact me over Github or on twitter (@wespiser). I've had to balance language features, tutorial breadth, and level of complexity to provide beginners with a robust implementation they will not take long to understand.  If you have any ideas on how I can better meet these goals, please let me know! I am an advocate of open source languages, although it is possible to use our Scheme to build a vendor-specific language, [this](https://www.stickyminds.com/article/hey-vendors-give-us-real-scripting-languages?page=0%2C0) article spells out why that is a bad idea. However, If you need an interpreter for commercial purposes, [Abstract Definition Interpreters](../sources/AbstractDefinitionalInterpreters.pdf) contains the conceptual basis for much of this project.     

## Roadmap
If want to get right to business, start with the next chapter, and work your way through. If not, finish this chapter for more context.      
[00_overview.md](../docs/00_overview.md)     
[01_introduction.md](../docs/01_introduction.md)     
[02_parsing.md](../docs/02_parsing.md)     
[03_evaluation.md](../docs/03_evaluation.md)     
[04_errors.md](../docs/04_errors.md)     
[05_primitives.md](../docs/05_primitives.md)     
[06_repl.md](../docs/06_repl.md)     
[07_mutation.md](../docs/07_mutation.md)     
[08_closures.md](../docs/08_closures.md)     
[09_io.md](../docs/09_io.md)     
[10_stdlib.md](../docs/10_stdlib.md)     
[11_conclusion.md](../docs/11_conclusion.md)     


## Why Lisp ?
Although the majority of modern programming follows C/Algo, Lisp syntax is simple, using the same syntax to represent code and data, the list.  This is called homoiconicity, a feature which makes both parsing and evaluation much simpler compared to other languages. Scheme, a dialect of Lisp, is a straight-forward lisp.  I won't strictly follow the Scheme standard for the sake of brevity, but aim to include as many useful features as possible. If you are interested in what a fully fledged Scheme looks like, Haskell-Scheme (github) is a fully featured language, in Haskell, and Chicken-Scheme implements in C. The scheme we will write will contain the basic elements of these more featured languages. The Scheme Programming Language is a great book to teach the function of an industrial strength reference Scheme, and is a great source to explain language features. Another great book to learn Scheme and interpretation is Structure and Interpretation of Computer Programs (SICP), which discusses how to build a meta-circular evaluator in lisp.

While on the topic of Lisp, Clojure Programming Language is a modern, functional approach to Lisp targeting the JVM. If you are interested in designing your own Lisp, Clojure is a great example of how you can take adopt Lisp to the modern programming environment. It is probably the most supported modern Lisp community,  and is a great source of information and ideas.  

## Why Haskell ?
First off, why not? I love Haskell! Haskell is a purely functional, typed, compiled, and lazy language with many sophisticated and advanced features. With over two decades of research level and industrial development, Haskell has been the focus of numerous computer science research projects and implements these ideas w/ a production worthy compiler. Who cares? Well, these features give the programmer expressive power above and beyond todays status quo. From its test bed has emerged a set of very powerful features. With great power, comes great responsibility, and there many complex and under-adopted features that take a while to learn. Fortunately, if you only use a subset of advanced features Haskell can be an effective production language. It may be attractive to include advanced type theory extensions and over engineer your project with all the latest advances in type theory. If your background is in academic Haskell research or you have been hacking around in Haskell of years, there is no problem understanding, or learning how. If you are trying to hire developers to
work on your project with you, finding people who can be trained with less cost than the performance gains will be a serious hinderance when proposing your project to weary business type folks. For this reason, I take a conservative approach on what features and abstract concepts are used in this Scheme. When in doubt, I pick the simplest possible abstraction that is needed for functionality, and avoid extra language pragma and type theory extensions at all cost. If you use Haskell for work, your colleagues job will go a lot easier and less technical debt will emerge.  

## Project Tool Chain
What you need to run the project is in `Readme.md`, and if you are excited to start, skip to chapter 1!     
Haskell is not an island unto itself, and we must manage the libraries required to build the project. I recommend using Ubuntu, version 14.04 or 16.04, (any Linux distribution should work, contact me if you have problems) and the build tool Stack. The library versions are determined for Stack in `scheme.cabal`, while `stack.yaml` is the version of Stack's dependency resolver, and `Build.hs` is Haskell code for generating documents from these markdown files. More info on stack, here: https://www.fpcomplete.com/blog/2015/06/why-is-stack-not-cabal. The `Readme.md` contains full instructions on how to build the project source code and documentation, which I encourage you to do. The best way to learn is to modify, break, fix, and finally improve the source code. Two included scripts, `build` which will monitor for file changes then build upon updates, and `run`, which will drop you into an interactive REPL, were invaluable in the development of this project. Please feel free to contact with me with any great ideas, modifications, improvements, or vaguely related but interesting concepts. I made this project for you, use it however you please.


#### Next, Introduction to our implementing Scheme
[next](01_introduction.md)
