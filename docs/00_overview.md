Write You A Scheme, Version 2.0
------------

## Welcome
Welcome to Write You a Scheme, Version 2.0. You may be familar with the original
Write You a Scheme, and this tutorial is designed as an upgraded version to
reflect modern Haskell. The purpose of this blog series is to teach readers how
to create a programming language and approaches the problem by walking the reader
through the process of creating a Scheme. This should take about a 
weekend of study/programming for a beginner who might have to look up a few new
concepts. If you are looking for a good Haskell intro, may I suggest Learn You a
Haskell for Great Good or Stephen Diehls "What I Wish I Knew When Learning Haskell" 
for a more comprehensive guide. If you have improvements, please contact me over
github or on twitter (@wespiser). Ive had to balance language features, tutorial breadth, and level of complexity to
provide beginners with a robust implementation they will not take long to understand, if you have any ideas on how I can
better meet these goals, please let me know!

## Why Lisp ?
Although the majority of modern programming follows C/Algo style semantics, Lisp
syntax is much simpler, with code and data using the same representation, a List.  This is called homoiconicity, and is one of the most beautiful features of the language. Scheme, a
dialect of Lisp, is an especially straight-forward lisp. I won't scrictly follow the Scheme
standard for the sake of brevity, but aim to make a Scheme you can use for Structure and
Interpreation of Computer Programs. This is an incredible text, and well worth a read
for the interested language developer or enthusiast. 
If you are interested in what a fully fledged Scheme looks like, Haskell-Scheme (github) is a fully featured language,
in Haskell, and Chicken-Scheme implements in C. 
The Scheme Programming Language is a great book to teach the function of an industrial strength reference Scheme, and is
a great source to explain language features.


While on the topic of Lisp, Clojure Programming Language is a modern, functional approach to Lisp targetting the JVM.
If you are interested in designing your own Lisp, Clojure is a great example of how you can take adopt Lisp to the
modern programming environment. It is probably the most supported modern Lisp community, 
and is a great source of information and ideas. 

## Why Haskell ?
First off, why not? Haskell is a purely functional, typed, compiled, and lazy language with
many sophisticated features. With over two decades of development, Haskell has
been the focus of numerous computer science white papers and implements these ideas w/ a
production worthy compiler . Haskell offers many features that give the programmer expressive power above and beyond
todays status quo. With great power, comes great responsiblity, and there many complex and under-adopted features of
Haskell that professional software engineers will struggle before understanding. 
When using Haskell in production, it may be attractive to include advanced type 
theory extensions and over engineer your project with all the latest advances in type theory. If your background is in
academic haskell research, there is no problem understanding, or learning how. If you are tring to hire developers to
work on your project with you, finding people who can be trained with less cost than the performance gains will be a
challenge. For this reason, I take a conservative approach on what features and abstract concepts are used in this
Scheme. If you use Haskell for work, your colleagues job will go a lot easier and less technical debt will emerge.  



