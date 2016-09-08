Write You A Scheme, Version 2.0
------------

## Welcome
Welcome to Write You a Scheme, Version 2.0. You may be familar with the original
Write You a Scheme, and this tutorial is designed as an upgraded version to
reflect modern Haskell. This series will teach readers how
to create a programming language by walking the reader
through the components of a Scheme Lisp in Haskell. This should take about a 
weekend of study/programming for a beginner who might have to look up a few new
concepts. I won't have time to go into a lot of detail on things like monad transformer
or interpreter theory, so f you are looking for a good Haskell intro, may I suggest Learn You a
Haskell for Great Good or Stephen Diehls "What I Wish I Knew When Learning Haskell" 
for a more comprehensive guide. I want this to be a community project, so if you have improvements, please contact me over
github or on twitter (@wespiser). Ive had to balance language features, tutorial breadth, and level of complexity to
provide beginners with a robust implementation they will not take long to understand, if you have any ideas on how I can
better meet these goals, please let me know!

## Why Lisp ?
Although the majority of modern programming follows C/Algo style semantics, Lisp
syntax is much simpler, using the same syntax to represent code and data, the list.  This is called homoiconicity, one of the most beautiful features of the language. Scheme, a
dialect of Lisp, is an especially straight-forward lisp.. I won't scrictly follow the Scheme
standard for the sake of brevity, but aim to make a Scheme you can use for Structure and
Interpreation of Computer Programs. This is an incredible text, and well worth a read
for the interested language developer or enthusiast at any level of experience. 
If you are interested in what a fully fledged Scheme looks like, Haskell-Scheme (github) is a fully featured language,
in Haskell, and Chicken-Scheme implements in C. The scheme we will write will contain the basic elements of these more
featured languages. 
The Scheme Programming Language is a great book to teach the function of an industrial strength reference Scheme, and is
a great source to explain language features.


While on the topic of Lisp, Clojure Programming Language is a modern, functional approach to Lisp targetting the JVM.
If you are interested in designing your own Lisp, Clojure is a great example of how you can take adopt Lisp to the
modern programming environment. It is probably the most supported modern Lisp community, 
and is a great source of information and ideas.  

## Why Haskell ?
First off, why not? Haskell is a purely functional, typed, compiled, and lazy language with
many sophisticated features. With over two decades of development, Haskell has
been the focus of numerous computer science research projects and implements these ideas w/ a
production worthy compiler. These features give the programmer expressive power above and beyond todays status quo, and
is a test bed for the features that will make tomorrow's language so much better than todays.
With great power, comes great responsiblity, and there many complex and under-adopted features that take a while to
learn. Fortunately, there is only a subset of advanced features that are really needed to make Haskell an effective
production language. 
It may be attractive to include advanced type 
theory extensions and over engineer your project with all the latest advances in type theory. If your background is in
academic haskell research, there is no problem understanding, or learning how. If you are tring to hire developers to
work on your project with you, finding people who can be trained with less cost than the performance gains will be a
challenge. For this reason, I take a conservative approach on what features and abstract concepts are used in this
Scheme. If you use Haskell for work, your colleagues job will go a lot easier and less technical debt will emerge.  




