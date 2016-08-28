Write You A Scheme, Version 2.0
------------

## Welcome
Welcome to Write You a Scheme, Version 2.0. You may be familar with the original
Write You a Scheme, and this tutorial is designed as an upgraded version to
reflect modern Haskell. The purpose of this blog series is to introduce languag
implementation in Haskell, and assumes only the most broad familiratiy with the
language. If you are looking for a good Haskell intro, may I suggest Learn You a
Haskell for Great Good. The following chapters are designed to be
self-contained, and an ambitious novice should be able to follow along. After
reading, you should have a pretty good grasp of the basic components of an
intrepreted language in Haskell.  Thus, you will have the knowledge required to
create your own domain specific or general purpose language.

## Why Lisp?
Although the majority of modern programming follows C/Algo style semantics, Lisp
syntax is much simpler, with code and data using the same syntax.  This is
called homoiconicity, and is one of the most beautiful features of the language. Scheme, a
dialect of lisp, is pretty straight forward. I won't scrictly follow the Scheme
standard for the sake of brevity.  For more on Lisp/Scheme, Structure and
Interpreation of Computer Programs is an essential computer science text, and
The Scheme Programming Language have both been very helpful. 



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
