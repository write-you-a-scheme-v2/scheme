Parsing
------------

## Sauces
https://github.com/bobatkey/parser-combinators-intro    
http://unbui.lt/#!/post/haskell-parsec-basics    
http://dev.stephendiehl.com/hask/#parsing    


## Definitions & Key Concepts
lexer,parser,lexeme,token,`LispVal`,Parsec,(Applicative,Monad -- move these to
intro?)
lexeme - the basic lexical unit of meaning(wikipedia)     
token - structure representing a lexeme that explicitly indicates its
categorization for the purpose of parsing (wikipedia)
Lexer - A algorithm for lexical analysis that seperates a stream of text into
its component lexemes.  Defines the rules for individual words, or allowed
symbols in a programming language.     
parser - An algorithm for converting the lexemes into valid language grammer.
Operates on the level above the lexer, and defines the grammatical rules.       

## About Parsec
Parsec is a monadic parser, and works by matching streaming text to abstract
syntax data constructors. Thus, for text input, the lexemes, or units of text
that define a language feature, are converted a `lispVal` structure. These
lexemes are individually defined via Parsec, and wholy define the valid lexical
structure of our lisp. 

## Why Parsec?
Parsec is simple compared to the alternatives: Alex & Happy or Attoparsec.
Alex & Happy are more complex, and require a seperate complition step. Parsec 
works well for most grammers, but is computationally expensive for left recursive grammers.   

