Parsing
------------

## Sauces
https://github.com/bobatkey/parser-combinators-intro    
http://unbui.lt/#!/post/haskell-parsec-basics    
http://dev.stephendiehl.com/hask/#parsing    
http://stackoverflow.com/questions/19208231/attoparsec-or-parsec-in-haskell/19213247#19213247

(User Input) -> (Internal Rep) -> (Evaluated Rep)
## Definitions & Key Concepts
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
Parsec is preferable compared to the alternatives: Alex & Happy or Attoparsec.
Alex & Happy are more complex, and require a seperate complition step. Parsec 
works well for most grammers, but is computationally expensive for left recursive grammers.
Attoparsec is faster, and preferable for parsing network messages or other binary 
formats. Parsec has better error messages, a helpful feature for programming languages

## How parsing will work
The parser will consume text, and return a `LispVal` that can be evaluated.     
Parsec defines parsers using,    
`newtype Parser a = Parser (String -> [(a,String)])`    
which shows a parser as a structure that consumes a String and returns some
result, the polymorphic `a`, and a string, the remaining text. 


parsing outline: Parser type, procession of consuming different possible matching sequences as implemented w/ monads,
try operator, how a grammers production rules can help unify lexical structure and the languages internal representation
via the parser. 
 
How to make a parser: 
list all possible lisp form(sexp, lambda, dotted list, let, quote)
match w/ parser fns

Tok.GenTokenParser


