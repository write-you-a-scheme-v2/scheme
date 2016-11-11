Parsing
------------
## What is Parsing?
**lexeme** the basic lexical unit of meaning.        
**token** structure representing a lexeme that explicitly indicates its categorization for the purpose of parsing.    
**lexer** A algorithm for lexical analysis that separates a stream of text into its component lexemes.  Defines the rules for individual words, or allowed symbols in a programming language.     
**parser** an algorithm for converting the lexemes into valid language grammar. Operates on the level above the lexer, and defines the grammatical rules.
![image](../img/WYAS-Lisp-Interpreter-Steps.png)    
Most basically, Parsing and Lexing is the process of converting the input text of either the REPL or script and converting that into a format that can be evaluated by the interpreter. That coverted format, in our case, is `LispVal`. The library we will use for parsing is called `Parsec`.

## About Parsec
Parsec is a monadic parser, and works by matching streaming text to lexeme then parsing that into an abstract syntax via data constructors. Thus, for text input, the lexemes, or units of text that define a language feature, are converted a `LispVal` structure. These lexemes are individually defined via Parsec, and wholly define the valid lexical structure of our Scheme.

## Why Parsec?
Parsec is preferable for its simplicity compared to the alternatives: Alex & Happy or Attoparsec. Alex & Happy are more complex, and require a separate compilation step. Parsec works well for most grammars, but is computationally expensive for left recursive grammars. Attoparsec is faster, and preferable for parsing network messages or other binary formats. Parsec has better error messages, a helpful feature for programming languages. If our language required a lot of left-recursive parsing, Alex & Happy would probably be a better choice. However, the simplicity and minimalism of Scheme syntax makes parsing relatively simple.


## How parsing will work
The parser will consume text, and return a `LispVal` that can be evaluated. Parsec defines parsers using,    
```Haskell
newtype Parser LispVal = Parser (Text -> [(LispVal,Text)])
```    
Thus, a Parser is a type consisting of a function that 1) takes some `Text` and 2) return a `LispVal` and some `Text`


## Parser.hs imports
```Haskell
import LispVal
import Text.Parsec
import Text.Parsec.Text
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang
import qualified Data.Text as T
import Control.Applicative hiding ((<|>))
import Data.Functor.Identity (Identity)
```
Good for us, Parsec is available with `Text`, not just `String`. If you are looking online for examples of Parsec, make sure you have the correct encoding of strings.


## Tokenizer
```Haskell
style
lexer
Tok.TokenParser
```
## ParseLispVals

## ParseExpr

## Putting it all together





#### Next, Evaluation
[back](01_introduction.md)[next](03_evaluation.md)




## Sauces
https://github.com/bobatkey/parser-combinators-intro    
http://unbui.lt/#!/post/haskell-parsec-basics    
http://dev.stephendiehl.com/hask/#parsing    
http://stackoverflow.com/questions/19208231/attoparsec-or-parsec-in-haskell/19213247#19213247
