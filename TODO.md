##TODO list
11/11/2016 (Goal: Write tutorial chapters 00 to 04)
- [x] Make sure resources from ./sources/* are added to introduction
- [ ] Balance intro/overview material between chapters
- [x] Figure out how Parser is constructing types around m_parens
- [x] Clean up Parser, remove uneeded variables decide if m_* or Tok.* should be used.
- [x] Replace String with Text in all LispError
- [x] Make LispError diversity and usage consistent throughout project
- [ ] Outline Eval chapters
- [ ] Write Eval Chapter
- [ ] Outline Prim chapters
- [ ] Write Prim Chapter



11/1/2016: Goal: Finish Code for initial release (chapters 0-4, enough code for REPL to work & turing completeness)
now that the parser is out of the way, we can focus on some fun stuff:    
get the following to work:  
- [x] eval w/ two args for: + - * ++ cons cadr quote    
- [x] eval w/ multiple args (fold version?)     
- [x] boolean,#t,#f,equals,not equals, if statement    
- [x] nested statements    
- [x] let statement    
- [x] lambda expressions    
- [x] begin function    
- [x] eval body to work when 2) let/lambda, support define statements    
- [x] io on files(read/write), read file to String, write string to file    
- [x] eval body to work when 1) reading files
- [ ] Test read/parse/eval files as script, evalBody like define    
**^^ leave this for later chapters ^^**    
- [x] write :: LispVal -> T.Text     
- [x] read  ::            T.Text -> LispVal     
