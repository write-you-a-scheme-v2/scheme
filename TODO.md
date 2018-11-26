##TODO list

11/23/2018 Compile Scheme to LLVM
- [ ] Solidify semantics, design decisions for compilation
- [ ] Add ANF recursive func (let rec) 
- [x] Add ANF If/else branching in ANF
- [ ] LispValue -> ExprReduced converstion via AST
- [ ] research LLVM implementation of ANF forms
- [ ] type checking/inference step on LispVal
- [ ] variable capture/substitution for non-unique names


1/3/2016 Goal: End game
- [x] Figure out recursive functions (evalArg? why isn't fold working")
- [x] Tests for lexical scoping 
- [x] Figure out stdlib integration into exec
- [x] Write standard library chapter
- [x] Write testing chapter 
- [x] Add functions for io manipulation (readFile, execute file, writeToFile,
  etc)
- [x] Write IO chapter

11/11/2016 (Goal: Write tutorial chapters 00 to 04)
- [x] Make sure resources from ./sources/* are added to introduction
- [x] Balance intro/overview material between chapters
- [x] Figure out how Parser is constructing types around m_parens
- [x] Clean up Parser, remove uneeded variables decide if m_* or Tok.* should be used.
- [x] Replace String with Text in all LispError
- [x] Make LispError diversity and usage consistent throughout project
- [x] Outline Eval chapters
- [x] Write Eval Chapter
- [x] check lambda lexical scoping (in both EnvCtx in Lambda and partial applyLambda)
- [x] Outline Prim chapters
- [x] Write Prim Chapter
- [x] Write Error Chapter
- [x] Integrate test framework into narrative to demonstrate features of presented code.
- [x] Find an alternative to the ExceptT error IO a


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
- [x] Test read/parse/eval files as script   
- [x] evalBody like define    
- [x] write :: LispVal -> T.Text     
- [x] read  ::            T.Text -> LispVal     
