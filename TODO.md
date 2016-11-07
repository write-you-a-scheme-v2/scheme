##TODO list
11/1/2016    
now that the parser is out of the way, we can focus on some fun stuff:    
get the following to work:  
  --eval w/ two args for: + - * ++ cons cadr quote    
  --eval w/ multiple args (fold version?)     
  --boolean,#t,#f,equals,not equals, if statement    
  --nested statements    
  --let statement    
  --lambda expressions    
  --begin function    
  --eval body to work when 2) let/lambda, support define statements    
  --io on files(read/write), read file to String, write string to file    
  --eval body to work when 1) reading files 
  Test read/parse/eval files as script, evalBody like define
  write :: LispVal -> T.Text     
  read  ::            T.Text -> LispVal     
