## TODO
create repl code foreach feature
craft pedantic narrative

### Objective
To create an updated version of Write Yourself a Scheme in 48 hours.    
http://www.akazlou.com/posts/2015-11-09-every-project-should-have-decisions.html

### Transformer Stack ReaderT EnvCtx (ExpectT  LispError IO) LispVal
EnvCtx is primative environment/lexical scope    
LispError is for in-language errror throwing, example (addThreeNums "adam" 42),
should inform the user three nums are needed    
IO is for reading/writing files of data and source code. DO NOT use this for
wonkiness like arbitrarily catching expcetions. Keep things pure.

### LispVal
union of (set of parsing text via Parsec) and (evaluation of LispVals w/
context)
includes handling of lambda functions along with their lexical scope    
apply fn for (primatives, IO functions, LispVal Lambdas)

### Prim Env
1) basic operators (add/sub/div/mul etc)
2) IO operators 
3) Lisp-specific things like cons/cadr

### Evaluation
lexical scoping via transformer stack on let/lambda    
apply :: Eval (lambda [LispVal Atoms] [SExpr body]) ->  [LispVals] -> Eval
LispVal

### Things to avoid
IORef --done
Read typeclass -- used in number
call/cc and continuations? -- none
complexity of implementation/language -- ask about this in review
