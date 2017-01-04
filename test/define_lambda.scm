(define app (lambda (arg1 arg2) (++ arg1 arg2)))
(define appendWord (lambda (arg) (app arg "talk")))
(appendWord "small")
