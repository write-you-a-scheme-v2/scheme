(define (not x)            (if x #f #t))
(define (null? obj)        (if (eql? obj '()) #t #f))
