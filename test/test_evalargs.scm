(define x (+ (+ 1 2) (let (x 222 y 333) (+ x y)) ((lambda (x) (+ 0 x)) 1000)))
x
