(begin
  (define y 1)
  (define add (lambda (x) (+ x y)))
  (let (y 10) (add y)))
