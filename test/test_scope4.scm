(begin
  (define y 42)
  (define foo (lambda (x) x))
  (let (y 12) (foo y)))
