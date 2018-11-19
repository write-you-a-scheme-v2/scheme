(begin
  (define foo (lambda (x) y))
  (let (y 42) (foo 0)))
