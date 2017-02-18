(define Y
  (lambda (f)
    ((lambda (x) (f (lambda () (x x))))
     (lambda (x) (f (lambda () (x x)))))))

(define ffact
  (lambda (f)
    (lambda (n)
      (if (eq? n 0)
        1
        (* n ((f) (- n 1)))))))

((Y ffact) 7)

