(define y (lambda (w) (lambda (f) (f (lambda (x) ((w w) f) x )))  ))
(define fix1 (y y))
(define fact (lambda (n) 
               (let (nNew n) 
                 (if (== nNew 0) 
                      1 
                      (* nNew (fact (- nNew 1))) ) )))
(define meta-fact 
  (lambda (fm) 
    (lambda (nm)  
      (if (== nm 0) 
          1 
          (* nm (fm (- nm 1)) ) ) )))

(define fix 
  (let (d (lambda (w) 
            (lambda (f) 
              (f (lambda (x) (((w w) f) x ))) ) ))
    (d d) ) )

--((fix fact) 2) -- get this to work
((meta-fact fact) 7)
