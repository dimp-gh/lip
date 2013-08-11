(define (sqr x) (* x x))

(define (cube x) (* x x x))

(define (double x)
  (if (= x 0)
      0
    (+ x x))) 

(print (sqr 15))
