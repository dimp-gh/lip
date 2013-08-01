(block
  (define factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1))))))
  (let (res (factorial 10))
    (print "Factorial of 10 is " res)))
  
