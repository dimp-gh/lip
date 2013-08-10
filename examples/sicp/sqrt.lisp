;; Square root example from the first chapter of SICP

;; abs defined through cond
(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (square x) (* x x))

;; newton method of finding square roots
(define (sqrt-iter guess x)
  (if (good-enough? guess x) ;; if guess fits well
      guess ;; then return it
    (sqrt-iter (improve guess x) x))) ;; otherwise improve guess and try again

(define (average a b) ;; average of two numbers
  (/ (+ b a) 2))

(define (improve guess e) ;; improving guess is
  (average guess (/ x guess))) ;; taking an average of guess and x / guess

(define (good-enough? guess q) ;; testing how good square root guess fits
  (< (abs (- (square guess) q)) 0.001)) ;; by squaring it

(define (sqrt x)
  (sqrt-iter 1.0 x))

(print (sqrt 2))
