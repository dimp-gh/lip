(+ (* 3
      (+ (* 2 4)
	 (+ 3 5)))
   (+ (- 10 7)
      6))

(define size 2)

(define pi 3.14159)
(define radius 10)

(* pi (* radius radius))

(define circumference (* 2 pi radius))

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(print (f 5))

(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
	(else x)))

(define (abs x)
  (if (< x 0)
      (- x)
    x))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
    y))

(define (sqrt-iter guess e)
  (if (good-enough? guess e)
      guess
    (sqrt-iter (improve guess e) e)))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess e)
  (average guess (/ e guess)))

(define (good-enough? guess q)
  (< (abs (- (square guess) q)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(print "sqrt of 15 is " (sqrt 15))
