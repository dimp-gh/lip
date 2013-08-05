{
  (define (sum list)
    (if (= #nil list)
	0
        (+ (car list) (sum (cdr list)))))
  (print "sum of list is" (sum (list 1 2 3 4 5)))
}
