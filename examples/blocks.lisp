{
  (define (sqr x) (* x x))
  (define n 15)
  (let (square (sqr n))
    {
      (print "Square of " n "is")
      (print square)
    }
  )
}
