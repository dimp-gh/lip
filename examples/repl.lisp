(define (repl)
  {(print (eval (read (input ">>> "))))
   (repl)})
(repl)
