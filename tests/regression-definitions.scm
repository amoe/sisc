(module m (a b)
  (module ((a x))
   (define x 10)
   (define (a) x))
  (module ((b x))
   (define x 20)
   (define (b) x)))
   
(define intvalue 1)
