;;test dynamic wind.

(define results '())
(define (collate obj)
  (set! results (cons obj results)))
(define (display-results)
  (for-each (lambda (x) (display x) (newline)) (reverse results)))

(define (f g h)
  (dynamic-wind
   (lambda () (collate 'b))
   (lambda ()
     (collate 'x)
     (dynamic-wind
      (lambda () (collate '-b))
      (lambda ()
	(let ((k #f))
	  (collate '-x)
	  (dynamic-wind
	   (lambda () (collate '--b1))
	   (lambda ()
	     (collate '--x1)
	     (g)
	     (collate '--y1))
	   (lambda () (collate '--a1)))
	  (dynamic-wind
	   (lambda () (collate '--b2))
	   (lambda ()
	     (collate '--x2)
	     (h)
	     (collate '--y2))
	   (lambda () (collate '--a2)))
	  (collate '-y)))
      (lambda () (collate '-a)))
     (collate 'y))
   (lambda () (collate 'a))))

(define (t0)
  (f (lambda () #f)
     (lambda () #f)))

(define (t1)
  (let ((k #f))
    (f (lambda () (call/cc (lambda (kk) (set! k kk))))
       (lambda () (if k (let ((kk k)) (set! k #f) (kk)))))))

(define (t2)
  (with/fc (lambda (m e) #f)
    (lambda () (f (lambda () (/ 1 0))
                  (lambda () #f)))))

;;the thing to watch out for is that b and a must always appear in
;;pairs and without interleaving unless the level (indicated by -)
;;deepens.
(set! results '())
(t0)
(display-results)
(set! results '())
(t1)
(display-results)
(set! results '())
(t2)
(display-results)

