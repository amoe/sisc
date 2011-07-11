;;; array as-procedure
;;; 1997 - 2001 Jussi Piitulainen

;;; Records are not standard in Scheme. An array representation consists of
;;; three fields: backing vector, index mapping, and shape vector.

;;; The type predicate is approximate.

(define (array:make vec idx shp)
  (cons (lambda (token)
          (case token
            ((vec) vec)
            ((idx) idx)
            ((shp) shp)))
        'array))

(define (array:vector array)
   ((car array) 'vec))

(define (array:index array)
   ((car array) 'idx))

(define (array:shape array)
  ((car array) 'shp))

(define (array:array? obj)
  (and (pair? obj)
       (procedure? (car obj))
       (eq? (cdr obj) 'array)))
