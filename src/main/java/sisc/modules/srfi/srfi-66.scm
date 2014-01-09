;; SISC implementation of SRFI-66: Octet Vectors
;; built on SISC's buffer library.
;;
;; 2006 Scott G. Miller

(define u8vector? buffer?)
(define make-u8vector make-buffer)
(define u8vector buffer)
(define u8vector-length buffer-length)
(define u8vector-ref buffer-ref)
(define u8vector-set! buffer-set!)
(define u8vector-compare buffer-compare)
(define list->u8vector buffer)
(define u8vector-copy! buffer-copy!)

(define (u8vector->list u8vector)
  (do ([i (- (u8vector-length u8vector) 1)
          (- i 1)]
       [acc '() (cons (u8vector-ref u8vector i) acc)])
      ((< i 0) acc)))

(define (u8vector=? v1 v2)
  (zero? (u8vector-compare v1 v2)))

(define (u8vector-copy v)
  (let ([newv (make-u8vector (u8vector-length v))])
    (u8vector-copy! v 0 newv 0 (u8vector-length newv))
    newv))
