; SISC implementation of SRFI-60.  Uses the logicops module 
; as a base
(define (logical:ash-4 x)
  (if (negative? x)
      (+ -1 (ashr (+ 1 x) 4))
      (ashr x 4)))
;@
(define (logtest n1 n2)
  (not (zero? (logand n1 n2))))
;@
(define (logbit? index n)
  (logtest (expt 2 index) n))
;@
(define (copy-bit index to bool)
  (if bool
      (logior to (arithmetic-shift 1 index))
      (logand to (lognot (arithmetic-shift 1 index)))))
;@
(define (bitwise-if mask n0 n1)
  (logior (logand mask n0)
          (logand (lognot mask) n1)))
;@
(define (bit-field n start end)
  (logand (lognot (arithmetic-shift -1 (- end start)))
          (arithmetic-shift n (- start))))
;@
(define (copy-bit-field to from start end)
  (bitwise-if (arithmetic-shift (lognot (arithmetic-shift -1 (- end start))) start)
              (arithmetic-shift from start)
              to))
;@
(define (rotate-bit-field n count start end)
  (let ((width (- end start)))
    (let ((count (modulo count width))
      (mask (lognot (arithmetic-shift -1 width))))
      (let ((zn (logand mask (arithmetic-shift n (- start)))))
    (logior (arithmetic-shift
    	 (logior (logand mask (arithmetic-shift zn count))
    		 (arithmetic-shift zn (- count width)))
    	 start)
    	(logand (lognot (arithmetic-shift mask start)) n))))))

;@
(define integer-length
  (letrec ((intlen (lambda (n tot)
                     (case n
                       ((0 -1) (+ 0 tot))
                       ((1 -2) (+ 1 tot))
                       ((2 3 -3 -4) (+ 2 tot))
                       ((4 5 6 7 -5 -6 -7 -8) (+ 3 tot))
                       (else (intlen (logical:ash-4 n) (+ 4 tot)))))))
    (lambda (n) (intlen n 0))))
;@
(define (log2-binary-factors n)
  (+ -1 (integer-length (logand n (- n)))))

(define (bit-reverse k n)
  (do ((m (if (negative? n) (lognot n) n) (arithmetic-shift m -1))
       (k (+ -1 k) (+ -1 k))
       (rvs 0 (logior (arithmetic-shift rvs 1) (logand 1 m))))
      ((negative? k) (if (negative? n) (lognot rvs) rvs))))
;@
(define (reverse-bit-field n start end)
  (define width (- end start))
  (let ((mask (lognot (arithmetic-shift -1 width))))
    (define zn (logand mask (arithmetic-shift n (- start))))
    (logior (arithmetic-shift (bit-reverse width zn) start)
            (logand (lognot (arithmetic-shift mask start)) n))))
;@
(define (integer->list k . len)
  (if (null? len)
      (do ((k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((<= k 0) lst))
      (do ((idx (+ -1 (car len)) (+ -1 idx))
           (k k (arithmetic-shift k -1))
           (lst '() (cons (odd? k) lst)))
          ((negative? idx) lst))))
;@
(define (list->integer bools)
  (do ((bs bools (cdr bs))
       (acc 0 (+ acc acc (if (car bs) 1 0))))
      ((null? bs) acc)))
(define (booleans->integer . bools)
  (list->integer bools))

(define (arithmetic-shift i n)
  (if (>= n 0)
      (ashl i n)
      (ashr i (abs n))))




