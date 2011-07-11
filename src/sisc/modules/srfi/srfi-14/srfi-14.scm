; Native implementation of Character Sets
; using Records (SRFI-9) for the distinct type,
; and SRFI-60 for operations on bignums as a bitvector
; Assumes a unicode capable underlying Scheme system.
;

;;; If CS is really a char-set, do CHAR-SET:S, otw report an error msg on
;;; behalf of our caller, PROC. This procedure exists basically to provide
;;; explicit error-checking & reporting.

(define (%char-set:s/check cs proc)
  (let lp ((cs cs))
    (if (char-set? cs) (char-set:s cs)
    (lp (error "Not a char-set:" proc cs)))))

;;; Parse, type-check & default a final optional BASE-CS parameter from
;;; a rest argument. Return a *fresh copy* of the underlying string.
;;; The default is the empty set. The PROC argument is to help us
;;; generate informative error exceptions.

(define (%default-base maybe-base proc)
  (if (pair? maybe-base)
      (let ((bcs  (car maybe-base))
            (tail (cdr maybe-base)))
    (if (null? tail)
        (if (char-set? bcs)
                  (char-set-copy bcs)
                  (error "BASE-CS parameter not a char-set" proc bcs))
        (error "Expected final base char set -- too many parameters"
    	   proc maybe-base)))
      (char-set-copy char-set:empty)))

(define (char-set-copy c1)
  (make-char-set (%char-set:s/check c1 char-set-copy)))

(define (pairwise-compare op ls proc)
  (or (null? ls)
      (let ([rest (cdr ls)])
        (or (null? rest)
            (and (op (%char-set:s/check (car ls) proc)
                     (%char-set:s/check (car rest) proc))
                 (pairwise-compare op rest proc))))))

(define (char-set= . args)
  (pairwise-compare = args char-set=))

(define (char-set<= . args)
  (pairwise-compare (lambda (x y)
                      (= 0 (logand x (logxor x y))))
                    args char-set<=))

(define (char-set-hash cs . bound)
  (let ([bound (if (null? bound)
                   (- HASH-BOUNDS 1)
                   (car bound))])
    (modulo (%char-set:s/check cs char-set-hash) bound)))

(define (char-set-cursor cset)
  (make-char-set-cursor (%char-set:s/check cset char-set-cursor) 0))

(define (lowest-bit-set cset cursor proc)
  (let ([v (logand (lognot (char-set-cursor-mask cursor))
                   (%char-set:s/check cset proc))])
    (- (bit-count (logxor (- v 1) v)) 1)))
         
(define (char-set-ref cset cursor)
  (if (end-of-char-set? cursor)
      (error 'char-set-ref "cursor already reached end of character set.")
      (integer->char (lowest-bit-set cset cursor char-set-ref))))

(define (generate-n-bits n)
  (- (ash 1 n) 1))

(define (char-set-cursor-next cset cursor)
  ; Unset the lowest bit and shift left, this shrinks the mask
  (let ([lbs (lowest-bit-set cset cursor char-set-cursor-next)]
        [mask (char-set-cursor-mask cursor)])
    ; This could be done faster
    (char-set-cursor-mask! cursor
                           (generate-n-bits (+ 1 lbs)))))

(define (end-of-char-set? cursor)
  (> (char-set-cursor-mask cursor)
     (char-set-cursor-cset cursor)))

;;; -- for-each map fold unfold every any

(define (char-set-fold kons knil cs)
  (let ((cursor (char-set-cursor cs)))
    (let loop ((knil knil))
      (if (end-of-char-set? cursor)
          knil
          (let ([v (kons (char-set-ref cs cursor) knil)])
            (char-set-cursor-next cs cursor)
            (loop v))))))

(define (%set1! s code-point)
  ; Silently ignore all chars above the MAX-CODEPOINT
  (char-set:s! s (logior (char-set:s s)
                         (ash 1 code-point))))

(define (%unset1! s code-point)
  (char-set:s! s (logand (char-set:s s)
                         (lognot (ash 1 code-point)))))

(define (%char-set-unfold! proc p f g s seed)
  (check-arg procedure? p proc)
  (check-arg procedure? f proc)
  (check-arg procedure? g proc)
  (let lp ((seed seed))
    (cond ((not (p seed))			; P says we are done.
       (%set1! s (%char->code-point (f seed)))	; Add (F SEED) to set.
       (lp (g seed))))))			; Loop on (G SEED).

(define (char-set-unfold p f g seed . maybe-base)
  (let ((bs (%default-base maybe-base char-set-unfold)))
    (%char-set-unfold! char-set-unfold p f g bs seed)
    (make-char-set bs)))

(define (char-set-unfold! p f g seed base-cset)
  (%char-set-unfold! char-set-unfold! p f g
    	     (%char-set:s/check base-cset char-set-unfold!)
    	     seed)
  base-cset)

;;; -- for-each map fold unfold every any

(define (char-set-for-each proc cs)
  (check-arg procedure? proc char-set-for-each)
  (let ((s (%char-set:s/check cs char-set-for-each)))
    (char-set-fold (lambda (c k)
                     (proc c))
                   #f s)))

(define (char-set-map proc cs)
  (check-arg procedure? proc char-set-map)
  (let ((s (%char-set:s/check cs char-set-map)))
    (char-set-fold (lambda (c cs)
                     (char-set-adjoin! cs c)
                     cs)
                   (char-set-copy char-set:empty)
                   s)))

(define (char-set . chars)
  (list->char-set chars))

(define (list->char-set char-list . base)
  (let ([rs (%default-base base list->char-set)])
    (list->char-set! char-list rs)))

(define (list->char-set! char-list rs)
  (apply char-set-adjoin! rs char-list))

(define (string->char-set s . base-cs)
  (apply list->char-set (string->list s) base-cs))

(define (string->char-set! s base-cs)
  (list->char-set! (string->list s) base-cs))

(define (char-set-filter pred cs . base)
  (let ([rs (%default-base base list->char-set)])
    (char-set-filter! pred cs rs)))

(define (char-set-filter! pred cs base)
  (char-set-for-each
   (lambda (char)
     (if (pred char)
         (char-set-adjoin! base char)))
   cs)
  base)

(define (ucs-range->char-set lower upper . args)
  (let ([error? (if (null? args)
                    #f
                    (car args))]
        [rs (if (null? args) (char-set-copy char-set:empty)
                (%default-base (cdr args) list->char-set))])
    (ucs-range->char-set! lower upper error? rs)))


(define (ucs-range->char-set! lower upper error? base)
  (char-set:s! base (ash (generate-n-bits (- upper lower))
                         lower)))

(define (char-set-adjoin rs . chars)
  (apply char-set-adjoin! (char-set-copy rs) chars))

(define (char-set-adjoin! rs . chars)
  (char-set:s! rs (apply logior
                         (%char-set:s/check rs char-set-adjoin!)
                         (map (lambda (c)
                                (ash 1 (%char->code-point c)))
                              chars)))
  rs)

(define (char-set-delete rs . chars)
  (apply char-set-delete! (char-set-copy rs) chars))

(define (char-set-delete! rs . chars)
  (for-each (lambda (c)
              (%unset1! rs (%char->code-point c)))
            chars)
  rs)

(define (->char-set x)
  (cond [(char-set? x) x]
        [(string? x)
         (string->char-set x)]
        [(char? x)
         (char-set-adjoin char-set:empty x)]
        [else (error '->char-set "Cannot coerce to char-set: " x)]))

(define (char-set-size cs)
  (bit-count (%char-set:s/check cs char-set-size)))

(define (char-set-count pred cs)
  (char-set-fold (lambda (char knil)
                   (if (pred char)
                       (+ knil 1)
                       knil))
                 0 cs))

(define (char-set->list cs)
  (char-set-fold cons '() cs))

(define (char-set->string cs)
  (list->string (char-set->list cs)))

(define (char-set-contains? cs char)
  (logtest (%char-set:s/check cs char-set-contains?)
           (ash 1 (%char->code-point char))))

(define (char-set-every pred cs)
  (call/cc (lambda (escape)
             (char-set-for-each (lambda (char)
                                  (if (not (pred char))
                                      (escape #f)))
                                cs)
             #t)))

(define (char-set-any pred cs)
  (call/cc (lambda (escape)
             (char-set-for-each (lambda (char)
                                  (if (pred char)
                                      (escape #t)))
                                cs)
             #f)))

(define (char-set-complement cs)
  (make-char-set (lognot (%char-set:s/check cs char-set-complement))))
(define (char-set-union  . args)
  (make-char-set (apply logior (map (lambda (a)
                                      (%char-set:s/check a char-set-union))
                                    args))))
(define (char-set-intersection . args)
  (make-char-set (apply logand (map (lambda (a)
                                      (%char-set:s/check a char-set-intersection)) args))))
(define (char-set-difference first . rest)
  (make-char-set (logand (%char-set:s/check first char-set-difference)
                         (lognot (apply logior
                                        (map (lambda (a)
                                               (%char-set:s/check a char-set-difference))
                                             rest))))))

(define (char-set-xor . args)
  (make-char-set (apply logxor (map (lambda (a)
                                      (%char-set:s/check a char-set-xor))
                                    args))))

(define (char-set-diff+intersection first . rest)
  (values (apply char-set-difference first rest)
          (char-set-intersection first (apply char-set-union rest))))

(define (char-set-complement! cs)
  (char-set:s! cs (lognot (%char-set:s/check cs char-set-complement!))))
(define (char-set-union!  cs . args)
  (char-set:s! cs (apply logior (map (lambda (a)
                                      (%char-set:s/check a char-set-union!))
                                     (cons cs args)))))
(define (char-set-intersection! cs . args)
  (char-set:s! cs (apply logand (map (lambda (a)
                                      (%char-set:s/check a char-set-intersection!)) 
                                     (cons cs args)))))
(define (char-set-difference! first . rest)
  (char-set:s! first (logand (%char-set:s/check first char-set-difference!)
                         (lognot (apply logior (map (lambda (a)
                                                      (%char-set:s/check a char-set-difference!))
                                                    rest))))))

(define (char-set-xor! cs . args)
  (char-set:s! cs (apply logxor (map (lambda (a)
                                      (%char-set:s/check a char-set-xor!)) 
                                     (cons cs args)))))

(define (char-set-diff+intersection! first . rest)
  (values (apply char-set-difference! first rest)
          (char-set-intersection! first (apply char-set-union! rest))))


