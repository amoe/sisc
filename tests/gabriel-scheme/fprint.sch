;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         fprint.sch
; Description:  FPRINT benchmark
; Author:       Richard Gabriel
; Created:      11-Apr-85
; Modified:     9-Jul-85 21:11:33 (Bob Shaw)
;               24-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FPRINT -- Benchmark to print to a file.

(define test-atoms '(abcdef12 cdefgh23 efghij34 ghijkl45 ijklmn56 klmnop67
                     mnopqr78 opqrst89 qrstuv90 stuvwx01 uvwxyz12
                     wxyzab23 xyzabc34 123456ab 234567bc 345678cd
                     456789de 567890ef 678901fg 789012gh 890123hi)
  )

(define (init-aux m n atoms)
  (cond ((= m 0) (car atoms))
        (else (do ((i n (- i 2))
                   (a '()))
                  ((< i 1) a)
                  (set! a (cons (car atoms) a))
                  (set! atoms (cdr atoms))
                  (set! a (cons (init-aux (- m 1) n atoms) a))))))

(define (init m n atoms)
  (define (copy x)
    (if (pair? x)
        (cons (copy (car x)) (copy (cdr x)))
        x))
  (let ((atoms (copy atoms)))
    (do ((a atoms (cdr a)))
        ((null? (cdr a)) (set-cdr! a atoms)))
    (init-aux m n atoms)))

(define test-pattern (init 6 6 test-atoms))

(define (fprint)
  (call-with-output-file
   "fprint.tst"
   (lambda (stream)
     (newline stream)
     (write test-pattern stream))))

;;; call:  (fprint)

(run-benchmark "Fprint" (lambda () (fprint)))
