;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         tprint.sch
; Description:  TPRINT benchmark from the Gabriel tests
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     19-Jul-85 19:05:26 (Bob Shaw)
;               23-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; TPRINT -- Benchmark to print and read to the terminal.
 
(define ttest-atoms '(abc1 cde2 efg3 ghi4 ijk5 klm6 mno7 opq8 qrs9
                          stu0 uvw1 wxy2 xyz3 123a 234b 345c 456d
                          567d 678e 789f 890g))
 
(define (init m n atoms)
(define (copy x)
   (if (pair? x)
       (cons (copy (car x)) (copy (cdr x)))
       x))
  (let ((atoms (copy atoms)))
    (do ((a atoms (cdr a)))
        ((null? (cdr a)) (set-cdr! a atoms) a))
    (init-aux m n atoms)))
 
(define (init-aux m n atoms)
  (cond ((= m 0) (car atoms))
        (else (do ((i n (- i 2))
                (a '()))
               ((< i 1) a)
             (set! a (cons (car atoms) a))
             (set! atoms (cdr atoms))
             (set! a (cons (init-aux (sub1 m) n atoms) a))))))
 
(define ttest-pattern (init 6 6 ttest-atoms))
 
;;; call:  (print ttest-pattern)
 
(run-benchmark "Tprint" (lambda () (write ttest-pattern)))
