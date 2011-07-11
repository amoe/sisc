;;;
;;; You need to write a procedure named "run-benchmark" that takes
;;; two arguments.  The first is a string identifying the particular
;;; benchmark being run.  The second is a thunk, a procedure of no
;;; arguments, that will actually run the benchmark.
;;;

(define (sub1 x) (- x 1))
(define (add1 x) (+ x 1))

(define pi (string->number "3.1415926536"))

(define (system-time) (java.lang.System.currentTimeMillis))

(define (time n thunk)
  (let loop ((x n) (res '()))
    (if (zero? x)
        (list (apply min res) 'min 'ms)
        (let ((st (system-time)))
          (begin (thunk)
                 (loop (- x 1) (cons (- (system-time) st) res)))))))

(define benchmark-results '())

(define run-benchmark 
  (lambda (benchmark-name benchmark-thunk)
    (display benchmark-name) (display "...")
    (let ((benchresult (cons benchmark-name 
                             (time 3 benchmark-thunk))))
      (set! benchmark-results (cons benchresult benchmark-results)))
    (newline)))
