;;;
;;; You need to write a procedure named "run-benchmark" that takes
;;; two arguments.  The first is a string identifying the particular
;;; benchmark being run.  The second is a thunk, a procedure of no
;;; arguments, that will actually run the benchmark.
;;;

(define (sub1 x) (- x 1))
(define (add1 x) (+ x 1))

(define-syntax get
  (syntax-rules ()
    ((_ x y) (getprop x y))))

(define-syntax put
  (syntax-rules ()
    ((_ x y z) (putprop x y z))))

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
                             (with/fc (lambda (m e) 'failed)
                               (lambda () (time 3 benchmark-thunk))))))
      (set! benchmark-results (cons benchresult benchmark-results)))
    (newline)))
