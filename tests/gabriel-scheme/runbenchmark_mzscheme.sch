;;;
;;; You need to write a procedure named "run-benchmark" that takes
;;; two arguments.  The first is a string identifying the particular
;;; benchmark being run.  The second is a thunk, a procedure of no
;;; arguments, that will actually run the benchmark.
;;;

(define proplist (make-hash-table))

(define (get-first-tier x)
 (hash-table-get proplist x 
  (lambda () (hash-table-put! proplist x (make-hash-table))
             (hash-table-get proplist x))))

(define-syntax get
  (syntax-rules ()
    ((_ x y) (hash-table-get (get-first-tier x) y))))

(define-syntax put
  (syntax-rules ()
    ((_ x y z) (hash-table-put! (get-first-tier x) y z))))

(define (time n thunk)
  (let loop ((x n) (res '()))
    (if (zero? x)
        (list (apply min res) 'min 'ms)
        (let ((st (current-milliseconds)))
          (begin (thunk)
                 (loop (- x 1) (cons (- (current-milliseconds) st) res)))))))

(define benchmark-results '())

(define run-benchmark 
  (lambda (benchmark-name benchmark-thunk)
    (display benchmark-name) (display "...")
    (let ((benchresult (cons benchmark-name 
                             (with-handlers ([not-break-exn? (lambda (e) 
                                                              'failed)])
                               (time 3 benchmark-thunk)))))
      (set! benchmark-results (cons benchresult benchmark-results)))
    (newline)))
