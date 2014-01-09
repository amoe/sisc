(define-nongenerative-struct promise
  sisc.srfi.srfi-45.promise-type
  (expr eager?))

(define-simple-syntax (lazy exp)
  (box (box (make-promise (lambda () exp) #f))))

(define (eager x)
  (box (box (make-promise x #t))))

(define-simple-syntax (delay exp)
  (lazy (eager exp)))

(define (force promise)
  (let* ((content (unbox (unbox promise)))
         (expr (promise-expr content)))
    (if (promise-eager? content)
        expr
        (let* ((promise* (expr))
               (content  (unbox (unbox promise))))
          (when (not (promise-eager? content))
            (set-box! (unbox promise) (unbox (unbox promise*)))
            (set-box! promise* (unbox promise)))
          (force promise)))))
