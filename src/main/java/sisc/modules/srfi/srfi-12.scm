;; SRFI-12
;;
;;This isn't fully compliant yet because internal exceptions do not
;;raise conditions as demanded by this SRFI's spec.

(define (current-exception-handler)
  (let ([cfc (call/fc (fk) fk)])
    (lambda (exn) (call/cc (lambda (k) (cfc exn k))))))

(define (with-exception-handler handler thunk)
  (with/fc 
      (lambda (m e)
        (if (string? m)
            (set! m (make-property-condition
                     'exn
                     'message m
                     'error-continuation e
                     'parent-failure-continuation
                     (call/fc (fk) fk))))
        (handler m))
    thunk))

(define-syntax handle-exceptions
  (lambda (expr)
    (syntax-case expr ()
      ((_ var handle-expr exprs ...)
       (syntax
        (with/fc (lambda (m e) (let ([var m]) handle-expr))
          (lambda () exprs ...)))))))
      
(define (condition-predicate kind-key)
  (lambda (exn)
    (has-condition-kind exn kind-key)))

(define make-property-condition 
  (letrec ([list2alist
        (lambda (ls)
          (cond [(null? ls) '()]
    	    [(null? (cdr ls)) 
    	     (error 'make-property-condition "Must be an even number of arguments to define condition properties.")]
    	    [else (cons (cons (car ls) (cadr ls))
    			(list2alist (cddr ls)))]))])

    (lambda (kind-key . props)
      (let ([alist (list2alist props)])
    (sisc-make-property-condition kind-key alist)))))

(define (condition-property-accessor kind-key prop-key)
  (lambda (exn)
    (let ([props (sisc-get-condition-properties exn kind-key)])
      (let ([ar (assoc prop-key props)])
    (and ar (cdr ar))))))

(define (abort obj)
  ((current-exception-handler) obj)
  (abort (make-property-condition
      'exn
      'message
      "Exception handler returned")))

(define (signal exn)
  ((current-exception-handler) exn))


  
