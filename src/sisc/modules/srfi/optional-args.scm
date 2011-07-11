;;code for CHECK-ARG, :OPTIONAL and LET-OPTIONALS*
;;
;;A few SRFI's use this.
;;
;;Most of this is copied from Scheme48.
;;
;;There should also be a definition of LET-OPTIONALS here, but it gets
;;only used in one place in srfi-1, so we replaced that code instead.

(define (check-arg pred val caller)
  (let lp ((val val))
    (if (pred val) val (lp (error "Bad argument" val pred caller)))))

(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default-exp)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
       (if (null? (cdr maybe-arg)) (car maybe-arg)
           (error "too many optional arguments" maybe-arg))
       default-exp)))

    ((:optional rest default-exp arg-test)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
       (if (null? (cdr maybe-arg))
           (let ((val (car maybe-arg)))
    	 (if (arg-test val) val
    	     (error "Optional argument failed test"
    		    'arg-test val)))
           (error "too many optional arguments" maybe-arg))
       default-exp)))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* arg (opt-clause ...) body ...)
     (let ((rest arg))
       (%let-optionals* rest (opt-clause ...) body ...)))))

(define-syntax %let-optionals*
  (syntax-rules ()
    ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
     (call-with-values (lambda () (xparser arg))
       (lambda (rest var ...)
         (%let-optionals* rest (opt-clause ...) body ...))))
    
    ((%let-optionals* arg ((var default) opt-clause ...) body ...)
     (call-with-values (lambda () (if (null? arg) (values default '())
    			      (values (car arg) (cdr arg))))
       (lambda (var rest)
     (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
     (call-with-values (lambda ()
    		 (if (null? arg) (values default '())
    		     (let ((var (car arg)))
    		       (if test (values var (cdr arg))
    			   (error "arg failed LET-OPT test" var)))))
       (lambda (var rest)
     (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
     (call-with-values (lambda ()
    		 (if (null? arg) (values default #f '())
    		     (let ((var (car arg)))
    		       (if test (values var #t (cdr arg))
    			   (error "arg failed LET-OPT test" var)))))
       (lambda (var supplied? rest)
     (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg (rest) body ...)
     (let ((rest arg)) body ...))

    ((%let-optionals* arg () body ...)
     (if (null? arg) (begin body ...)
     (error "Too many arguments in let-opt" arg)))))
