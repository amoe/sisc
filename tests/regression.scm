;;SISC-specific regression tests

(include "regression-definitions.scm")

(define-syntax should-be
  (syntax-rules ()
    ((_ test-id value expression)
     (with/fc (lambda (m e)
                (display (format "Failed: ~a, expected '~a', got exception ~a\n"
                                 test-id value (error-message m))))
       (lambda ()
         (let ([return-value expression])
           (if (not (equal? return-value value))
               (display (format "Failed: ~a, expected '~a', got '~a'\n"
                                test-id value return-value))
               (display (format "Passed: ~a\n" test-id)))))))))


(should-be 1151368 9
           (let ([x 0])
             (call/cc 
               (lambda (k)
                 (dynamic-wind
                   (lambda () (set! x (+ x 1)))
                   (lambda () (set! x (+ x 3)) (k))
                   (lambda () (set! x (+ x 5))))))
             x))

;;used to cause an NPE
(should-be 807474 #f
           (let ((k #f))
             (dynamic-wind
              void 
              (lambda () (call/cc (lambda (x) (set! k x))) (list))
              void)
             (and k (let ((kk k)) (set! k #f) (kk)))))

;;Used to be an infinite loop
(should-be 812557 'okay (dynamic-wind void (lambda () 'okay) void))

;;Used to corrupt the global environment, resulting in identity being undefined.
(should-be 812537 3
           (begin (putprop 'identity (lambda (x) x))
                  (with/fc (lambda (m e) (void))
                    (lambda () 
                      (eval '(import foobarbaz) 
                            (scheme-report-environment 5))))
                  (eval '(identity 3))))

;; (Partially fixed) References to helper functions from macros in report-env
;; will generate code incapable of resolving the helper                        
(define test-env (scheme-report-environment 5))
(should-be 818786 '(3 okay #t)
           (let ([test-env (scheme-report-environment 5)])
             (eval '(define-syntax foo 
                      (syntax-rules ()
                        ((_ x)
                         (car x))
                        ((_ x y)
                         (car y)))) test-env)
             (list
              (with/fc (lambda (m e) 'error)
                       (lambda () (eval '(force (delay 3)) test-env)))
              (eval '(foo a '(okay)) test-env)
              (with/fc (lambda (m e) (equal? (error-message m)
                                             "invalid syntax (foo a b c)"))
                       (lambda () (eval '(foo a b c) test-env))))))
                            
;; Used to cause an out of memory error (Interrupts must be enabled to pass)
(should-be 820401 'okay
           (let ()
             (import threading)
             (let loop ([x 200])
               (if (zero? x)
                   'okay
                   (let ([t (thread/new (lambda () (force (let loop () 
                                                           (delay (force (loop)))))))])
                     (thread/start t)
                     (sleep 100)
                     (thread/interrupt t)
                     (loop (- x 1)))))))

;; Used to trigger any number of serialization bugs related to ports when
;; run from the repl
(should-be 856491 22
           (let ()
             (import serial-io)
             (define (foo x)
               (+ x (call/cc
                     (lambda (k)
                       (call-with-serial-output-file
                           "test.ser"
                         (lambda (port) (serialize k port)))
                       1))))
             (let ([rv (foo 10)])
               (if (= rv 11)
                   ((call-with-serial-input-file
                     "test.ser"
                     deserialize)
                    12)
                   rv))))

;;Used to cause an error trying to resolve CharSequence
(should-be 830507 'ok
           (let ()
             (import s2j)
             ((generic-java-method '|matcher|)
              ((generic-java-method '|compile|)
               (java-null (java-class '|java.util.regex.Pattern|))
               (->jstring "([0-9])"))
              (->jstring "a3b"))
             'ok))

;;Used to cause a stack overflow error
(should-be 864792 2000
           (let ([x (iota 2000)])
             (import serial-io)
             (call-with-serial-output-file "test.ser"
               (lambda (out)
                 (serialize x out)))
             (length (call-with-serial-input-file "test.ser"
                       (lambda (in)
                         (deserialize in))))))

;; Used to cause an error (couldn't serialize)
(should-be 870468 "Hello, World!"
           (let ()
             (import serial-io)
             (import s2j)
             (let ([x (->jstring "Hello, World!")])
               (call-with-serial-output-file "test.ser"
                 (lambda (out) (serialize x out)))
               (->string (call-with-serial-input-file "test.ser"
                          (lambda (in) (deserialize in)))))))

;;Used to be true
(should-be 870845 #f (char>? #\c #\a #\b))

;;Used to return (1 2)
(should-be 878707 '(2 2)
           (let ([entrances 0]
                 [exits 0])
             (with/fc
              (lambda (a b) (b 5))
              (lambda () (dynamic-wind
                             (lambda () (set! entrances (+ entrances 1)))
                             (lambda () (/ 1 0))
                             (lambda () (set! exits (+ exits 1))))))
             (list entrances exits)))

;Used to cause an undefined variable error
(should-be 886733 4
           (let ([x (car '(2))])
             (letrec ([y x]
                      [g (lambda (i)
                           (+ y x))])
               (g y))))

(should-be 937722 #t
           (andmap (lambda (p)
                     (with/fc (lambda (m e) #t)
                              (lambda () (p '()) (p '(a)) #f)))
                   (list caar cadr cdar cddr)))

;;interfaces and classes that inherit from Object must have Object
;;last in their declared-superclasses list
(should-be 953043 '(#t #t #t)
           (let ()
             (import s2j)
             (define-java-classes
               <java.lang.object>
               <java.io.file>
               <java.io.serializable>
               (<int*> |int[]|))
             (map (lambda (c)
                    (eqv? (car
                           (reverse
                            (java-class-declared-superclasses c)))
                          <java.lang.object>))
                  (list <java.io.file>
                        <java.io.serializable>
                        <int*>))))

;; equal? was insensitive to exactness of numbers
(should-be 987271 #f (equal? 3 3.0))

;; Should cause an error
(should-be 994759 #f
  (with/fc (lambda (m e) #f)
           (lambda () (and (set-cdr! `(3) 3) #t))))

(should-be 1037559 -1/4 (- 1/4))

(should-be 1074510 '(1 1) `(1 . ,'(1)))

(should-be 1094233 #t (= 2 2 2))
(should-be 1094233 4.0 (max 3.9 4))
(should-be 1094233 3.0 (min 3 4.9))

(should-be 1093762 '((quasiquote (b (unquote (+ 1 2))))) `(`(b ,(+ 1 2))))

(should-be 1093699 #f (begin
			(putprop 'foo 'bar 'baz)
			(remprop 'foo 'bar)
			(getprop 'foo 'bar)))

(should-be 1094723 #t 
  (with/fc (lambda (m e) #t)
    (lambda () 
      (with-input-from-string "'#(1 2 . ,(- 3))" read))))

(should-be 1096496 #t (> 0.1 (atan 0) -0.1))

(should-be 1099750 #t
           (with/fc (lambda (m e) #f)
                    (lambda ()
                      ((current-optimizer) 
                       '(begin (lambda #t () () 3)
                               (letrec #t () () 4)))
                      #t)))

(should-be 1103630 "\u0e10\u0e1f\u0e2b\u0e01\u0e14"
           (let ()
              (import serial-io)
              (call-with-serial-output-file "test.ser" 
                (lambda (port) (serialize "\u0e10\u0e1f\u0e2b\u0e01\u0e14" 
                                          port)))
              (call-with-serial-input-file "test.ser" deserialize)))

(should-be 1096047 '#(1 2 unquote (list 3 4)) 
                   `#(1 2 unquote (list 3 4)))

(should-be 1124005 10
  (let () 
    (import m)
    (a)))

(should-be 1181453 #t
  (procedure? ((call/cc call/cc) (lambda (f) f))))

(should-be 1309820 '(#t #t #t #t #t #t #t #t #t)
           (let ()
             (import string-io)
             (define (try-read s)
               (with/fc (lambda (m e) #t)
                 (lambda () (call-with-input-string s read))))
             (map try-read '("("        "(."            "(.)"
                             "(1"       "(1 ."          "(1 .)"
                             "(1 . 2"   "(1 . 2 3"      "(1 . 2 3)"))))

(should-be 1311739 #t
           (let ()
             (import string-io)
             (set-car! (call-with-input-string "'(foo)" read) 1)
             #t))

(should-be 1320475 #t
           (let ()
             (import string-io)
             (call-with-input-string "bar"
               (lambda (port)
                 (with/fc (lambda (m e) #t)
                   (lambda () (read-string "foo" 0 3 port)))))))

(should-be 1346205 '(() ())
           (map (lambda (f)
                  (with/fc (lambda (m e) '())
                    (lambda () (f 1 1))))
                (list string=? char=?)))

(should-be 1353781 #t
           (let ()
             (import* file-manipulation file-delete!)
             (import procedure-properties)
             (import s2j)
             (define-java-classes
               <java.io.file-output-stream>
               <java.io.object-output-stream>
               <java.io.file-input-stream>
               <java.io.object-input-stream>)
             (define-generic-java-methods
               write-object
               read-object
               flush
               close)
             (define ser-file "test.ser")
             (define (proc) #t)
             (set-procedure-property! proc 'foo 'bar)
             (dynamic-wind
              (lambda ()
                (let ([p (java-new <java.io.object-output-stream>
                                   (java-new <java.io.file-output-stream>
                                             (->jstring ser-file)))])
                  (write-object p (java-wrap proc))
                  (flush p)
                  (close p)))
              (lambda ()
                (let ([p (java-new <java.io.object-input-stream>
                                   (java-new <java.io.file-input-stream>
                                             (->jstring ser-file)))])
                  (define proc1 (java-unwrap (read-object p)))
                  (close p)
                  (eq? (procedure-property proc1 'foo) 'bar)))
              (lambda ()
                (file-delete! ser-file)))))

(should-be 1372516 '(1 . 3)
           (let ()
             (import* misc define-simple-syntax)
             (define-simple-syntax (let/cc k . body)
               (call/cc (lambda (k) . body)))
             (let* ([p1 (make-parameter 1)]
                    [k (let/cc out 
                               (parameterize ([p1 2])
                                             (p1 3) 
                                             (let ([r (let/cc k (out k))])
                                               (cons r (p1)))))])
               (if (procedure? k) 
                   (k (p1))
                   k))))

;This uses a complex nesting of error handling because we
;want to detect the error at application but not at compilation time.
(should-be 1434848 'passed
           (call/cc (lambda (esc)
                      (with/fc (lambda (m e) 'passed)
                               (lambda ()
                                 ((with/fc (lambda (m e) (esc 'failed))
                                           (lambda ()
                                             (eval '(lambda () (+ ((lambda () #f) 3))))))))))))

(should-be 1453863 1/2
           (with/fc
               (lambda (m e) (e 2))
             (lambda () (/ (/ (/ (/ 1 0)))))))

(should-be 1456295 #f
           (with/fc
               (lambda (m e) #f)
               (lambda () (import threading)
                          (begin (thread/holds-lock? (mutex/new) (mutex/new)) #t))))


(define (foo) 3)

(should-be 1457379 3
      (let ([env (scheme-report-environment 5)])
        (eval '(define-syntax foo
                 (syntax-rules () 
                   ((foo) 1)))
              env)
        (eval '(foo) (interaction-environment))))

(require-extension (srfi 11))
(should-be 1460118 '(1 2 3)
           (let-values ([(a b c) (values 1 2 3)])
             (list a b c)))

(should-be 1482221 #f (equal? '(((()))) '(())))

;;this used to die
(should-be 1509504 0
           (begin
             (eval '(define my-length length))
             (eval '(begin
                      (define (letrec-opt-bug v p)
                        (letrec ([l (my-length v)])
                          (p l)))
                      (set! my-length (lambda (x) (length x)))
                      (letrec-opt-bug '() values)))))

;;this used to throw an error about call-with-input-strings
;;arity, because the string constant was read incorrectly, consuming
;;part of the following expression
(should-be 1520129 #t
           (let ()
             (import string-io)
             (call-with-input-string "\"\u0000\""
               (lambda (in)
                 (string? (read in))))))

; Used to pick up the escaped newline as a real one,
; and start parsing later on the comment line.
(should-be 1538637 1 
   (begin 1 ; \n 2
    ))

; Used to throw an error handling 'fix'
(should-be 1545294 #t
           (eval '(begin (define (f) (fix)) #t) (sisc-initial-environment)))

; In 1.16.4, we failed to escape backslash
(should-be 1557206 #t 
  (let ()
    (import string-io)
    (char=? #\\ (string-ref 
                  (with-output-to-string 
                    (lambda ()
                      (write "a\\b")))
                  3))))
     
; Multi-arg LCM and GCD were broken
(should-be 1640371 12 (lcm 2 3 4))
(should-be 1640371 2 (gcd 12 6 4))
               
; eval was evaluating code in the wrong environment
(should-be 1650514 1
  (let ([env (sisc-initial-environment)])
    (set! intvalue 3)
    (eval '(load "regression-definitions.scm") env)
    (getprop 'intvalue env)))

; read-string was returning -1 on eof
(should-be 1653382 #!eof
  (let ()
    (import string-io)
    (read-string (make-string 8) 0 1 (open-input-string ""))))

;Used to loop infinitely and crash with OutOfMemory
(should-be 1666713 #t
  (let ()
    (import string-io)
    (sc-expand (with-input-from-string "'#0=(1 . #0#)" read))
    #t))

(should-be 1693950 1 (append 1))


