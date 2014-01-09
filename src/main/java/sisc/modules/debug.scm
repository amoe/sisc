(define (annotated? obj)
  (not (null? (annotation-keys obj))))

(define (show-expr expr)
  (let loop ([expr expr]
             [phases (map cdr
                          (compilation-phases '((l) (l))
                                              (interaction-environment)))])
    (pretty-print (if (procedure? expr) (express expr) expr))
    (newline)
    (display "=>")
    (newline)
    (if (null? phases)
        (expr)
        (loop ((car phases) expr) (cdr phases)))))

(define-simple-syntax (show expr) (show-expr (quote expr)))
  
(define trace-depth (make-parameter -1))

(define (indent n)
  (for-each (lambda (n) (display #\space)) (iota n)))

(define (trace-call before call)
  (dynamic-wind
   (lambda () (trace-depth (+ (trace-depth) 1)))
   (lambda ()
     (let ([depth (trace-depth)])
       (if (> depth 20)
           (begin 
             (indent 40)
             (display (format "[~s]" depth)))
           (indent (* depth 2)))
       (write (before))
       (newline)
       (let ([result (call)])
         (if (> depth 20)
             (begin 
               (indent 40)
               (display (format "[~s]" depth)))
             (indent (* depth 2)))
         (write result)
         (newline)
         result)))
   (lambda () (trace-depth (- (trace-depth) 1)))))

(define-syntax trace-lambda
  (syntax-rules ()
    ((trace-lambda name (formal ...) body ...)
     (lambda (formal ...)
       (trace-call (lambda () (list 'name formal ...))
                   (lambda () (begin body ...)))))
    ((trace-lambda name (formal ... . rest) body ...)
     (lambda (formal ... . rest)
       (trace-call (lambda () (list 'name formal ... rest))
                   (lambda () (begin body ...)))))
    ((trace-lambda name formal body ...)
     (lambda formal
       (trace-call (lambda () (list 'name formal))
                   (lambda () (begin body ...)))))))

(define-syntax trace-let
  (syntax-rules ()
    ((trace-let name ([var val] ...) body ...)
     (let name ([var val] ...)
       (trace-call (lambda () (list 'name var ...))
                   (lambda () (begin body ...)))))))

(define (make-traced procedure-name procedure)
  (lambda args
    (trace-call (lambda () (cons procedure-name args))
                (lambda () (apply procedure args)))))

(define *TRACED-PROCEDURES* (make-hashtable eq?))

(define (install-tracer real-ps proc)
  (let ([traced-proc (make-traced real-ps proc)])
    (putprop real-ps traced-proc)
    (cons proc traced-proc)))

; Check whether any of the traced procedures have changed. If so,
; trace the new procedures.
(define (verify-traced!)
  (hashtable/for-each
   (lambda (real-ps rest)
     (if (not (eq? (cdr rest) (getprop real-ps)))
         (let* ([real-ps (sc-expand real-ps)]
                [proc (getprop real-ps)])
           (if (procedure? proc)
               (hashtable/put! *TRACED-PROCEDURES*
                               real-ps
                               (install-tracer real-ps proc))))))
   *TRACED-PROCEDURES*))

(define (trace . procs)
  (verify-traced!)
  (if (null? procs)
      (display (format "{currently traced procedures: ~a}~%" 
                       (hashtable/keys *TRACED-PROCEDURES*)))
      (for-each 
       (lambda (procedure-symbol)
         (let* ([real-ps (sc-expand procedure-symbol)]
                [proc (getprop real-ps)])
           (if (procedure? proc)
               (hashtable/get! *TRACED-PROCEDURES*
                               real-ps
                               (lambda ()
                                 (install-tracer real-ps proc)))
               (error 'trace "'~s' is not bound to a procedure." 
                      procedure-symbol))))
       procs)))

(define (untrace . procs)
  (verify-traced!)
  (if (null? procs)
      (display (format "{currently traced procedures: ~a}~%" 
                       (hashtable/keys *TRACED-PROCEDURES*)))
      (for-each 
       (lambda (procedure-symbol)
         (let* ([real-ps (sc-expand procedure-symbol)]
                [proc (hashtable/remove! *TRACED-PROCEDURES* real-ps)])
           (if proc
               (when (eq? (cdr proc) (getprop real-ps))
                 (putprop real-ps (car proc)))
               (error 'untrace "~a is not bound to a traced procedure."
                      procedure-symbol))))
       procs)))

(define *BREAKPOINTS* (make-hashtable eq?))
(define *CURRENT-BREAKPOINT* #f)

(define (current-breakpoint-continuation)
  (and *CURRENT-BREAKPOINT*
       (car *CURRENT-BREAKPOINT*)))
(define (current-breakpoint-args)
  (and *CURRENT-BREAKPOINT*
       (cdr *CURRENT-BREAKPOINT*)))
(define (set-current-breakpoint-args! proc . args)
  (and *CURRENT-BREAKPOINT*
       (begin (set-cdr! *CURRENT-BREAKPOINT* (cons proc args)) #t)))

(define (set-breakpoint! function-id)
  (define (make-breakpoint proc)
    (lambda args
      ; Setup the return continuation
      (call-with-values
          (lambda ()
            (call/cc
             (lambda (k)
               (set! *CURRENT-BREAKPOINT* (cons k (cons proc args)))
               ;; now drop to the repl
               ((getprop 'repl '*debug*)
                (lambda ()
                  (display (format "{break: ~s}~%" 
                                   (cons function-id args)))))
               ;; normally we never get here, but just in case we do
               ;; let's do something sensible
               (values proc args))))
        apply)))
  (let* ([function-id (sc-expand function-id)]
         [function (getprop function-id)])
    (if (not function)
        (error 'set-breakpoint! "no such function: ~a" function-id))
    (hashtable/get! *BREAKPOINTS*
                    function-id
                    (lambda ()
                      (putprop function-id (make-breakpoint function))
                      function))))

(define (clear-breakpoint! function-id) 
  (let* ([function-id (sc-expand function-id)]
         [function (hashtable/remove! *BREAKPOINTS* function-id)])
    (if function
        (putprop function-id function)
        (error 'clear-breakpoint!
               "no such function ~a or function is not a breakpoint."
               function-id))))

(define (continue)
  (let ([c *CURRENT-BREAKPOINT*])
    (if c
        (begin (set! *CURRENT-BREAKPOINT* #f)
               ((car c) (cadr c) (cddr c)))
        (error 'continue "nowhere to continue to."))))


;;;;;;;;;;;;;; exception display ;;;;;;;;;;;;;;;;;;;;

(define suppressed-stack-trace-source-kinds (make-parameter '(#f)))

(define (filter-expr? exp)
  (memq (annotation exp 'source-kind)
        (suppressed-stack-trace-source-kinds)))

(define (filter-stack stack)
  (if (null? stack)
      '()
      (let ([entry (car stack)]
            [tail (filter-stack (cdr stack))])
        (if (pair? entry)
            (let ([sub-stack (filter-stack (cdr entry))])
              (if (null? sub-stack)
                  tail
                  (cons (cons (car entry) sub-stack) tail)))
            (if (filter-expr? entry)
                tail
                (cons entry tail))))))

(define (stack-trace k)
  (cond
    [(not k)
     '()]
    [(annotation k 'unsafe-cont) => stack-trace]
    [else
      (let ([nxp  (continuation-nxp k)]
            [st   (continuation-stack-trace k)]
            [stk  (continuation-stk k)])
        (let ([tail (stack-trace stk)])
          (if st
              (let ([filtered (filter-stack (cdr st))])
                (if (null? filtered)
                    tail
                    (cons (cons nxp (cons (car st) filtered)) tail)))
              (if (filter-expr? nxp)
                  tail
                  (cons nxp tail)))))]))

(define (format-expression-location expr)
  (define (source-annotations)
    (apply values (map (lambda (v)
                         (cond [(annotation expr v) => (lambda (x) x)]
                               [else '?]))
                       '(source-file
                         line-number
                         column-number
                         proc-name))))
  (call-with-values source-annotations
    (lambda (sourcefile line column procname)
      (cond [(not (eq? procname '?))
             (format "~a:~a:~a: <from call to ~a>"
                     sourcefile line column procname)]
            [(and (_fill-rib? expr)
                  (_free-reference-exp? 
                   (_fill-rib-exp expr)))
             (format "~a:~a:~a: <from call to/argument of ~a>" 
                     sourcefile
                     line column
                     (_free-reference-symbol (_fill-rib-exp expr)))]
            [else
             (format "~a:~a:~a: <indeterminate call>" 
                     sourcefile
                     line column)]))))

(define (format-k-stack l level)
  (let ([prefix (apply string (map (lambda (x) '#\ ) (iota (* 2 level))))])
    (apply string-append
           (map (lambda (x)
                  (string-append
                   prefix
                   (if (pair? x)
                       (string-append
                        (format "~a repetitions of ...\n" (car x))
                        (format-k-stack (cdr x) (+ level 1)))
                       (string-append
                        (format-expression-location x)
                        "\n"))))
                l))))

(define (format-stack-trace-entry entry)
  (if (pair? entry)
      (let ([expr       (car entry)]
            [overflown? (cadr entry)]
            [stack      (cddr entry)])
        (string-append (format-k-stack stack 0)
                       (if overflown? "...\n" "")))
      (string-append (format-expression-location entry) "\n")))

(define (stack-trace-entry-has-data? entry)
  (if entry
      (if (pair? entry)
          (not (null? (cddr entry)))
          #t)
      #f))

(define (full-stack-tracing-enabled?)
  (> (max-stack-trace-depth) 0))

(define *STACK-TRACE-MESSAGE-PRINTED?* #f)
(define *SUPPRESSED-MESSAGE-PRINTED?* #f)

(define (print-stack-trace k)
  (define (print-single-entry entry count)
    (if (full-stack-tracing-enabled?)
        (display "---------------------------\n"))
    (display entry)
    (when (> count 1)
      (display (format "[previous ~a repeated~a]\n"
                       (if (full-stack-tracing-enabled?)
                           "entries"
                           "entry")
                       (case count
                         ((1) " once")
                         ((2) " twice")
                         (else (format " ~a times" count)))))))
  ;;; Bunch up multiple lines and add a repeat count
  (let loop ([entries (stack-trace k)]
             [last #f]
             [count 1])
    (if (null? entries)
        (if last (print-single-entry last count))
        (let ([current (car entries)]
              [rest    (cdr entries)])
          (if (stack-trace-entry-has-data? current)
              (let ([print-rep (format-stack-trace-entry current)])
                (if (equal? print-rep last)
                    (loop rest last (+ count 1))
                    (begin
                      (if last (print-single-entry last count))
                      (loop rest print-rep 1))))
              (loop rest last count)))))
  ;;; If detailed stack tracing was never enabled, tell the user how
  ;;; to enable it.
  (if (full-stack-tracing-enabled?)
      (set! *STACK-TRACE-MESSAGE-PRINTED?* #t)
      (if (not *STACK-TRACE-MESSAGE-PRINTED?*)
          (begin
            (display 
             (string-append
              "---------------------------\n"
              "To enable more detailed stack tracing, set the dynamic "
              "parameter max-stack-trace-depth to a non-zero value, "
              "e.g. 16.\n"))
            (set! *STACK-TRACE-MESSAGE-PRINTED?* #t))))
  ;;; If the user has never seen a full stack trace, tell them how to
  ;;; get one.
  (if (null? (suppressed-stack-trace-source-kinds))
      (set! *SUPPRESSED-MESSAGE-PRINTED?* #t)
      (if (not *SUPPRESSED-MESSAGE-PRINTED?*)
          (begin
            (display
             (string-append
              "---------------------------\n"
              "Some stack trace entries may have been suppressed. "
              "To see all entries set the dynamic parameter "
              "suppressed-stack-trace-source-kinds to '().\n"))
             (set! *SUPPRESSED-MESSAGE-PRINTED?* #t)))))

(print-exception-stack-trace-hook
 'debug
 (lambda (next e)
   (if (exception? e)
       (print-stack-trace (exception-continuation e))
       (next e))))

;; 
;; The contents of this file are subject to the Mozilla Public
;; License Version 1.1 (the "License"); you may not use this file
;; except in compliance with the License. You may obtain a copy of
;; the License at http://www.mozilla.org/MPL/
;; 
;; Software distributed under the License is distributed on an "AS
;; IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the License for the specific language governing
;; rights and limitations under the License.
;; 
;; The Original Code is the Second Interpreter of Scheme Code (SISC).
;; 
;; The Initial Developer of the Original Code is Scott G. Miller.
;; Portions created by Scott G. Miller are Copyright (C) 2000-2007
;; Scott G. Miller.  All Rights Reserved.
;; 
;; Contributor(s):
;; Matthias Radestock 
;; 
;; Alternatively, the contents of this file may be used under the
;; terms of the GNU General Public License Version 2 or later (the
;; "GPL"), in which case the provisions of the GPL are applicable 
;; instead of those above.  If you wish to allow use of your 
;; version of this file only under the terms of the GPL and not to
;; allow others to use your version of this file under the MPL,
;; indicate your decision by deleting the provisions above and
;; replace them with the notice and other provisions required by
;; the GPL.  If you do not delete the provisions above, a recipient
;; may use your version of this file under either the MPL or the
;; GPL.
;;
