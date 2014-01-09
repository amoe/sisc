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

;; Parameter Support, compatible with SRFI-39

(define (make-parameter value . converter)
  (cond [(null? converter) 
         (_make-parameter value)]
        [(null? (cdr converter))
         (let ([param (_make-parameter value)]
               [converter (car converter)])
           (lambda arg
             (if (null? arg)
                 (param)
                 (param (converter (car arg))))))]
        [else (error 'make-parameter "too many arguments.")]))

(define (make-config-parameter name value . converter)
    (cond [(null? converter) 
           (_make-config-parameter name value)]
          [(null? (cdr converter))
           (let ([param (_make-config-parameter name value)]
                 [converter (car converter)])
             (lambda arg
               (if (null? arg)
                   (param)
                   (param (converter (car arg))))))]
          [else (error 'make-config-parameter "too many arguments.")]))

(define (make-native-parameter name . converter)
    (cond [(null? converter) 
           (_make-native-parameter name)]
          [(null? (cdr converter))
           (let ([param (_make-native-parameter name)]
                 [converter (car converter)])
             (lambda arg
               (if (null? arg)
                   (param)
                   (param (converter (car arg))))))]
          [else (error 'make-config-parameter "too many arguments.")]))

(define-syntax parameterize
  (syntax-rules ()
    [(_ ((param-name new-value) ...) . body)
     (let ([old-values #f]
           [tmps (list new-value ...)])
       (dynamic-wind 
        (lambda () 
          (set! old-values (list (param-name) ...))
          (for-each (lambda (p l) (p l))
                    (list param-name ...)
                    tmps))
        (lambda () . body)
        (lambda () 
          (set! tmps (list (param-name) ...))
          (for-each (lambda (p l) (p l))
                    (list param-name ...)
                    old-values))))]))

;; native parameters

(define current-input-port      (make-native-parameter "inputPort"))
(define current-output-port     (make-native-parameter "outputPort"))
(define case-sensitive          (make-native-parameter "caseSensitive"))
(define print-shared            (make-native-parameter "printShared"))
(define vector-length-prefixing (make-native-parameter "vectorLengthPrefixing"))
(define emit-debugging-symbols  (make-native-parameter "emitDebuggingSymbols"))
(define emit-annotations        (make-native-parameter "emitAnnotations"))
(define character-set           (make-native-parameter "characterSet"))
(define permissive-parsing      (make-native-parameter "permissiveParsing"))
(define internal-debugging      (make-native-parameter "internalDebugging"))
(define synopsis-length         (make-native-parameter "synopsisLength"))
(define source-annotations      (make-native-parameter "sourceAnnotations"))
(define max-stack-trace-depth   (make-native-parameter "maxStackTraceDepth"))
(define custom-printing         (make-native-parameter "customPrinting"))
(define custom-display-type-map (make-native-parameter "customDisplayTypeMap"))
(define custom-write-type-map   (make-native-parameter "customWriteTypeMap"))

; Enable inlining and optimizer assumptions while expanding libraries
 
(putprop 'assumptive-procedures '*opt*
         '(not + - * / car cdr caar cadr cdar cddr zero?
               null? pair? number? procedure?))
 
(if (equal? (getenv "sisc.debugging") "true")
    (begin
      (emit-annotations #t)
      (emit-debugging-symbols #t)))

;;;;;;;;;; hooks ;;;;;;;;;;

(define (hook-items proc)
  (annotation proc 'items))
  
(define (set-hook-items! proc items)
  (set-annotation! proc 'items items))

(define (make-hook default)
  (let ([items (list default)])
    (define (hook key item)
      (cond [(assq key (cdr items))
             => (lambda (x) (set-cdr! x item))]
            [else
              (set-cdr! items (cons (cons key item)
                                    (cdr items)))]))
    (set-hook-items! hook items)
    hook))

(define (invoke-hook hook . args)
  (let* ([all-items (hook-items hook)]
         [def (car all-items)])
    (let loop ([items (cdr all-items)]
               [args  args])
      (if (null? items)
          (apply def args)
          (apply (cdar items)
                 (lambda args (loop (cdr items) args))
                 args)))))

;;;;;;;;;;;;;;;; error handling ;;;;;;;;;;;;;;;

; Needed later
(define pretty-print)

;; Most of the code here is for providing SRFI-23 style error
;; producing, which is then applied into SISC's failure-continuation
;; model.

(define (make-exception error error-k)
  `((error-continuation . ,error-k) . ,error))
(define (exception? e)
  (and (pair? e) (pair? (car e)) (eq? (caar e) 'error-continuation)))
(define (exception-continuation exception)
  (cdar exception))
(define (exception-error exception)
  (cdr exception))

(define (throw error-or-exception . error-k)
  (call-with-failure-continuation
      (lambda (fk)
        (cond
          [(pair? error-or-exception)
           (if (null? error-k)
               (call-with-current-continuation
                   (lambda (k) (fk error-or-exception k)))
               (fk error-or-exception (car error-k)))]
          [(and (exception? error-or-exception)
                (null? error-k))
           (fk (exception-error error-or-exception)
               (exception-continuation error-or-exception))]
          [else
            (error 'throw "expected error-record or exception, got ~a"
                   error-or-exception)]))))

(define (error . args)
  (throw (apply make-error args)))

(define (make-nested-error error-record parent-record . parent-k)
  `((parent . ,(if (null? parent-k)
                   parent-record
                   (make-exception parent-record (car parent-k))))
    . ,error-record))

(define (make-error . args)
  (let ([ops (print-shared #t)]
        [error-record '()])
    ;;Location
    (cond [(null? args) (void)]
          [(and (not (null? (cdr args)))
                (symbol? (car args)))
           (set! error-record (cons (cons 'location (car args)) 
                                    error-record))
           (set! args (cdr args))]
          [(not (car args))
           (set! args (cdr args))])
      
    ;;Message/Value
    (let ([message (and (not (null? args))
                        (car args))])
      (if message (set! args (cdr args)))
      (if (null? args)
          (if message (set! error-record (cons `(message . ,message)
                                               error-record)))
          (if (string? message)
              (set! error-record
                (cons `(message . ,(apply format message args))
                      error-record))
              (error 'error "cannot specify arguments to a non format-string error."))))
    (print-shared ops)
    error-record))
    

(define (error-location error-record)
  (cond [(and (pair? error-record) (assq 'location error-record))
         => cdr]
        [else #f]))

(define (error-message error-record)
  (cond [(and (pair? error-record) (assq 'message error-record))
         => cdr]
        [else #f]))

(define (error-parent error-record)
  (cond [(and (pair? error-record) (assq 'parent error-record))
         => cdr]
        [else #f]))

(define (error-parent-error error-record)
  (let ([error-parent (error-parent error-record)])
    (and error-parent (exception-error error-parent))))

(define (error-parent-continuation error-record)
  (let ([error-parent (error-parent error-record)])
    (and error-parent (exception-continuation error-parent))))

(define (make-error-message location message)
  (if location
      (if message
          (format "Error in ~a: ~a" location message)
          (format "Error in ~a." location))
      (if message
          (format "Error: ~a" message)
          "Error.")))

(define (display-error e)
  (display (make-error-message (error-location e)
                               (error-message e)))
  (newline))

(define print-exception-stack-trace-hook
  (make-hook (lambda args
               (display "{warning: printing of stack trace not supported}\n"))))

(define (print-exception-stack-trace e)
  (invoke-hook print-exception-stack-trace-hook e))

(define (print-exception e . st)
  (let ([error (exception-error e)])
    (display-error error)
    (if (or (null? st) (car st))
        (print-exception-stack-trace e))
    (let ([p (and (pair? error) (error-parent error))])
      (if p 
          (begin (display "===========================\nCaused by ")
                 (apply print-exception p st))))))

(define (print-error e k)
  (print-exception (make-exception e k)))

;; FORMAT
(define format
  (let ()
    (define (round* n scale) ;; assert scale < 0
      (let ((one (expt 10 (- scale))))
        (/ (round (* n one)) one)))
    
    (define (string-index str c)
      (let ((len (string-length str)))
        (let loop ((i 0))
          (cond ((= i len) #f)
                ((eqv? c (string-ref str i)) i)
                (else (loop (+ i 1)))))))
    
    (define (string-grow str len char)
      (let ((off (- len (string-length str))))
        (if (positive? off)
            (string-append (make-string off char) str)
            str)))
    
    (define (string-pad-right str len char)
      (let ((slen (string-length str)))
        (cond ((< slen len)
               (string-append str (make-string (- len slen) char)))
              ((> slen len)
               (substring str 0 len))
              (else str))))
    (define documentation-string
"(format []  [...]) --  is #t, #f or an output-port
OPTION	[MNEMONIC]	DESCRIPTION	-- Implementation Assumes ASCII Text Encoding
~H	[Help]		output this text
~A	[Any]		(display arg) for humans
~S	[Slashified]	(write arg) for parsers
~W	[WriteCircular]	like ~s but outputs circular and recursive data structures
~~	[tilde]		output a tilde
~T	[Tab]		output a tab character
~%	[Newline]	output a newline character
~&	[Freshline]	output a newline character if the previous output was not a newline
~D	[Decimal]	the arg is a number which is output in decimal radix
~X	[heXadecimal]	the arg is a number which is output in hexdecimal radix
~O	[Octal]		the arg is a number which is output in octal radix
~B	[Binary]	the arg is a number which is output in binary radix
~w,dF	[Fixed]		the arg is a string or number which has width w and d digits after the decimal
~C	[Character]	charater arg is output by write-char
~_	[Space]		a single space character is output
~Y	[Yuppify]	the list arg is pretty-printed to the output
~?	[Indirection]	recursive format: next 2 args are format-string and list of arguments
~K	[Indirection]	same as ~?
")
    
    (define (format-fixed number-or-string width digits) ; returns a string
      (cond
       ((string? number-or-string)
        (string-grow number-or-string width #\space))
       ((number? number-or-string)
        (let ((real (real-part number-or-string))
              (imag (imag-part number-or-string)))
          (cond
           ((not (zero? imag))
            (string-grow
             (string-append (format-fixed real 0 digits)
                            (if (negative? imag) "" "+")
                            (format-fixed imag 0 digits)
                            "i")
             width
             #\space))
           (digits
            (let* ((rounded-number (exact->inexact (round* real (- digits))))
                   (rounded-string (number->string rounded-number))
                   (dot-index (string-index  rounded-string #\.))
                   (exp-index (string-index  rounded-string #\e))
                   (length    (string-length rounded-string))
                   (pre-string
                    (cond
                     (exp-index
                      (if dot-index
                          (substring rounded-string 0 (+ dot-index 1))
                          (substring rounded-string 0 (+ exp-index 1))))
                     (dot-index
                      (substring rounded-string 0 (+ dot-index 1)))
                     (else
                      rounded-string)))
                   (exp-string
                    (if exp-index (substring rounded-string exp-index length) ""))
                   (frac-string
                    (if exp-index
                        (substring rounded-string (+ dot-index 1) exp-index)
                        (substring rounded-string (+ dot-index 1) length))))
              (string-grow
               (string-append pre-string
                              (if dot-index "" ".")
                              (string-pad-right frac-string digits #\0)
                              exp-string)
               width
               #\space)))
           (else ;; no digits
            (string-grow (number->string real) width #\space)))))
       (else
        (error 'format "~~F requires a number or a string, got ~s" number-or-string))))
    (lambda args
      (cond
       ((null? args)
        (error 'format "required format-string argument is missing"))
       ((string? (car args))
        (apply format #f args))
       ((< (length args) 2)
        (error 'format "too few arguments ~s" args))
       (else
        (let ((output-port   (car  args))
              (format-string (cadr args))
              (args          (cddr args)))
          (letrec ((port 
                    (cond ((output-port? output-port) output-port)
                          ((eq? output-port #t) (current-output-port)) 
                          ((eq? output-port #f) (open-output-string)) 
                          (else (error 'format "bad output-port argument: ~s"
                                       output-port))))
                   (return-value 
                    (if (eq? output-port #f)    ;; if format into a string 
                        (lambda () (get-output-string port)) ;; then return the string
                        void))) ;; else do something harmless
            
            (define (format-help format-strg arglist)
              
              (let ((length-of-format-string (string-length format-strg)))
                (letrec ((anychar-dispatch       
                          (lambda (pos arglist last-was-newline) 
                            (if (>= pos length-of-format-string) 
                                arglist ; return unused args 
                                (let ((char (string-ref format-strg pos)))
                                  (cond            
                                   ((eqv? char #\~)
                                    (tilde-dispatch (+ pos 1) arglist
                                                    last-was-newline))
                                   (else                   
                                    (write-char char port)     
                                    (anychar-dispatch (+ pos 1) arglist #f)))))))
                         (has-newline?
                          (lambda (whatever last-was-newline)
                            (or (eqv? whatever #\newline)
                                (and (string? whatever)
                                     (let ((len (string-length whatever)))
                                       (if (zero? len)
                                           last-was-newline
                                           (eqv? #\newline
                                                 (string-ref whatever (- len 1)))))))))
                         (tilde-dispatch          
                          (lambda (pos arglist last-was-newline)     
                            (cond           
                             ((>= pos length-of-format-string)   
                              (write-char #\~ port) ; tilde at end of string is just output
                              arglist) ; return unused args
                             (else      
                              (case (char-upcase (string-ref format-strg pos)) 
                                ((#\A)       ; Any -- for humans 
                                 (let ((whatever (car arglist)))
                                   (display whatever port)
                                   (anychar-dispatch (+ pos 1) 
                                                     (cdr arglist) 
                                                     (has-newline? whatever last-was-newline))))
                                ((#\S)       ; Slashified -- for parsers  
                                 (let ((whatever (car arglist)))
                                   (write whatever port)     
                                   (anychar-dispatch (+ pos 1) 
                                                     (cdr arglist) 
                                                     (has-newline? whatever last-was-newline))))
                                ((#\W)
                                 (let ((whatever (car arglist)))
                                   (let ([old-shared-state (print-shared #t)])
                                     (write whatever port)
                                     (print-shared old-shared-state))
                                   (anychar-dispatch (+ pos 1) 
                                                     (cdr arglist) 
                                                     (has-newline? whatever last-was-newline))))
                                ((#\D)       ; Decimal
                                 (display (number->string (car arglist) 10) port)  
                                 (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                ((#\X)       ; HeXadecimal    
                                 (display (number->string (car arglist) 16) port)
                                 (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                ((#\O)       ; Octal   
                                 (display (number->string (car arglist)  8) port)
                                 (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                ((#\B)       ; Binary
                                 (display (number->string (car arglist)  2) port)
                                 (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                ((#\C)       ; Character 
                                 (write-char (car arglist) port) 
                                 (anychar-dispatch (+ pos 1) (cdr arglist) (eqv? (car arglist) #\newline)))
                                ((#\~)       ; Tilde  
                                 (write-char #\~ port)   
                                 (anychar-dispatch (+ pos 1) arglist #f))
                                ((#\%)       ; Newline   
                                 (newline port) 
                                 (anychar-dispatch (+ pos 1) arglist #t))
                                ((#\&)      ; Freshline
                                 (if (not last-was-newline) ;; (unless last-was-newline ..
                                     (newline port))
                                 (anychar-dispatch (+ pos 1) arglist #t))
                                ((#\_)       ; Space 
                                 (write-char #\space port)   
                                 (anychar-dispatch (+ pos 1) arglist #f))
                                ((#\T)       ; Tab 
                                 (write-char #\tab port)          
                                 (anychar-dispatch (+ pos 1) arglist #f))
                                ((#\Y)       ; Pretty-print
                                 (pretty-print (car arglist) port) 
                                 (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                ((#\F)
                                 (display (format-fixed (car arglist) 0 #f) port)
                                 (anychar-dispatch (+ pos 1) (cdr arglist) #f))
                                ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) ;; gather "~w[,d]F" w and d digits
                                 (let loop ((index (+ pos 1))
                                            (w-digits (list (string-ref format-strg pos)))
                                            (d-digits '())
                                            (in-width? #t))
                                   (if (>= index length-of-format-string)
                                       (error 'format "improper numeric format directive in ~s" format-strg)
                                       (let ((next-char (string-ref format-strg index)))
                                         (cond
                                          ((char-numeric? next-char)
                                           (if in-width?
                                               (loop (+ index 1)
                                                     (cons next-char w-digits)
                                                     d-digits
                                                     in-width?)
                                               (loop (+ index 1)
                                                     w-digits
                                                     (cons next-char d-digits)
                                                     in-width?)))
                                          ((char=? next-char #\F)
                                           (let ((width  (string->number (list->string (reverse w-digits))))
                                                 (digits (if (zero? (length d-digits))
                                                             #f
                                                             (string->number (list->string (reverse d-digits))))))
                                             (display (format-fixed (car arglist) width digits) port)
                                             (anychar-dispatch (+ index 1) (cdr arglist) #f)))
                                          ((char=? next-char #\,)
                                           (if in-width?
                                               (loop (+ index 1)
                                                     w-digits
                                                     d-digits
                                                     #f)
                                               (error 'format "too many commas in directive ~s" format-strg)))
                                          (else
                                           (error "~~w,dF directive ill-formed in ~s" format-strg)))))))
                                ((#\? #\K)       ; indirection -- take next arg as format string
                                 (cond	     ;  and following arg as list of format args
                                  ((< (length arglist) 2)
                                   (error 'format "less arguments than specified for ~~?: ~s" arglist))
                                  ((not (string? (car arglist)))
                                   (error 'format "~~? requires a string: ~s" (car arglist)))
                                  (else
                                   (format-help (car arglist) (cdr arglist))
                                   (anychar-dispatch (+ pos 1) (cddr arglist) #f))))
                                ((#\H)	; Help
                                 (display documentation-string port)
                                 (anychar-dispatch (+ pos 1) arglist #t))
                                (else                
                                 (error "unknown tilde escape: ~s"
                                        (string-ref format-strg pos)))))))))
                                        ; format-help main      
                  (anychar-dispatch 0 arglist #f))))
              
                                        ; format main    
              (let ((unused-args (format-help format-string args)))
                (if (not (null? unused-args))
                    (error "unused arguments ~s" unused-args))
                (return-value)))))))))
  
;Loads an already expanded file (ie does not run it through the expander)
(define (load-expanded file)
  (call-with-input-file file
    (lambda (port) 
      (do ((x (read port) (read port)))
          ((eof-object? x))
        (eval (list "noexpand" x))))))

(add-file-handler 'sce load-expanded)
(add-file-handler 'pp load-expanded)

;; This code is based on Richard Kelsey and Jonathan Rees' version of
;; dynamic-wind in Scheme48 (http://s48.org). It has been heavily
;; modified to account for SISC's lack of structures, make exception
;; handling work properly and conform with SRFI18 requirements with
;; regard to call/cc behaviour.
(define dynamic-wind)
(define call-with-current-continuation-unsafe call-with-current-continuation)
(define call/cc-unsafe call-with-current-continuation)

;;a point in the dynamic wind stack
;;-this would be easier if we had structures
(let-syntax ([point-depth 
              (syntax-rules () 
                ((_ point) (vector-ref point 0)))]
             [point-in
              (syntax-rules () 
                ((_ point) (vector-ref point 1)))]
             [point-out
              (syntax-rules () 
                ((_ point) (vector-ref point 2)))]
             [point-parent
              (syntax-rules () 
                ((_ point) (vector-ref point 3)))])
  (let ((original-call/cc call-with-current-continuation))
    ;;the dynamic wind stack
    (define get-dynamic-point current-wind)
    (define set-dynamic-point! current-wind)
    (define make-point vector) ; (depth in out parent)
    ;;
    (define (travel-between-points here target)
      (cond ((eq? here target)
             (set-dynamic-point! here))
            ((if here               ; HERE has reached the root.
                 (and target
                      (< (point-depth here) (point-depth target)))
                 #t)
             (travel-between-points here (point-parent target))
             ((point-in target))
             (set-dynamic-point! target))
            (else
             (set-dynamic-point! (point-parent here))
             ((point-out here))
             (travel-between-points (point-parent here) target))))
    ;;wind-safe call/cc
    (define (make-wind-safe cont)
      (let* ((point (get-dynamic-point))
             (safe-k
              (lambda results                
                (travel-between-points (get-dynamic-point) point)
                (apply cont results))))
        (set-annotation! safe-k 'unsafe-cont cont)
        safe-k))
    (define (dynwind-call/cc proc)
      (original-call/cc
       (lambda (cont)
         (proc (make-wind-safe cont)))))
    (define (dynamic-wind/impl in body out)
      (let* ((here (get-dynamic-point))
             (point (make-point (if here (+ (point-depth here) 1) 1)
                                in out here)))
        (in)
        (set-dynamic-point! point)
        (call-with-values
            (lambda () (with/fc (lambda (m e)
                                  (let ([wind-safe-exception
                                         (make-wind-safe e)])
                                    (set-dynamic-point! here)
                                    (out)
                                    (throw m wind-safe-exception)))
                                body))
          (lambda results
            (set-dynamic-point! here)
            (out)
            (apply values results)))))
    ;;finally, the install the dynamic-wind hooks
    (set! dynamic-wind dynamic-wind/impl)
    (set! call-with-current-continuation dynwind-call/cc)))

(define call/fc call-with-failure-continuation)
(define with/fc with-failure-continuation)
(define call/ec call-with-escape-continuation)
(define call/cc call-with-current-continuation)

(define (with-environment env thunk)
  (let ([old-env (interaction-environment env)])
    (dynamic-wind
        (lambda () (interaction-environment env))
        (lambda () (_with-environment env thunk))
        (lambda () (interaction-environment old-env)))))

;;;; "ratize.scm" Convert number to rational number (ported from SLIB)

(define rationalize (void))
(letrec ([rational:simplest 
          (lambda (x y)
            (cond ((< y x) (rational:simplest y x))
                  ((not (< x y)) (if (rational? x) x 
                                     (error 'rationalize 
                                            "~s weirdness" x)))
                  ((positive? x) (sr x y))
                  ((negative? y) (- (sr (- y) (- x))))
                  (else (if (and (exact? x) (exact? y)) 0 0.0))))]
         [sr 
          (lambda (x y) 
            (let ((fx (floor x)) (fy (floor y)))
              (cond ((not (< fx x)) fx)
                    ((= fx fy) (+ fx (/ (sr (/ (- y fy)) (/ (- x fx))))))
                    (else (+ 1 fx)))))])
  (set! rationalize 
        (lambda (x e)
          (rational:simplest (- x e) (+ x e)))))

(define list-tail (lambda (x k) (if (zero? k) x (list-tail (cdr x) (- k 1)))))

(define make-polar
  (lambda (x y)
    (make-rectangular (* x (cos y))
                      (* x (sin y)))))

(define (square x) (* x x))

(define (magnitude x)
  (cond [(real? x) (abs x)]
        [else (let ([c (abs (real-part x))]
                    [d (abs (imag-part x))])
               (if (< d c)
                   (* c (sqrt (+ 1 (square (/ d c)))))
                   (* d (sqrt (+ 1 (square (/ c d)))))))]))

(define angle 
  (lambda (x)
    (cond [(integer? x) (if (>= x 0)
                            (atan 0 1)
                            (atan 0 -1))]
          [(real? x) (atan 0 x)]
          [else (atan (imag-part x) (real-part x))])))

(define (string-copy x)
  (string-append x))

;(define (unquote x)
;  (error 'unquote "expression ~s not valid outside of a quasiquote."
;         x))

;(define (unquote-splicing x)
;  (error 'unquote-splicing "expression ~s valid outside of a quasiquote."
;         x))

;;; macro-defs.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

;;; simple delay and force; also defines make-promise

(define-syntax delay
  (lambda (x)
    (syntax-case x ()
      ((delay exp)
       (syntax (make-promise (lambda () exp)))))))

(define (make-promise proc)
  (let ([result-ready? #f]
        [result #!void])
    (lambda ()
      (if result-ready? result
          (let ([x (proc)])
            (if result-ready? result
                (begin (set! result-ready? #t)
                       (set! result x)
                       result)))))))

(define (force promise) (promise))

(define-syntax time
  (lambda (x)
    (syntax-case x ()
      ((_ e)
       (syntax (let* ((st (system-time))
                      (val e)
                      (et (system-time)))
                 (list val (list (- et st) 'ms)))))
      ((_ n e)
       (syntax (let* ((st (system-time))
                      (val (let loop ([x (- n 1)])
                             (if (zero? x) 
                                 e
                                 (begin e (loop (- x 1))))))
                      (et (system-time)))
                 (list val (list (quotient (- et st) n) 'avg 'ms))))))))

;; Unless and When 
(define-syntax when
  (syntax-rules ()
    ((_ e0 e1 e2 ...)
     (if e0 (begin e1 e2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ e0 e1 e2 ...)
     (if e0 '#!void (begin e1 e2 ...)))))

;;perform macro expansion on a file
(define (expand-file from to . scexpopts)
  (let ([inf (open-source-input-file from)]
        [outf (open-output-file to)])
    (let loop ([e (read-code inf)])
      (or (eof-object? e)
          (let ([source (_analyze!
                         ((current-optimizer)
                          (apply sc-expand e scexpopts))
                         (interaction-environment))])
            (pretty-print source outf)
            (newline outf)
            (loop (read-code inf)))))
    (close-output-port outf)
    (close-input-port inf)))

;; I/O ;;

(define (call-with-input-port&close port proc)
  (with/fc (lambda (m e)
             (close-input-port port)
             (throw m e))
           (lambda () 
             (call-with-values 
              (lambda () (proc port))
              (lambda results
                (close-input-port port)
                (apply values results))))))

(define (call-with-output-port&close port proc)
  (with/fc (lambda (m e)
             (close-output-port port)
             (throw m e))
           (lambda () 
             (call-with-values 
              (lambda () (proc port))
              (lambda results
                (close-output-port port)
                (apply values results))))))

  
(define (call-with-input-file file procOrEncoding . proc)
  (cond [(null? proc) 
         (call-with-input-port&close (open-input-file file) procOrEncoding)]        
        [(null? (cdr proc))
         (call-with-input-port&close (open-input-file file procOrEncoding)
                                     (car proc))]
        [else (error 'call-with-input-file "too many arguments.")]))

(define (call-with-output-file file procOrEncoding . proc)
  (cond [(null? proc) 
         (call-with-output-port&close (open-output-file file) procOrEncoding)]        
        [(null? (cdr proc))
         (call-with-output-port&close (open-output-file file procOrEncoding)
                                      (car proc))]
        [else (error 'call-with-output-file "too many arguments.")]))

(define (with-input-from-port port thunk)
  (let ([cip (current-input-port)])
    (dynamic-wind
     (lambda () (current-input-port port))
     thunk
     (lambda () (current-input-port cip)))))

(define (with-output-to-port port thunk)
  (let ([cop (current-output-port)])
    (dynamic-wind
     (lambda () (current-output-port port))
     thunk
     (lambda () (current-output-port cop)))))

(define (with-input-from-file file thunkOrEncoding . thunk)
  (cond [(null? thunk)
         (call-with-input-file file 
           (lambda (port) (with-input-from-port port thunkOrEncoding)))]
        [(null? (cdr thunk))
         (call-with-input-file file thunkOrEncoding
           (lambda (port) (with-input-from-port port (car thunk))))]
        [else (error 'with-input-from-file "too many arguments.")]))

(define (with-output-to-file file thunkOrEncoding . thunk)
  (cond [(null? thunk)
         (call-with-output-file file 
           (lambda (port) (with-output-to-port port thunkOrEncoding)))]
        [(null? (cdr thunk))
         (call-with-output-file file thunkOrEncoding
           (lambda (port) (with-output-to-port port (car thunk))))]
        [else (error 'with-output-to-file "too many arguments.")]))

(define (with-character-set set thunk)
 (let ([previous-set (character-set)])
    (dynamic-wind
     (lambda () (character-set set))
     thunk
     (lambda () (character-set previous-set)))))

(define (with-current-url url thunk)
  (let ([previous-url (current-url)])
    (dynamic-wind
     (lambda () (current-url (normalize-url previous-url url)))
     thunk
     (lambda () (current-url previous-url)))))

(set! class-path-extension-append!
  (let ([original-cpea class-path-extension-append!])
    (lambda (urls)
      (let ([c-url (current-url)])
        (original-cpea (map (lambda (url) (normalize-url c-url url))
                            urls))))))

;; needed in a few places; cut-down version from SRFI-1
(define (iota count)
  (do ((count (- count 1) (- count 1))
       (ans '() (cons count ans)))
      ((< count 0) ans)))

;;;;;;;;;;;;;;;; native functions ;;;;;;;;;;;;;

(if (getprop 'string-order)
    (let ((string-order-predicate 
           (lambda (p o)
             (lambda (str1 str2)
               (p (o str1 str2) 0)))))
      ;; string=? isn't present because equal? str1 str2 is far faster
      (set! string<? (string-order-predicate < string-order))
      (set! string>? (string-order-predicate > string-order))
      (set! string<=? (string-order-predicate <= string-order))
      (set! string>=? (string-order-predicate >= string-order))
      (set! string-ci=? (string-order-predicate = string-order-ci))
      (set! string-ci>? (string-order-predicate > string-order-ci))
      (set! string-ci<? (string-order-predicate < string-order-ci))
      (set! string-ci<=? (string-order-predicate <= string-order-ci))
      (set! string-ci>=? (string-order-predicate >= string-order-ci))))

;;;;;;;;;;;;; legacy macro support ;;;;;;;;;;;;

(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      ((_ (name . args) . body)
       (syntax (define-macro name (lambda args . body))))
      ((_ name transformer)
       (syntax
        (define-syntax name
          (lambda (y)
            (syntax-case y ()
               ((_ . args)
                (datum->syntax-object
                 (syntax _)
                 (apply transformer
                        (syntax-object->datum (syntax args)))))))))))))

(define-syntax defmacro
  (syntax-rules ()
    ((_ name args . body)
     (define-macro name (lambda args . body)))))

;;our srfi-0 implementation relies on this
(define *features* '())
(define (add-feature feature)
  (if (not (memq feature *features*))
      (set! *features* (cons feature *features*))))
(define (add-features features)
  (for-each add-feature features))
(define (has-feature? feature)
  (memq feature *features*))

(add-feature 'sisc)

;;hook that gets invoked when SISC is started
(define initialize #f)
(define repl-initialize #f)
(define on-startup #f)
(define on-repl-start
  (let ([repl-start-hooks '()])
    (set! repl-initialize
          (lambda ()
            (for-each (lambda (thunk) (thunk)) 
                      (reverse repl-start-hooks))))
    (lambda (thunk)
      (if (not (procedure? thunk))
          (error "~a is not a procedure" thunk))
      (set! repl-start-hooks (cons thunk repl-start-hooks)))))
      
(let ([*startup-hooks* '()]
      [startup-enabled? #t])
  (set! initialize
        (lambda ()
          (set! startup-enabled? #f)
          (for-each (lambda (thunk) (thunk)) (reverse *startup-hooks*))))
  (set! on-startup
        (lambda (thunk)
          (if (not startup-enabled?)
              (error "on-startup is only callable during heap build"))
          (if (not (procedure? thunk))
              (error "~a is not a procedure" thunk))
          (set! *startup-hooks* (cons thunk *startup-hooks*)))))

;;
(if (not (getprop 'LITE (get-symbolic-environment '*sisc*)))
    (load "../modules/std-modules.scm"))

;;And disable inlining/assumptions
(putprop 'assumptive-procedures '*opt* '())
