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
;; The SISC read-eval-print-loop

; Most of the REPL is parameterized, here we define those
; parameters
(define _exit-handler (make-parameter '()))
(define current-exit-handler (make-parameter (lambda (rv) rv)))

(define current-default-error-handler
  (make-parameter
   (lambda (m e)
     (let ([exception (make-exception m e)])
       (putprop 'last-exception '*debug* exception)
       (print-exception exception (stack-trace-on-error))))))

(define current-writer
  (make-parameter (lambda (v)
                    (if (and (procedure? (getprop 'pretty-print))
                             (not (getprop '*sisc* 'LITE))
                             (not (circular? v)))
                        (pretty-print v)
                        ;;dynamic wind would be better here, but
                        ;;we don't want to use it in core code
                        (let ([ps (print-shared)])
                          (print-shared #t)
                          (write v)
                          (print-shared ps))))))

(define repl-prompt
  (make-config-parameter "replPrompt" (lambda (repl-depth)
                                        (format "#;~a> "
                                                (if (zero? repl-depth)
                                                    "" repl-depth)))))
(define stack-trace-on-error
  (make-config-parameter "stackTraceOnError" #t))

(define (get-last-exception)
  (getprop 'last-exception '*debug*))

(define repl-thread
  (let ((thread #f))
    (lambda arg
      (if (null? arg)
          thread
          (let ((old thread))
            (set! thread (car arg))
            old)))))
  
(define make-interrupt-handler
  (if (not (getprop 'LITE (get-symbolic-environment '*sisc*)))
      (lambda ()
        ; We use absolute references to threading here so that this can
        ; coexist with the lite and full dists without using import
        (repl-thread (|@threading-native::thread/current|))
        (letrec ([handler
                  (lambda ()
                    (_signal-unhook! "INT" handler)
                    (if (repl-thread)
                        (|@threading-native::thread/interrupt| (repl-thread))))])
          handler))
      (lambda () #f)))

(define (repl)
  (define (repl/read)
    (let ([handler (and (permit-interrupts)
                        (make-interrupt-handler))])
      (define (repl/eval exp)
        (let ([compiled (compile-with-flags exp 'expand '((e) (e))
                                            (interaction-environment))])
          (when handler (_signal-hook! "INT" handler))
          (with/fc
              (lambda (m e)
                (when handler (_signal-unhook! "INT" handler))
                (throw m e))
            (lambda () 
              (let ([val (compiled)])
                (when handler (_signal-unhook! "INT" handler))
                val)))))

      ;; Display the prompt
      (let ([rp (repl-prompt)]
            [repl-depth (- (length (_exit-handler)) 1)])
        (display (if (procedure? rp)
                     (rp repl-depth)
                     rp)))
                
      ;;read
      (let ([exp (read-code (current-input-port))])
        (if (eof-object? exp) 
            (if ((current-exit-handler) exp)
                (exit exp)
                (repl/read))
            (begin
              ;; Consume any whitespace
              (let loop ()
                (when (and (char-ready? (current-input-port))
                           (char-whitespace? (peek-char)))
                  (read-char)
                  (loop)))
                        
              ;;eval
              (let ([val (repl/eval exp)])
                ;;print
                (if (not (void? val))
                    (begin ((current-writer) val) (newline))))
              ;;loop
              (repl/read))))))
  (let ([repl-start #f])
    (repl-initialize)
    (or ((current-exit-handler)
         (call/cc
          (lambda (k)
            (_exit-handler (cons k (_exit-handler)))
            (begin
              (let ([kret
                     (call/cc (lambda (k) 
                                (set! repl-start k)
                                (putprop 'repl '*debug* k)))])
                ; The repl-return continuation can optionally
                ; be passed a thunk which is executed
                ; *in this dynamic environment*
                (if (procedure? kret)
                    (kret)))
              (let loop ()
                (with/fc (lambda (m e)
                           ((current-default-error-handler) m e)
                           (loop))
                  (lambda ()
                    (repl/read)
                    (void))))))))
        (repl-start))))

(define (sisc-cli)
  (display (format "SISC (~a)\n"
                   (getprop 'version (get-symbolic-environment '*sisc*))))
  (let loop ()
    (with/fc (lambda (m e)
               (display (format "Uncaught error: ~a~%Please report this error to sisc-devel@lists.sourceforge.net~%" m))
               (if (pair? (_exit-handler))
                   (_exit-handler (cdr (_exit-handler))))
               (loop))
      repl)))

(define (exit . return-value)
  (if (not (pair? (_exit-handler)))
      (error 'exit "not in a read-eval-print-loop."))
  (let ([k (car (_exit-handler))]
        [rv (if (null? return-value) #!void (car return-value))])
    (_exit-handler (cdr (_exit-handler)))
    (newline)
    (if k (k rv) rv)))

(on-repl-start
 (lambda ()
   (source-annotations '((source-kind . user)))
   (current-url
    (string-append
     "file:"
     (let ([dir (getenv "user.dir")])
       (if dir
           (string-append dir (getenv "file.separator"))
           "."))))))
