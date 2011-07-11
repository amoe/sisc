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

;;turn on syntax expansion and optimization

(define current-optimizer (_make-parameter (lambda (x) x)))

; Define the required environment manipulation, temporarily
(define with-environment _with-environment)

(define compilation-phases
  (let ([old-compile compile])
    (lambda (flags env)
      (define (expand expr)
        (with-environment env
          (lambda () (apply sc-expand expr flags))))
      (define (optimize expr)
        ((current-optimizer) expr))
      (define (analyze expr)
        (_analyze! expr env))
      (define (compile expr)
        (old-compile expr env))
      `((expand     . ,expand)
        (optimize   . ,optimize)
        (analyze    . ,analyze)
        (compile    . ,compile)))))

(define (compile-with-flags expr start-phase flags . env)
  ;;we could use 'compose' from the 'misc' module here, but that
  ;;requires moving it out of that module and into init.scm
  (define (compose . fs)
    (if (null? fs)
        (lambda (x) x)
        (let ([fn (car fs)]
              [tail (apply compose (cdr fs))])
          (lambda (x) (tail (fn x))))))
  (define (select-phases start-phase phases)
    (if (eq? (caar phases) start-phase)
        (map cdr phases)
        (select-phases start-phase (cdr phases))))
  (let ([env (if (null? env) 
                 (interaction-environment)
                 (car env))])
    ((apply compose (select-phases start-phase
                                   (compilation-phases flags env)))
     expr)))

(set! compile
  (lambda (expr . env)
    (apply compile-with-flags expr 'expand '((l) (l)) env)))

(define (eval x . env)
  (let ([phase 'expand])
    (cond [(and (pair? x) (equal? (car x) "noexpand"))
           (set! phase 'compile)
           (set! x (cadr x))]
          [(and (pair? x) (equal? (car x) "analyzeonly"))
           (set! phase 'analyze)
           (set! x (cadr x))]
          [(and (null? env) (strict-r5rs-compliance))
           (error 'eval "expected 2 arguments to procedure, got 1.")])
    (let ([compiled-expr (apply compile-with-flags x phase '((e) (e)) env)])
      (if (null? env) 
          (compiled-expr)
          (with-environment (car env) compiled-expr)))))
        
