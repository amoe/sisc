(define-syntax self-evaluating?
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (not (or (pair? x) (vector? x) (and (null? x)
                                           (strict-r5rs-compliance))))))))
                                           
(define (opt:optimize e)
  (let-values ([(rv state) (opt e (scan e '() ()))])
    rv))

(define (opt e state)
  (match e
    (,x
     (guard (symbol? x))
     (opt:ref x state))
    (,x
     (guard (constant? x))
     (values x (new-state)))
    ((#%quote ,x) 
     (values `(#%quote ,x) (new-state)))
    #|      ((,?program ,nrefv ,nsetv ,nfreev ,expr)
             (guard (core-form-eq? ?program 'program #%program))
             (let* ([state (make-state (state-get state cpc) nfreev nrefv nsetv)]
                    [rv (opt expr state)])
               `(#%program ,(trim (state-get state refv))
                           ,(trim (state-get state setv))
                           ,(state-get state freev)
                           ,rv)))|#
    ((,?begin ,[exp1 exp1-state] ,[exps* exps-state*] ...)
     (guard (core-form-eq? ?begin 'begin #%begin))
     (let-values ([(rv state) (opt:begin exp1 exps* state)])
       (values rv (merge-states state exp1-state 
                                (apply merge-states exps-state*)))))
    ((,?if ,[test test-state] ,[conseq conseq-state] ,[altern altern-state])
     (guard (core-form-eq? ?if 'if #%if))
     (let-values ([(rv state) (opt:if ?if test conseq altern state)])
       (values rv (merge-states state test-state conseq-state altern-state))))
    ;; Lets
    (((,?lambda ,formals ,body) ,[values* values-state*] ...)
     (guard (and (core-form-eq? ?lambda 'lambda #%lambda)                    
                 (list? formals)))
     (if (= (length formals) (length values*))
         (let-values ([(rv state) (opt:let ?lambda formals values* body state)])
           (values rv (merge-states state (apply merge-states values-state*))))
         (let-values ([(l lstate) (opt:lambda ?lambda formals body state)])
           (values `(,l ,@values*)
                   (apply merge-states values-state*)))))
    ((,?lambda/letrec #t ,formals ,meta ,body)
     ; An already analyzed expression, skip it.  This can occur when
     ; psyntax pre-expands some forms in complicated syntax-case expressions.
     ; The analyzer also must deal with expressions that contain a mix of
     ; analyzed and unanalyzed code.  Only lambda/letrec have a different structure
     ; in analyzed vs unanalyzed code, hence only the distinction here.
     (guard (or (core-form-eq? ?lambda/letrec 'lambda #%lambda)
                (core-form-eq? ?lambda/letrec 'letrec #%letrec)))
     (values `(,?lambda/letrec #t ,formals ,meta ,body) (new-state)))
    ((,?lambda ,formals ,body)
     (guard (core-form-eq? ?lambda 'lambda #%lambda))
     (opt:lambda ?lambda formals body state))
    ((,?letrec ((,lhs* ,[rhs* rhs-state*]) ...) ,body)
     (guard (core-form-eq? ?letrec 'letrec #%letrec))
     (let-values ([(rv state) (opt:letrec ?letrec lhs* rhs* body state)])
       (values rv (merge-states state (apply merge-states rhs-state*)))))
    ((,?define ,lhs ,[rhs rhs-state])
     (guard (core-form-eq? ?define 'define #%define))
     (values `(,?define ,lhs ,rhs) rhs-state))
    ((,?set! ,formal ,[value vstate])
     (guard (core-form-eq? ?set! 'set! #%set!))
     (let-values ([(rv state) (opt:set! ?set! formal value state)])
       (values rv (merge-states state vstate))))     
    ((,?annotate ,[expr expr-state] ,annotation)
     (guard (core-form-eq? ?annotate 'annotate #%annotate))
     (values `(,?annotate ,expr ,annotation)
             expr-state))
    ((,[rator rator-state] ,[rands* rands-state*] ...)
     (let-values ([(rv state) (opt:application rator rands* state)])
       (values rv (merge-states state rator-state 
                                (apply merge-states rands-state*)))))
    (,other (error 'optimizer "Unrecognized s-expression: ~a" other))))

(define (scan expr lvars ulrvars)
  (match expr
    (,x 
     (guard (symbol? x))
     (when (memq x ulrvars)
     	 (if (strict-r5rs-compliance)
     	     (error 'compile "naked left hand reference in letrec rhs: '~a'.}~%" x)
             (display (format "{warning: naked left-hand reference in letrec rhs: '~a'.}~%" x))))
     (cons `(refed-vars ,x)
           (if (memq x lvars) '()
               `((free-vars ,x)))))
    (,x
     (guard (constant? x))
     (new-state))
    ((#%quote ,x) 
     (new-state))
#|    ((,?program ,nrefv ,nsetv ,nfreev ,[expr])
     (guard (core-form-eq? ?program 'program #%program))
     expr)|#
    ((,?begin ,[exp1] ,[exps*] ...)
     (guard (core-form-eq? ?begin 'begin #%begin))
     (merge-states exp1 (apply merge-states exps*)))
    ((,?if ,[test] ,[conseq] ,[altern])
     (guard (core-form-eq? ?if 'if #%if))
     (merge-states test conseq altern))
    (((,?lambda ,formals ,body) ,[values*] ...)
     (guard (core-form-eq? ?lambda 'lambda #%lambda))
     (merge-states (scan body (append lvars (make-proper formals)) 
                         ulrvars)
                   (apply merge-states values*)))
    ((,?lambda #t ,formals ,meta ,body)
     (guard (core-form-eq? ?lambda 'lambda #%lambda))
     (new-state))
    ((,?lambda ,formals ,body) 
     (guard (core-form-eq? ?lambda 'lambda #%lambda))
     (scan body (append lvars (make-proper formals)) '()))
    ((,?letrec #t ,bindings ,meta ,body)
     (guard (core-form-eq? ?letrec 'letrec #%letrec))
     (new-state))     
    ((,?letrec ((,lhs* ,rhs*) ...) ,body)
     (guard (core-form-eq? ?letrec 'letrec #%letrec))
     (let ([nlv (append lvars lhs*)]
           [nurlv (append lhs* ulrvars)])
       (merge-states (apply merge-states (map (lambda (rhs)
                                                (scan rhs nlv nurlv))
                                              rhs*))
                     (scan body nlv ulrvars))))
    ((,?define ,lhs ,[rhs]) 
     (guard (core-form-eq? ?define 'define #%define))
     rhs)
    ((,?set! ,formal ,[value]) 
     (guard (core-form-eq? ?set! 'set! #%set!))
     (union-state-entry
      (union-state-entry value 'set-vars formal)
      'refed-vars formal))
    ((,?annotate ,[expr] ,annotation) 
     (guard (core-form-eq? ?annotate 'annotate #%annotate))
     expr)
    ((,[rator] ,[rands*] ...)
     (merge-states rator (apply merge-states rands*)))
    (,other (error 'optimizer "Unrecognized s-expression: ~a" other))))

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

