(define (opt:lambda keyword formals body state)
  (let-values ([(rv state)
                (opt body
                     (union-state-entry*
                      state
                      'lvars
                      (cond [(list? formals)
                             formals]
                            [(pair? formals)
                             (make-proper formals)]
                            [else (list formals)])))])
    (values (list keyword formals rv) state)))

;;Constant Propogation (always safe)
; This function is complicatedly thorough

(define cp-candidates 
  (lambda (formals values* state rec)
    (let ((set-vars (or (get-state-entry state 'set-vars) '()))
          (refed-vars (or (get-state-entry state 'refed-vars) '()))
          (free-vars (or (get-state-entry state 'free-vars) '()))
          (nf '())
          (nv '())
          (sec '()))
      (define (cp-helper x y acc)
        ;Watch for null? x but not null? y, or vice versa, indicating
        ;argument count mismatch.  If we find one, don't optimize
        (cond [(null? x) (if (null? y) acc '())]
              [(null? y) '()] 
              [else 
                (let ((cx (car x))
                      (cy (car y)))
                  (cond 
                    ;; If this var is never referenced, add its rhs to 
                    ;; the side-effect-only list
                    [(not (memq cx refed-vars))
                     (set! sec (cons cy sec))
                     (cp-helper (cdr x) (cdr y) acc)]
                    ;; If this var gets set!'ed, or its right-hand side
                    ;; is non-immediate, or is the same var in a letrec,
    	    ;; skip it
                    [(or (not (immediate? cy))
                         (and set-vars (memq cx set-vars))
    		 (and rec (eq? cx cy)))
                     (set! nf (cons cx nf))
                     (set! nv (cons cy nv))
                     (cp-helper (cdr x) (cdr y) acc)]
                    ;; If this var is bound to another var ref,
                    ;; see if it too is bound to a cp-candidate,
                    ;; and use that value instead of the var.
                    [(and rec
                          (symbol? cy)
                          (assq cy acc)) =>
                          (lambda (vref)
                            (let vloop ((v (cdr vref)))
                              ;;Watch that we never have a chain leading
                              ;;to an existing var
                              (cond [(eq? v cx)
                                     (error 'optimizer "optimizer detected circular variable assignment")]
                                    ;; Reached an immediate or non-var
                                    [(not (symbol? v)) 
                                     (cp-helper (cdr x)
                                                (cdr y)
                                                (cons (cons cx v) acc))]
                                    [(assq v acc) =>
                                     (lambda (vref2)
                                       (vloop (cdr vref2)))]
                                    [else (cp-helper (cdr x)
                                                     (cdr y)
                                                     (cons (cons cx v) acc))])))]
                    ;; If the right hand side is a var-ref, 
                    ;; but its set!'ed or a free variable, skip it.
                    [(and (symbol? cy) 
                          (or (memq cy set-vars)
                              (memq cy free-vars)))
                     (set! nf (cons cx nf))
                     (set! nv (cons cy nv))
                     (cp-helper (cdr x) (cdr y) acc)]
                    ;; If the right-hand-side is a var-ref and 
                    ;; that var is not set! in the body, we can
                    ;; do a simple variable renaming.
                    [(and (symbol? cy) 
                          (or (and rec (memq cy formals))
                              (memq cy (or (get-state-entry state 'lvars)
                                           '()))))
                     (cp-helper (cdr x)
                                (cdr y) 
                                (cons (cons cx cy) acc))]
                    [else 
                      (if rec
                          (begin
                            (let vloop ((v acc))
                              (unless (null? v) 
                                (if (eq? (cdr v) cx)
                                    (set-cdr! v cy))))
                            (set! nf (cons cx nf))
                            (set! nv (cons cy nv))))
                      (cp-helper (cdr x)
                                 (cdr y) 
                                 (cons (cons cx cy) acc))]))]))
      (let ((cpc (cp-helper formals values* '())))
        (if (and (null? cpc) (null? sec))
            (values formals values* cpc sec)
            (values nf nv cpc sec))))))
  
(define (opt:letrec-helper formals values* state)
  (let ((state (union-state-entry* state 'lvars formals)))
    (let-values ([(nf nvo cpc sec)
                  (cp-candidates formals values* state #t)])
      (let ([state (if (null? cpc)
                       state
                       (union-state-entry* state 'constant-prop cpc))])
        (if (null? cpc)
            (values nf nvo sec state)
            (let-values ([(nv fstate*)
                          (let ([x (map (lambda (e)
                                          (let-values ([(rv state) (opt e state)])
                                            (cons rv state)))
                                        nvo)])
                            (values (map car x) (map cdr x)))])
              (if (= (length nv) (length values*))
                  (values nf nvo sec state)
                  (opt:letrec-helper
                   nf nv (merge-states 
                          state 
                          (apply merge-states fstate*))))))))))

(define (opt:letrec keyword formals values* body state)
  (let-values ([(nf nv sec state) (opt:letrec-helper formals values* state)])
    (let-values ([(newbody bstate) (opt body state)])
      (values
       (apply make-begin 
              (append sec 
                      (list
                       (if (null? nf)
                           newbody
                           `(,keyword ,(map list nf nv) ,newbody)))))
       (merge-states state bstate)))))

(define (opt:let keyword formals values* body state)
  (let ((state (union-state-entry* state 'lvars formals)))
    (let-values ([(nf nv cpc sec) (cp-candidates formals values* state #f)])
      (let-values ([(rv state)
                    (opt body (if (null? cpc)
                                  state
                                  (union-state-entry* state 'constant-prop
                                                      cpc)))])
        (values
         (apply make-begin 
                (append sec
                        (list
                         (if (null? nf)
                             rv
                             `((,keyword ,nf ,rv) ,@nv)))))
         state)))))

(define (opt:ref ref state)
  (let ((cp (get-state-entry state 'constant-prop)))
    (cond [(and cp (assq ref cp)) =>
           (lambda (v)
             (opt (cdr v) state))]
          [else (values ref (new-state))])))

(define (opt:if keyword test conseq altern state)
  (match test
    ;;Branch elimination (always safe)
    (,x
     (guard (constant? x))
     (values 
      (if x conseq altern)
      (new-state)))
    ;;Various lookahead opts
    ((,?if ,B #f #f)     
     (guard (core-form-eq? ?if 'if #%if))
     (values 
      (make-begin B altern)
      (new-state)))
    ((,?if ,B #f ,x)
     (guard (and (constant? x)
                 (core-form-eq? ?if 'if #%if)))
     (opt:if keyword B altern conseq state))
    ((,?if ,B ,x #f)
     (guard (and (constant? x) (core-form-eq? ?if 'if #%if)))
     (opt:if keyword B conseq altern state))
    ((,?if ,B ,x ,y)
     (guard (and (constant? x) (constant? y) (core-form-eq? ?if 'if #%if)))
     (values 
      (make-begin B conseq)
      (new-state)))
    ;;Or optimization
    (((,?lambda (,var) (,?if ,itest ,iconseq ,ialtern))
      ,val)
     (guard (and (core-form-eq? ?if 'if #%if) 
                 (core-form-eq? ?lambda 'lambda #%lambda)
                 (eq? itest var)
                 (eq? iconseq itest)))
      (opt:if #%if `(#%if ,val #t ,ialtern)
              conseq altern state))
    ;;Begin lifting (possibly unsafe)
    ((,?begin ,e* ... ,el)
     (guard (core-form-eq? ?begin 'begin #%begin))
     (let-values ([(rv state) (opt:if keyword el conseq altern state)])
       (values (apply make-begin (append e* `(,rv)))
               (merge-states state '((new-assumptions begin))))))
    (,other (values `(,keyword ,other ,conseq ,altern) (new-state)))))

;; Applications and constant folding (possibly unsafe)
(define opt:application 
  (lambda (rator rands state)
    (cond [(and (eq? rator 'not)
                (not-redefined? 'not)
                (= (length rands) 1))
           (let-values ([(rv state) (opt:if '#%if (car rands) #f #t state)])
             (values rv (merge-states state '((new-assumptions not)))))]
          [(and (symbol? rator)
                (not-redefined? rator)
                (andmap constant? rands))
           (values (eval `("noexpand" (,rator ,@rands)))
                   `((new-assumptions ,rator)))]
          [else 
            (values `(,rator ,@rands) (new-state))))))

;; begin flattening, constant elimination, and collapse
(define (mb-helper exp1 . exps*)
  (if (null? exps*)
      (if (and (pair? exp1)
               (core-form-eq? (car exp1) 'begin '#%begin))
          (apply mb-helper (cdr exp1))
          (list exp1))
      (match exp1
        ;; Eliminate constants and variable references
        ;; in command context.
        (,x
         (guard (or (constant? x) (symbol? x)))
         (apply mb-helper exps*))
        ;; Eliminate procedures in command context
        ((lambda ,formals ,body)
         (apply mb-helper exps*))
        ;; Flatten nested begins
        ((,?begin ,sexps* ...)
         (guard (core-form-eq? ?begin 'begin #%begin))
         (let ((sub-begin (apply mb-helper sexps*)))
           (append sub-begin 
                   (apply mb-helper exps*))))
        (,other (cons other (apply mb-helper exps*))))))

(define (make-begin exp1 . exps*)
  (if (null? exps*) 
      exp1
      (let ((result (apply mb-helper exp1 exps*)))
        (if (and (pair? result)
                 (= (length result) 1))
            (car result)
            (cons #%begin result)))))
                   

(define (opt:begin exp1 exps* state)
  (values
   (apply make-begin exp1 exps*)
   (new-state)))

(define (opt:set! keyword lhs rhs state)
  (match rhs
    ;; Begin lifting
    ((begin ,e* ... ,el)
;     (guard (core-form-not-redefined? 'begin))
     (values
      (apply make-begin (append e* `((,keyword ,lhs ,el))))
      '((new-assumptions 'begin))))
    (,else
      (values
       `(,keyword ,lhs ,else)
       (new-state)))))

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
