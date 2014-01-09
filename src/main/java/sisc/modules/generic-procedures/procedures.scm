;;;;;;;;;; HELPERS ;;;;;;;;;;

(define (take lis k)
  (let recur ((lis lis) (k k))
    (if (or (zero? k) (null? lis)) ;;added null check here
        '()
        (cons (car lis)
              (recur (cdr lis) (- k 1))))))


;;;;;;;;;; TYPES ;;;;;;;;;;

(define (get-methods proc)
  (or (procedure-property proc 'methods)
      (error "~a is not a generic procedure" proc)))

(define (set-methods! proc methods)
  (set-procedure-property! proc 'methods methods))

(define-nongenerative-struct method-list
  sisc.generic-procedures.method-list-type
  (methods arity cache))

(define (make-method-list-from-methods methods)
  (make-method-list methods
                    (+ (apply max 0 (map method-arity methods)) 1)
                    (make-method-cache)))

(define (generic-procedure-methods proc)
  (method-list-methods (cdr (get-methods proc))))

(define (set-generic-procedure-methods! proc methods)
  (set-methods! proc (cons (mutex/new)
                           (make-method-list-from-methods methods))))

(define (add-method proc method)
  (add-methods proc (list method)))

(define (add-methods proc methods)
  (add-methods-to-list (get-methods proc) methods))

(define (add-methods-to-list methods new-methods)
  (mutex/synchronize-unsafe
   (car methods)
   (lambda ()
     (set-cdr! methods
               (make-method-list-from-methods
                (let loop ([meths (method-list-methods
                                   (cdr methods))]
                           [new-methods new-methods])
                  (if (null? new-methods)
                      meths
                      (loop (add-method-helper meths
                                               (car new-methods))
                            (cdr new-methods)))))))))
(define (add-method-helper methods m)
  (let loop ([meths methods])
    (if (null? meths)
        (cons m methods)
        (if (method= m (car meths))
            (begin
              (set-car! meths m)
              methods)
            (loop (cdr meths))))))


;;;;;;;;;; METHOD CACHE ;;;;;;;;;;

;;What we really want is a hashtable whose keys are lists which are
;;compared element-by-element using eqv?
;;Instead we are using a nested hashtable

(define (make-method-cache)
  (make-hashtable eqv? #f))

(define (method-cache-get cache otypes)
  (if (null? otypes)
      (hashtable/get cache otypes)
      (let ([cache (hashtable/get cache (car otypes))])
        (and cache (method-cache-get cache (cdr otypes))))))

(define (method-cache-put! cache otypes val)
  (if (null? otypes)
      (hashtable/put! cache otypes val)
      (method-cache-put! (hashtable/get! cache
                                         (car otypes)
                                         make-method-cache)
                         (cdr otypes)
                         val)))
        

;;;;;;;;;; DISPATCH ;;;;;;;;;;

;;Returns the list of applicable methods. The applicable methods are
;;returned in a total order based on their specificity with respect to
;;otypes.
;;
;;The algorithm used here is the same as the one employed by CLOS
(define (applicable-methods proc otypes)
  (applicable-methods-wrapper (get-methods proc) otypes take))
(define (applicable-methods-wrapper methods args get-types)
  (mutex/synchronize-unsafe
   (car methods)
   (lambda ()
     (let ([mlist (cdr methods)])
       (let ([cache  (method-list-cache mlist)]
             [otypes (get-types args (method-list-arity mlist))])
         (or (method-cache-get cache otypes)
             (let ([res (applicable-methods-helper
                         (method-list-methods mlist)
                         otypes)])
               (method-cache-put! cache otypes res)
               res)))))))
(define (applicable-methods-helper methods otypes)
  (define (insert applicable m)
    (if (null? applicable)
        (list m)
        (let ([other (car applicable)])
          (case (compare-methods m other otypes)
            [(more-specific)
             (cons m applicable)]
            [(less-specific)
             (cons other (insert (cdr applicable) m))]
            (else '())))))
  ;;optimization opportunity: turn otypes into a vector so that
  ;;method-applicable can do a fast size test. Not much point doing
  ;;this though if we are going to cache the result of this entire
  ;;operation anyway.
  (let loop ([methods    methods]
             [applicable '()])
    (if (null? methods)
        applicable
        (loop (cdr methods)
              (let ([m (car methods)])
                (if (method-applicable? m otypes)
                    (insert applicable m)
                    applicable))))))

(define (call-method-helper applicable args)
  (apply (method-procedure (car applicable))
         (let ([applicable (cdr applicable)])
           (and (not (null? applicable))
                (lambda args (call-method-helper applicable args))))
         args))

(define (limited-type-of args count)
  ;;The unoptimized version of this is
  ;; (map type-of (take args count)
  (if (or (null? args) (zero? count))
      '()
      (cons (type-of (car args))
            (limited-type-of (cdr args) (- count 1)))))

(define (invoke-generic-procedure proc args)
  (let ([applicable (applicable-methods-wrapper (get-methods proc)
                                                args
                                                limited-type-of)])
    (if (null? applicable)
        (error (string-append "no applicable method for args ~s "
                              "in generic procedure ~s")
               args proc)
        (call-method-helper applicable args))))


;;;;;;;;;; CONSTRUCTORS ;;;;;;;;;;

(define (make-generic-procedure . procs)
  (define (proc . args) (invoke-generic-procedure proc args))
  (set-generic-procedure-methods! proc '())
  (add-methods proc (apply append
                           (map generic-procedure-methods procs)))
  (let ([methods (get-methods proc)])
    (for-each (lambda (p) (set-methods! p methods)) procs))
  proc)

(define-simple-syntax (define-generic ?name . ?rest)
  (define ?name (make-generic-procedure . ?rest)))

(define-syntax define-generics
  (syntax-rules ()
    [(_ (?name . ?procs) . ?rest)
     (begin (define-generic ?name . ?procs)
            (define-generics . ?rest))]
    [(_ ?name . ?rest)
     (begin (define-generic ?name)
            (define-generics . ?rest))]
    [(_)
     (begin)]))

(define-simple-syntax (define-method (?name . ?signature) . ?body)
  (module () (add-method ?name (method ?signature . ?body))))

(define-simple-syntax (define-methods ?name (?signature . ?body) ...)
  (module () (add-methods ?name (list (method ?signature . ?body) ...))))

(define (first-method-procedure generic types)
  (let ([methods (applicable-methods generic types)])
    (let ([first-method (car methods)]
          [next-method (if (null? (cdr methods)) #f (cadr methods))])
      (let ([proc (method-procedure first-method)])
        (lambda temps 
          (apply proc next-method temps))))))

(define-syntax let-monomorphic
  (lambda (x)
    (syntax-case x ()
      ((_ () expr ...)
       (syntax (let () expr ...)))
      ((_ (((aka fun) type ...) rest ...) expr ...)
       (with-syntax ([(temps ...)
                      (generate-temporaries (syntax (type ...)))])
         (syntax
          (let ([aka (first-method-procedure fun (list type ...))])
            (let-monomorphic (rest ...) expr ...)))))
      ((_ ((fun type ...) rest ...) expr ...)
       (syntax
        (let-monomorphic (((fun fun) type ...) rest ...) expr ...))))))
         
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