;;;;;;;;;; HELPERS ;;;;;;;;;;

(define (fold kons knil lis)
  (let lp ((lis lis) (ans knil))
    (if (null? lis)
        ans
        (lp (cdr lis) (kons (car lis) ans)))))


;;;;;;;;;; REFLECTION PASS-THROUGH ;;;;;;;;;;

(define java-synchronized                       java/synchronized)
(define java-wrap                               java/wrap)
(define java-unwrap                             java/unwrap)
(define java-null                               java/null)
(define java-array-class                        java/array-class)
(define java-proxy-class                        java/proxy-class)
(define java-object?                            java/object?)
(define java-class?                             java/class?)
(define java-interface?                         java/interface?)
(define java-constructor?                       java/constructor?)
(define java-method?                            java/method?)
(define java-field?                             java/field?)
(define java-array?                             java/array?)
(define java-null?                              java/null?)
(define java-array-length                       java/array-length)
(define java-class-declared-classes             java/classes)
(define java-class-declared-constructors        java/constructors)
(define java-class-declared-methods             java/methods)
(define java-class-declared-fields              java/fields)
(define java-class-name                         java/name)
(define java-constructor-name                   java/name)
(define java-method-name                        java/name)
(define java-field-name                         java/name)
(define java-class-flags                        java/modifiers)
(define java-constructor-flags                  java/modifiers)
(define java-method-flags                       java/modifiers)
(define java-field-flags                        java/modifiers)
(define java-class-declaring-class              java/declaring-class)
(define java-constructor-declaring-class        java/declaring-class)
(define java-method-declaring-class             java/declaring-class)
(define java-field-declaring-class              java/declaring-class)
(define java-constructor-parameter-types        java/parameter-types)
(define java-method-parameter-types             java/parameter-types)
(define java-method-return-type                 java/return-type)
(define java-field-type                         java/field-type)


;;;;;;;;;; ARRAYS ;;;;;;;;;;

(define (java-array-new jclass dims)
  (java/array-new jclass
                  (cond [(null? dims)
                         (error "at least one dimension required")]
                        [(pair? dims) dims]
                        [(vector? dims) (vector->list dims)]
                        [else (list dims)])))

(define (java-array-ref array idx)
  (if (null? idx)
      array
      (fold (lambda (idx array) (java/array-ref array idx))
            array
            (cond [(pair? idx)   idx]
                  [(vector? idx) (vector->list idx)]
                  [else          (list idx)]))))

(define (java-array-set! array idx val)
  (define (helper a l)
    (if (null? l)
        (error "at least one index required")
        (let loop ([a a]
                   [l l])
          (let ([head (car l)]
                [tail (cdr l)])
            (if (null? tail)
                (java/array-set! a head val)
                (loop (java/array-ref a head) tail))))))
  (helper array (cond [(null? idx)   idx]
                      [(pair? idx)   idx]
                      [(vector? idx) (vector->list idx)]
                      [else          (list idx)])))


;;;;;;;;;; PRIMITIVE TYPES ;;;;;;;;;;

(define-syntax define-primitive-java-type
  (lambda (x)
    (syntax-case x ()
      ((_ name)
       (with-syntax ((sname (wrap-symbol "<j" (syntax name) ">")))
         (syntax
          (define sname (java/class 'name))))))))

(define-simple-syntax (define-primitive-java-types name ...)
  (begin (define-primitive-java-type name) ...))

(define-primitive-java-types
  void boolean double float long int short byte char)


;;;;;;;;;; EXCEPTION HANDLING ;;;;;;;;;;

(define display-java-stack-trace)

;;(define (initialize-s2j-exception-handling) (void))
(define (initialize-s2j-exception-handling)
  (define-generic-java-methods
    print-stack-trace
    to-string
    close)
  (define-java-classes
    <java.io.string-writer>
    <java.io.print-writer>)
  (set! display-java-stack-trace
    (lambda (java-exception)
      (let* ([sw (java-new <java.io.string-writer>)]
             [pw (java-new <java.io.print-writer> sw)])
        (print-stack-trace java-exception pw)
        (close pw)
        (display (->string (to-string sw))))))
  (print-exception-stack-trace-hook
   'java
   (lambda (next e)
     (if (exception? e)
         (let ([m (error-message (exception-error e))])
           (if (java-object? m)
               (display-java-stack-trace m))))
     (next e))))


;;;;;;;;;; HOOKS ;;;;;;;;;;

(define (java-type-of-hook next o)
  (if (java/object? o)
      (java/class-of o)
      (next o)))

(define (java-type<=-hook next x y)
  (cond [(java/class? x)
         (if (java/class? y)
             (if (memv y (java-class-precedence-list x)) #t #f)
             (type<= <jvalue> y))]
        [(java/class? y) #f]
        [else (next x y)]))

(define (java-compare-types-hook next x y c)
  (if (or (java/class? x) (java/class? y) (java/class? c))
      (if (and (java/class? x)
               (java/class? y)
               (java/class? c))
          ;;find first occurrence of x or y in c's cpl
          (let loop ([cpl (java-class-precedence-list c)])
            (if (null? cpl)
                (error 'compare-types
                       (string-append
                        "~a and ~a do not appear in the "
                        "type precedence list of ~a")
                       (java/name x)
                       (java/name y)
                       (java/name c))
                (let ([p (car cpl)])
                  (cond [(eqv? p x) 'more-specific]
                        [(eqv? p y) 'less-specific]
                        [else (loop (cdr cpl))]))))
          (error 'compare-types
                 (string-append
                  "~s or ~s is not a sub-type of ~s"
                  x y c)))
      (next x y c)))

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