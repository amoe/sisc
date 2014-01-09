;;a method is a procedure. It has an arity and a signature (a list of
;;parameter types) and flag indicating whether it can take rest
;;parameters
(define-nongenerative-record-type <method> sisc.generic-procedures.method-type
  (_make-method procedure arity types rest?)
  method?
  (procedure    method-procedure        set-method-procedure!)
  (arity        method-arity            set-method-arity!)
  (types        method-types            set-method-types!)
  (rest?        method-rest?            set-method-rest!))

(define (make-method f types rest?)
  (_make-method f (length types) types rest?))

(define (method= m1 m2)
  (and (= (method-arity m1) (method-arity m2))
       (eq? (method-rest? m1) (method-rest? m2))
       (types= (method-types m1) (method-types m2))))

;;Checks whether a method can be called with arguments of specific
;;types.
(define (method-applicable? m otypes)
  (let ([l (length otypes)]
        [a (method-arity m)])
    (and (or (= a l) (and (< a l) (method-rest? m)))
         (types<= otypes (method-types m)))))

;;compares two methods m1, m2, taking into account the cpls of otypes
;;NB: we assume that both methods are applicable, i.e. otypes matches
;;both their signature.
(define (compare-methods m1 m2 otypes)
  (let loop ([m1-t (method-types m1)]
             [m2-t (method-types m2)]
             [o-t  otypes])
    (cond [(and (null? m1-t) (null? m2-t))
           (let ([r1 (method-rest? m1)]
                 [r2 (method-rest? m2)])
             (cond [(eq? r1 r2) 'equal]
                   [r1 'less-specific]
                   [r2 'more-specific]))]
          [(null? m1-t) 'less-specific]
          [(null? m2-t) 'more-specific]
          [else
            (let ([m1-tn (cdr m1-t)]
                  [m2-tn (cdr m2-t)]
                  [o-tn  (cdr o-t)])
              (case (compare-types (car m1-t) (car m2-t) (car o-t))
                [(less-specific) 'less-specific]
                [(more-specific) 'more-specific]
                [(equal) (loop m1-tn m2-tn o-tn)]))])))

(define-syntax method
  (syntax-rules (next:)
    [(_ ((next: ?next) (?type ?arg) ...) . ?body)
     (make-method (lambda (?next ?arg ...) . ?body)
                  (list ?type ...)
                  #f)]
    [(_ ((next: ?next) (?type ?arg) ... . ?rest) . ?body)
     (make-method (lambda (?next ?arg ... . ?rest) . ?body)
                  (list ?type ...)
                  #t)]
    [(_ ((next: ?next) . ?rest) . ?body)
     (error "parameters must be typed")]
    [(_ ?rest . ?body)
     (method ((next: dummy) . ?rest) . ?body)]))

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