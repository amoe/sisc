; Helper functions

(define (make-proper p)
  (cond [(null? p) '()]
        [(not (pair? p)) (list p)]
        [else (cons (car p) (make-proper (cdr p)))]))
        
      
(define (union-2 ls1 ls2)
  (cond [(null? ls1) ls2]
        [(memq (car ls1) ls2)
         (union-2 (cdr ls1) ls2)]
        [else (cons (car ls1) (union-2 (cdr ls1) ls2))]))

(define (union ls1 . lses)
  (if (null? lses) ls1
      (apply union (union-2 ls1 (car lses)) (cdr lses))))

(define (constant? e)
  (or (atom? e) 
      (and (pair? e) (= (length e) 2) (eq? (car e) #%quote))))

(define (immediate? e)
  (or (symbol? e)
      (constant? e)))

(define (not-redefined? proc)
  (and (memq proc (getprop 'assumptive-procedures '*opt*))
       (eq? (getprop proc) 
            (getprop proc (sisc-initial-environment)))))

(define-syntax core-form-eq? 
  (syntax-rules ()
    ((_ proc name syntoken)
     (or (eq? proc syntoken)
     	 (and (eq? proc name)
              (eq? (getprop proc (get-symbolic-environment '*sisc-specific*))
                   (getprop proc)))))))

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