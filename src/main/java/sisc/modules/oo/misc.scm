(define <object>)

(define-generic initialize)

(define (initialize-classes)
  (set! <object> (make-class-helper '<object> 'sisc.oo.object-type '() '()))
  (add-method initialize (method ((<object> _)) (void))))

(define (make class . args)
  (let ([obj ((class-constructor class))])
    (apply initialize obj args)
    obj))

;;we don't need a type-of-hook because objects are records and hence
;;the existing record hook is sufficient

(define (oo-type<=-hook next x y)
  (cond [(class? x)
         (if (class? y)
             (if (memq y (class-precedence-list x)) #t #f)
             (type<= <value> y))]
        [(class? y) #f]
        [else (next x y)]))

(define (oo-compare-types-hook next x y c)
  (if (or (class? x) (class? y) (class? c))
      (if (and (class? x) (class? y) (class? c))
          ;;find first occurrence of x or y in c's cpl
          (let loop ([cpl (class-precedence-list c)])
            (if (null? cpl)
                (error 'compare-types
                       (string-append
                        "~a and ~a do not appear in the "
                        "class precedence list of ~a")
                       (class-name x)
                       (class-name y)
                       (class-name c))
                (let ([p (car cpl)])
                  (cond [(eq? p x) 'more-specific]
                        [(eq? p y) 'less-specific]
                        [else (loop (cdr cpl))]))))
          (error 'compare-types
                 "~s or ~s is not a sub-type of ~s"
                 x y c))
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