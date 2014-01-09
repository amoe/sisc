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
;;; compat.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

;;; This file contains nonstandard help procedures.
;;; They are all present in Chez Scheme, but are easily defined
;;; in any standard Scheme system.
;;; These versions do no error checking.

; In Chez Scheme "(void)" returns an object that is ignored by the
; REP loop.  It is returned whenever a "nonspecified" value is specified
; by the standard.  The following should pick up an appropriate value.

;(define void
;   (let ((void-object (if #f #f)))
;      (lambda () void-object)))

; "andmap" is like "map" except instead of "consing" the results it
; "ands" them, quitting early if #f" is obtained.
; The following does no error checking.
(define andmap
  (lambda (f first . rest)
    (or (null? first)
        (if (null? rest)
            (let andmap ((first first))
              (let ((x (car first)) (first (cdr first)))
                (if (null? first)
                    (f x)
                    (and (f x) (andmap first)))))
            (let andmap ((first first) (rest rest))
              (let ((x (car first))
                    (xr (map car rest))
                    (first (cdr first))
                    (rest (map cdr rest)))
                (if (null? first)
                    (apply f (cons x xr))
                    (and (apply f (cons x xr)) (andmap first rest)))))))))

(define (gen-sym base)
  (if base
      (string->symbol (string-append (symbol->string (gensym)) "_"
                                     (symbol->string base)))
      (gensym)))

(define (ormap proc list1)
  (and (not (null? list1))
       (or (proc (car list1)) (ormap proc (cdr list1)))))

(define (remq o lst)
  (cond [(null? lst) '()]
        [(eq? o (car lst)) (remq o (cdr lst))]
        [else (cons (car lst) (remq o (cdr lst)))]))

(define $sc-put-cte)
(define identifier?)
(define sc-expand)
(define datum->syntax-object)
(define syntax-object->datum)
(define generate-temporaries)
(define free-identifier=?)
(define bound-identifier=?)
(define literal-identifier=?)
(define syntax-error)
(define $syntax-dispatch)
(define $make-environment)

(define (throw . args) (apply error args))

(define (error . args)
  (for-each (lambda (arg)
              (display arg) (display #\space))
            args)
  (newline))

(define strict-r5rs-compliance (_make-native-parameter "strictR5RSCompliance"))
  
(define (atom? v)
  (not (or (pair? v) (symbol? v))))
            
(define (make-false v) #f)

;;;;;;;;;;;;; Module loading

(if (not (getprop 'LITE (get-symbolic-environment '*sisc*)))
    (for-each load-module '("sisc.modules.OptionalPrimitives$Index")))
