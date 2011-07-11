;; Miscellaneous useful pieces of code that are not big enough
;; for justifying a separate module

;; create a new symbol by pre and postfixing an existing symbol. Use
;; like this:
;; (define-syntax wrap
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ name)
;;        (with-syntax ([nname (wrap-symbol "<" (syntax name) ">")])
;;          (syntax nname))))))
;; (define <foo> 1)
;; (wrap foo) ;=> 1
(define (wrap-symbol pre symb app)
  (datum->syntax-object
   symb
   (string->symbol
    (string-append pre
                   (symbol->string
                    (syntax-object->datum symb))
                   app))))

;; Al Petrofsky's dynamic-freeze.
;; dynamic-freeze returns a procedure whose body is always evaluated
;; in the dynamic context of the procedure's creation, rather than
;; that of its application. I.e., when it is applied, it restores the
;; dynamic context of the call to dynamic-freeze and passes its
;; arguments to proc. The dynamic context of the call to the frozen
;; procedure is then restored and the result(s) of the call to proc
;; are returned.
(define (dynamic-freeze proc)
  ((call-with-current-continuation
       (lambda (freeze-k)
         (lambda ()
           (lambda args
             (call-with-current-continuation
                 (lambda (call-k)
                   (freeze-k
                    (lambda ()
                      (call-with-values (lambda () (apply proc args))
                        call-k)))))))))))

;; An implementation of define-values that exploits psyntax's module
;; system in order to allow appearance of the macro in any place where
;; a definition can occur.
;; The code is based on a c.l.s postings by Andrew Wilcox and
;; Abdulaziz Ghuloum.
(define-syntax define-values
  (lambda (ctx)
    (syntax-case ctx ()
      [(_ (id? ...) e)
       (with-syntax ([(tmp? ...) (generate-temporaries #'(id? ...))])
         #'(module (id? ...)
               (define id? (void)) ...
               (call-with-values (lambda () e)
                 (lambda (tmp? ...)
                   (set! id? tmp?) ...))))])))

;; Nice macro to make the other macros look like simple function defines.
;; e.g.
;; (define-simple-syntax (when condition body ...)
;;   (if condition
;;       (begin body ...)))
(define-syntax define-simple-syntax
  (syntax-rules ()
    ((_ (name . args) body ...)
     (define-syntax name
       (syntax-rules ()
         ((name . args)
          (begin body ...)))))))

;; function composition
;; This version handles multi-value returns and is optimized for
;; application. Construction is tail recursive.
(define (compose . fs)
  (let loop ((g values) (fs fs))
    (if (null? fs)
        g
        (loop (let ([f (car fs)])
                (lambda args
                  (call-with-values
                      (lambda () (apply f args))
                    g)))
              (cdr fs)))))

;; TOTAL-ORDER computes a total order from a set of partial orders,
;; e.g.
;; (total-order '((a d) (b c) (c d))) ;=> '(a b c d)
;; #f is returned if no such total order exists
;;
;; Optionally, we handle weak partial orders, i.e. we allow the total
;; ordering to perform some re-ordering of the partial orders as long
;; as the strong ordering constraint (determined by a predicate) is
;; preserved.

(define (any pred l)
  (and (pair? l) (or (pred (car l)) (any pred (cdr l)))))
(define (filter pred l)
  (if (pair? l)
      (if (pred (car l))
          (cons (car l) (filter pred (cdr l)))
          (filter pred (cdr l)))
      l))
(define (remove pred l) (filter (lambda (x) (not (pred x))) l))
(define (weak-partial-order-select partial-orders pred)
  (define (weakly-ordered? partial-order c)
    (let loop ([processed '()]
               [partial-order partial-order])
      (or (null? partial-order)
          (if (eqv? (car partial-order) c)
              (not (any (lambda (x) (pred x c)) processed))
              (loop (cons (car partial-order) processed)
                    (cdr partial-order))))))
  (define (all-weakly-ordered? partial-orders c)
    (or (null? partial-orders)
        (and (weakly-ordered? (car partial-orders) c)
             (all-weakly-ordered? (cdr partial-orders) c))))
  (let loop ([remaining partial-orders])
    (and (not (null? remaining))
         (let ([c (caar remaining)])
           (if (all-weakly-ordered? partial-orders c)
               c
               (loop (cdr remaining)))))))
(define (total-order partial-orders . rest)
  (define (merge result remaining)
    (define (find-next l)
      (let ([c (car l)])
        (and (not (any (lambda (l) (memv c (cdr l))) remaining))
             c)))
    (set! remaining (remove null? remaining))
    (if (null? remaining)
        (reverse result)
        (let ([next (any find-next remaining)])
          (if next
              (merge (cons next result)
                     (map (lambda (l)
                            (if (eqv? (car l) next) (cdr l) l))
                          remaining))
              (and (not (null? rest))
                   (car rest)
                   (let ([c (weak-partial-order-select remaining
                                                       (car rest))])
                     (and c
                          (merge (cons c result)
                                 (map (lambda (l)
                                        (remove (lambda (x)
                                                  (eqv? x c)) l))
                                      remaining)))))))))
  (merge '() partial-orders))

(define (type-safe-intern type-check guid value)
  (call-with-values
      (lambda () (intern guid value))
    (lambda (new-guid new-value)
      (cond
        [(and (eq? new-guid guid) (eq? new-value value))
         value]
        [(eq? new-guid guid)
         (if (type-check new-value)
             new-value
             (error (string-append
                     "guid already in use for ~a, "
                     "which is not of the correct type")
                    new-value))]
        [(eq? new-value value)
         ;;this should never happen, since value is supposed to be
         ;;fresh
         (error "something bad happened")]
        [else
          ;;this should never happen, since value is supposed to be
          ;;fresh
          (error "something bad happened")]))))

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