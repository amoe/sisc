(define *VOID-MARKER* (list #f))

(define *HASH-PROCS* `((,eq?         . ,hash-by-eq)
                       (,eqv?        . ,hash-by-eqv)
                       (,equal?      . ,hash-by-equal)
                       (,string=?    . ,hash-by-string=)
                       (,string-ci=? . ,hash-by-string-ci=)))

;;(make-hashtable [eq-proc [hash-proc]] [safe? [weak?]])
(define (make-hashtable . rest)
  (define (process-opt-args)
    (let ([eq-proc equal?]
          [hash-proc hash-by-equal]
          [safe? #t]
          [weak? #f])
      (if (not (null? rest))
          (begin
            (if (procedure? (car rest))
                (begin (set! eq-proc (car rest))
                       (set! hash-proc
                         (cond [(assq eq-proc *HASH-PROCS*) => cdr]
                               [else hash-by-equal]))
                       (set! rest (cdr rest))))
            (if (not (null? rest))
                (begin
                  (if (procedure? (car rest))
                      (begin (set! hash-proc (car rest))
                             (set! rest (cdr rest))))
                  (if (not (null? rest))
                      (begin
                        (set! safe? (car rest))
                        (set! rest (cdr rest))
                        (if (not (null? rest))
                            (begin
                              (set! weak? (car rest))
                              (set! rest (cdr rest))
                              (if (not (null? rest))
                                  (error 'make-hashtable
                                         "expected 0-4 args"))))))))))
      (values eq-proc hash-proc safe? weak?)))
  (call-with-values process-opt-args hashtable/make))

(define (alist->hashtable alist . rest)
  (hashtable/add-alist! (apply make-hashtable rest) alist))

(define (hashtable/get! ht key thunk . rest)
  (define (helper)
    (let* ([def (list #f)]
           [res (hashtable/get ht key def)])
      (if (eq? res def)
          (let ([res (thunk)])
            (hashtable/put! ht key res)
            res)
          res)))
  (if (hashtable/thread-safe? ht)
      ((if (or (null? rest) (car rest))
           synchronized
           synchronized-unsafe)
       ht
       helper)
      (helper)))

(define (hashtable/contains? ht key)
  (not (eq? (hashtable/get ht key *VOID-MARKER*) *VOID-MARKER*)))

;;The following are quite inefficient. To get around that we'd have
;;to expose java collection iterators.
(define (hashtable/for-each proc ht)
  (for-each (lambda (x) (proc (car x) (cdr x))) (hashtable->alist ht)))
(define (hashtable/map proc ht)
  (map (lambda (x) (proc (car x) (cdr x))) (hashtable->alist ht)))


(define <hashtable> (make-type '|sisc.modules.hashtable.HashtableBase|))

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