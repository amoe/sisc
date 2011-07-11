;; Datastructure support for the optimizer
(define-syntax new-state
  (syntax-rules ()
    ((_) '())))
     
(define (get-state-entry state key)
  (cond [(assq key state) => cdr]
        [else #f]))

(define (set-state-entry state key value)
  (cond [(null? state) (list (cons key value))]
        [(eq? (caar state) key) 
         (cons (cons key value) (cdr state))]
        [else (cons (car state) (set-state-entry (cdr state) key value))]))

(define (generic-modify-state-entry addproc)
  (lambda (state key value)
    (let ((entry (get-state-entry state key)))
      (set-state-entry state key
                       (if entry 
                           (addproc entry value)
                           (list value))))))

(define (generic-modify-state-entry* addproc)
  (lambda (state key value*)
    (let ((entry (get-state-entry state key)))
      (set-state-entry state key
                       (if entry 
                           (addproc entry value*)
                           value*)))))

(define union-state-entry*)
(define union-state-entry)

(define (distinct ls)
  (cond [(null? ls) '()]
        [(memq (car ls) (cdr ls))
         (distinct (cdr ls))]
        [else (cons (car ls) (distinct (cdr ls)))]))

(define (union-assoc . a*)
  (if (null? a*) 
      '()
      (let ((keys (distinct (map-car (apply append a*)))))
        (let loop ((x keys))
          (if (null? x) 
              '()
              (cons (cons (car x)
                          (apply union (map (lambda (v)
                                              (cond [(assoc (car x) v) => cdr]
                                                    [else '()]))
                                            a*)))
                    (loop (cdr x))))))))

(define merge-states)

(define (initialize)
  (set! union-state-entry*
    (generic-modify-state-entry*
    (lambda (entry value*)
       (union value* entry))))
  (set! union-state-entry
    (generic-modify-state-entry 
     (lambda (entry value)
       (if (not (memq value entry))
           (cons value entry)
           entry))))
  (set! merge-states union-assoc))

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