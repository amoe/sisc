;;
; convenience wrapper around thread/new&start
;;
(define (thread/spawn thunk)
  (let ([thread (thread/new thunk)])
    (thread/start thread)
    thread))

;;
; mutex/synchronize-unsafe will synchronize the execution of a thunk
; on a given mutex, but will *not* safely unlock the mutex when
; execution of the thunk raises an error or escapes the thunk by
; invoking a continuation.
;
; The only advantage over mutex/synchronize is a profound 
; difference in speed. USE WITH CARE.
;;
(define (mutex/synchronize-unsafe mutex thunk)
  (mutex/lock! mutex)
  (call-with-values thunk
    (lambda res
      (mutex/unlock! mutex)
      (apply values res))))

(define (synchronized-unsafe obj thunk)
  (mutex/synchronize-unsafe (mutex-of obj) thunk))

;;
; mutex/synchronize will synchronize the execution of a thunk on a
; given mutex and will safely unlock the mutex if the synchronized
; thunk is left due to error or a non-local exit.
;;
(define (mutex/synchronize mutex thunk)
  (dynamic-wind
   (lambda () (mutex/lock! mutex))
   thunk
   (lambda () (mutex/unlock! mutex))))

(define (synchronized obj thunk)
  (mutex/synchronize (mutex-of obj) thunk))

;;

(define (parallel . thunks)
  (let ([threads (map thread/spawn thunks)])
    (for-each (lambda (t)
                (let loop ([rv (thread/join t)])
                  (if (not rv) (loop (thread/join t)))))
              threads)
    (apply values (map thread/result threads))))

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