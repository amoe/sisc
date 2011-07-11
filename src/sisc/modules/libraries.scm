(define *libraries* (make-hashtable eq?))

(define (locate-library lib)
  (define (suffixed-lib ext)
    (find-resource (string-append (symbol->string lib) "." ext)))
  (or (suffixed-lib "scc")
      (suffixed-lib "sce")
      (suffixed-lib "scm")))

(define (normalize-lib-name lib)
  (if (symbol? lib) lib (string->symbol lib)))

(define (provide-library lib)
  (hashtable/put! *libraries* lib #t))

(define *tracker* #f)

(define (track item thunk)
  (let ([tracker #f])
    (dynamic-wind
     (lambda ()
       (set! tracker *tracker*)
       (set! *tracker* '()))
     (lambda ()
       (thunk)
       (let ([res (cons item *tracker*)])
         (if tracker (set! tracker (append tracker (list res))))
         res))
     (lambda ()
       (set! *tracker* tracker)))))

(define (require-library lib)
  (synchronized
   *libraries*
   (lambda()
     (let ([lib (normalize-lib-name lib)])
       (or (library-loaded? lib)
           (let ([url (locate-library lib)])
             (if (not url)
                 (error "library ~a not found" lib))
             (track lib (lambda ()
                          (load url)
                          (provide-library lib)))))))))

(define (library-exists? lib)
  (let ([lib (normalize-lib-name lib)])
    (or (library-loaded? lib)
        (and (locate-library lib) #t))))

(define (library-loaded? lib)
  (let ([lib (normalize-lib-name lib)])
    (hashtable/get *libraries* lib)))

(define (load-compiled name)
  (call-with-serial-input-file name
    (lambda (port)
      (let loop ()
        (let ([v (deserialize port)])
          (or (eof-object? v) (begin (v) (loop))))))))

(define (compile-file from to)
  (let ([inf #f]
        [outf #f])
    (dynamic-wind
     (lambda ()
       (set! inf (open-source-input-file from))
       (set! outf (open-serial-output-port (open-binary-output-file to))))
     (lambda ()
       (with-current-url from
         (lambda ()
           (let loop ()
             (let ([e (read-code inf)])
               (or (eof-object? e)
                   (let ([p (compile e)])
                     (p)
                     (serialize p outf)
                     (loop))))))))
     (lambda ()
       (close-output-port outf)
       (close-input-port inf)))))

(define (install)
  (add-file-handler 'scc load-compiled))

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