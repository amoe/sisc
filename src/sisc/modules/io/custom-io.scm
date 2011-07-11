(define-java-classes
  <sisc.io.custom.scheme-reader>
  <sisc.io.custom.scheme-writer>
  <sisc.io.custom.scheme-input-stream>
  <sisc.io.custom.scheme-output-stream>
  <sisc.io.custom.custom-binary-input-port>
  <sisc.io.custom.custom-character-input-port>
  <sisc.io.custom.custom-binary-output-port>
  <sisc.io.custom.custom-character-output-port>
  <java.io.pushback-reader>)

(define (build-custom-port name wrapper-type java-stream-chain
                           proc-type . procs)
  (for-each (lambda (proc)
          (unless (procedure? proc)
    	(error name "expected procedure, got '~a'." proc)))
        procs)
  (let* ([java-proxy-stream
          (apply java-new wrapper-type (map java-wrap procs))]
         [java-layer
          (let loop ([java-stream java-proxy-stream]
                     [jst java-stream-chain])
            (if (null? jst)
                java-stream
                (loop (java-new (car jst) java-stream)
                      (cdr jst))))])
    (java-unwrap
     (java-new proc-type java-layer java-proxy-stream))))

(define (make-custom-character-input-port read read-string ready close)
  (build-custom-port
   'make-custom-character-input-port
   <sisc.io.custom.scheme-reader>
   (list <java.io.pushback-reader>)
   <sisc.io.custom.custom-character-input-port>
   read read-string ready close))
  
(define (make-custom-binary-input-port read read-block available close)
  (build-custom-port
   'make-custom-binary-input-port
   <sisc.io.custom.scheme-input-stream>
   '()
   <sisc.io.custom.custom-binary-input-port>
   read read-block available close))
  
(define (make-custom-character-output-port write write-string flush close) 
  (build-custom-port
   'make-custom-character-output-port
   <sisc.io.custom.scheme-writer>
   '()
   <sisc.io.custom.custom-character-output-port>
   write write-string flush close))

(define (make-custom-binary-output-port write write-block flush close)
  (build-custom-port    
   'make-custom-character-output-port
   <sisc.io.custom.scheme-output-stream>
   '()
   <sisc.io.custom.custom-binary-output-port>
   write write-block flush close))

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
