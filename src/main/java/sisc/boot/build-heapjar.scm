#! /usr/bin/env scheme-r5rs

; This is an SRFI-22 script which will build a SISC Heap jar
; by locating the heap from the current directory, the Heap Anchor, 
; and emitting sisc-heap.jar, or another file if specified as a 
; command-line argument


(import s2j)
(import oo)
(import binary-io)
(import generic-procedures)

(define-java-classes
  <sisc.boot.heap-anchor>
  <sisc.interpreter.app-context>
  <sisc.data.scheme-binary-output-port>
  <java.util.zip.zip-output-stream>
  <java.io.file-output-stream>
  <java.util.zip.zip-entry>
  (<java.net.URL> |java.net.URL|))

(define-generic-java-methods put-next-entry close-entry write close find-heap
  get-resource get-output-stream)

(define (copy in out)
  (let ([buffer (make-buffer 8192)])
    (let loop ([rc 0])
      (unless (eof-object? rc)
        (write-block buffer 0 rc out)
        (loop (read-block buffer 0 (buffer-length buffer) in))))
    (flush-output-port out)))

(define (find-heap-file)
  (open-binary-input-file (->string (find-heap (java-null <sisc.interpreter.app-context>)
                                               (java-null <java.net.URL>)))))

(define (find-heap-anchor-file)
  (open-binary-input-file
   (->string (get-resource <sisc.boot.heap-anchor> (->jstring "HeapAnchor.class")))))

(define (build-heap-jar . output)
  (let ([outfile (open-binary-output-file
                  (if (null? output)
                      "sisc-heap.jar"
                      (car output)))]
        [heapfile (find-heap-file)]
        [heapanchor (find-heap-anchor-file)])
    (let* ([zip-out (java-new <java.util.zip.zip-output-stream>
                              (get-output-stream (java-wrap outfile)))]
           [zip-schemeout (java-unwrap (java-new <sisc.data.scheme-binary-output-port> zip-out))]
           [anchorentry (java-new <java.util.zip.zip-entry>
                                  (->jstring "sisc/boot/HeapAnchor.class"))]
           [heapentry (java-new  <java.util.zip.zip-entry>
                                 (->jstring "sisc/boot/sisc.shp"))])
      (put-next-entry zip-out anchorentry)
      (copy heapanchor zip-schemeout)
      (close-entry zip-out)
      (put-next-entry zip-out heapentry)
      (copy heapfile zip-schemeout)
      (close-entry zip-out)
      (close-output-port zip-schemeout))))

(define (main args)
  (apply build-heap-jar (cdr args)))

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