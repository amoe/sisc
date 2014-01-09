(define-java-classes
  <java.lang.runtime>
  <java.lang.process>
  <java.io.file>
  <java.lang.string*>
  <java.lang.string>)
  
(define-generic-java-methods
  (java-get-runtime |getRuntime|)
  exec get-error-stream get-input-stream 
  get-output-stream wait-for destroy 
  exit-value)

(define runtime-instance #f)

(define (get-runtime)
  (or runtime-instance
      (begin
        (set! runtime-instance 
              (java-get-runtime (java-null <java.lang.runtime>)))
        (get-runtime))))

(define (process-terminated? process)
  (with-failure-continuation
   (lambda (m e) #f)
   (lambda () (->number (exit-value process)))))

(define (wait-for-process process)
  (with/fc (lambda (m e) #f)
    (lambda () (->number (wait-for process)))))
  
(define (process? val)
  (and (java-object? val) 
       (instance-of? val <java.lang.process>)))

(define (get-process-stdout process)
  (->binary-input-port (get-input-stream process)))

(define (get-process-stderr process)
  (->binary-input-port (get-error-stream process)))

(define (get-process-stdin process . aflush)
  (let ([base-port (->binary-output-port (get-output-stream process))])
    (if aflush
        (open-buffered-binary-output-port base-port)
        base-port)))
        
(define (spawn-process progname . arglist)
  (let ([runtime (get-runtime)])
    (if (null? arglist)
        (exec runtime (->jstring progname))
        (exec runtime 
         (->jarray (map ->jstring (cons progname (car arglist)))
    	       <java.lang.string>)))))

(define (assoc->envp assoc)
  (->jarray (map (lambda (binding)
    	   (->jstring 
                       (format "~a=~a" (car binding) (cdr binding))))
            assoc)
       <java.lang.string>))

(define (spawn-process-with-environment progname arglist env . working-dir)
  (let ([runtime (get-runtime)])
    (exec runtime 
      (->jarray (map ->jstring (cons progname arglist))
    	   <java.lang.string>)
      (if env (assoc->envp env) (java-null <java.lang.string*>))
      (java-new <java.io.file> 
    	   (->jstring 
    	    (if (null? working-dir)
    		(current-directory)
    		(normalize-url (car working-dir))))))))

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
