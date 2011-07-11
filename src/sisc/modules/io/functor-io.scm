(define-java-classes
  <sisc.io.scheme-reader>
  <sisc.io.scheme-writer>
  <sisc.io.scheme-input-stream>
  <sisc.io.scheme-output-stream>
  <sisc.data.scheme-binary-input-port>
  <sisc.data.scheme-character-input-port>
  <sisc.data.scheme-binary-output-port>
  <sisc.data.scheme-character-output-port>
  <java.io.pushback-reader>)

(define-generic-java-methods 
 (jset-port-local! |setPortLocal|)
 get-port-local)


(define (set-port-local! port value)
  (let ([oldval (java-unwrap (get-port-local port))])
    (jset-port-local! port (java-wrap value))
    oldval))
  
(define (build-custom-port name
	 wrapper-type proc-type . procs)
  (for-each (lambda (proc)
	      (unless (procedure? proc)
		(error name "expected procedure, got '~a'." proc)))
	    procs)
  (let* ([java-layer
	  (apply java-new wrapper-type
		 (map java-wrap procs))]
	 [scheme-layer (java-unwrap
			(java-new proc-type java-layer))])
    (set-port-local! java-layer scheme-layer)
    (values java-layer scheme-layer)))

(define (make-custom-character-input-port read read-string ready close)
  ; Slightly suboptimal, we build an extra SCIP
  (call-with-values
      (lambda ()
	(build-custom-port
	 'make-custom-character-input-port
	 <sisc.io.scheme-reader>
	 <sisc.data.scheme-character-input-port>
	 read read-string ready close))
    (lambda (stream port)
      (values 
       stream
       (java-unwrap
	(java-new <sisc.data.scheme-character-input-port>
		  (java-new <java.io.pushback-reader> stream)))))))
  
(define (make-custom-binary-input-port read read-block available close)
  (build-custom-port
   'make-custom-binary-input-port
   <sisc.io.scheme-input-stream>
   <sisc.data.scheme-binary-input-port>
   read read-block available close))
  
(define (make-custom-character-output-port write write-string flush close) 
  (build-custom-port
   'make-custom-character-output-port
   <sisc.io.scheme-writer>
   <sisc.data.scheme-character-output-port>
   write write-string flush close))

(define (make-custom-binary-output-port write write-block flush close)
  (build-custom-port    
   'make-custom-character-output-port
   <sisc.io.scheme-output-stream>
   <sisc.data.scheme-binary-output-port>
   write write-block flush close))
