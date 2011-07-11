(import s2j)

;;example data: a Java object that refers to a k that refers to a Java
;;object that refers to a Scheme object.

(define-java-class <java.util.vector>)
(define-generic-java-methods add element-at)

(define v (java-new <java.util.vector>))
(let ([d (java-new <java.util.vector>)])
  (add d (java-wrap '(1 2)))
  (call/cc (lambda (k) (add v (java-wrap k))))
  (java-unwrap (element-at d (->jint 0))))

;;serialization

(define-java-classes
  <java.io.file-output-stream>
  <java.io.object-output-stream>)
(define-generic-java-methods write-object flush close)

(define p (java-new <java.io.object-output-stream>
                    (java-new <java.io.file-output-stream>
                              (->jstring "sisck.ser"))))
(define-generic-java-method print-stack-trace)

(with/fc (lambda (m e) (print-stack-trace m) #f)
  (lambda () (write-object p v)))

(flush p)
(close p)

;;deserialization

(define-java-classes
  <java.io.file-input-stream>
  <java.io.object-input-stream>)
(define-generic-java-methods read-object close)

(define p (java-new <java.io.object-input-stream>
                    (java-new <java.io.file-input-stream>
                              (->jstring "sisck.ser"))))
(define vv #f)
(with/fc (lambda (m e) (print-stack-trace m) #f)
  (lambda () (set! vv (read-object p))))
(close p)
(define kk (java-unwrap (element-at vv (->jint 0))))
(kk)


;;serialisation using new framework

(import serial-io)
(define (foo x)
  (+ x (call/cc (lambda (k)
                  (call-with-serial-output-file "/tmp/rade/foo.ser"
                    (lambda (port) (serialize k port)))
                  1))))
(foo 10) ;=> 11
(exit)
;;restart
(import serial-io)
(define k (call-with-serial-input-file "/tmp/rade/foo.ser" deserialize))
(k 2) ;=> 12

;; serialisation of interned values
(define ser-file "/tmp/rade/foo.ser")
(import record)
(import oo)
(import type-system)
(import generic-procedures)
(define-nongenerative-struct foo foo-guid (field1))
(define-generics :field1 :field1!)
(define-nongenerative-class (bar) bar-guid
  (field1 :field1 :field1!))
(define aa (make bar))
(define a (make-foo aa))
(:field1! aa a) ;;introduce a circularity, just for fun
;;serial-io
(import serial-io)
(call-with-serial-output-file ser-file
  (lambda (port) (serialize a port)))
(define b (call-with-serial-input-file ser-file deserialize))
(foo? b) ;=> #t
(instance-of? (foo-field1 b) bar) ;=> #t
;;Java serialization
(import s2j)
(define-java-classes
  <java.io.file-output-stream>
  <java.io.object-output-stream>
  <java.io.file-input-stream>
  <java.io.object-input-stream>)
(define-generic-java-methods write-object read-object flush close)
(define p (java-new <java.io.object-output-stream>
                    (java-new <java.io.file-output-stream>
                              (->jstring ser-file))))
(write-object p (java-wrap a))
(flush p)
(close p)
(define p (java-new <java.io.object-input-stream>
                    (java-new <java.io.file-input-stream>
                              (->jstring ser-file))))
(define b (java-unwrap (read-object p)))
(close p)
(foo? b) ;=> #t
(instance-of? (foo-field1 b) bar) ;=> #t
;;the above also works across restarts, and in different order
;;(i.e. deser b first, then define a), and for serialisation of aa

