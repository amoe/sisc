(import custom-io)
(require-extension (srfi 11))

;; String I/O
; Use the local value to store a pair of string position and the string

(define (sio/read local)
  (let ([str (car local)]
	[ptr (cdr local)])
    (if (= (string-length str) ptr)
	-1
	(let ([c (char->integer
		  (string-ref str ptr))])
	  (set-cdr! local (+ ptr 1))
	  c))))

(define (sio/read-string local buffer offset length)
  (let ([str (car local)])
    (do ([i offset (+ i 1)]
	 [j (cdr local) (+ j 1)]
	 [c 0 (+ c 1)])
	((or (= i (+ offset length))
	     (= j (string-length str)))
	 (set-cdr! local j)
	 c)
      (string-set! buffer i (string-ref str j)))))

(define null (lambda args (void)))

(define (open-input-string str)
  (unless (string? str)
    (error 'open-input-string "expected string, got '~a'.~%" str))
  (let-values ([(stream port)
		(make-custom-character-input-port
		 sio/read sio/read-string null null)])
    (set-port-local! stream (cons str 0))
    port))
     

   
    
  

