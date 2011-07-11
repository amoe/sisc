; Reference implementation for SRFI 54: Formatting

(define (cat object . rest)
  (receive (str-list rest-list) (partition string? rest)
    (if (null? rest-list)
    (apply string-append
           (cond
    	((number? object) (number->string object))
    	((string? object) object)
    	((char? object) (string object))
    	((boolean? object) (if object "#t" "#f"))
    	(else
    	 (get-output-string
    	  (let ((str-port (open-output-string)))
    	    (write object str-port)
    	    str-port))))
           str-list)
    (receive (width port char converter precision sign radix exactness
                        separator writer pipe take)
        (rest-values "cat: bad argument" rest-list #t
    		 (cons 0 (lambda (x) (and (integer? x) (exact? x))))
    		 (cons #f (lambda (x) (or (boolean? x)
    					  (output-port? x))))
    		 (cons #\space char?)
    		 (cons #f (lambda (x)
    			    (and (pair? x)
    				 (procedure? (car x))
    				 (procedure? (cdr x)))))
    		 (cons #f (lambda (x) (and (integer? x) (inexact? x))))
    		 (cons #f (lambda (x) (eq? x 'sign)))
    		 '(decimal octal binary hexadecimal)
    		 (cons #f (lambda (x) (memq x '(exact inexact))))
    		 (cons #f (lambda (x)
    			    (and (list? x)
    				 (< 0 (length x) 3)
    				 (char? (car x))
    				 (or (null? (cdr x))
    				     (let ((n (cadr x)))
    				       (and (integer? n)
    					    (exact? n)
    					    (< 0 n)))))))
    		 (cons #f procedure?)
    		 (cons #f (lambda (x)
    			    (and (list? x)
    				 (not (null? x))
    				 (every procedure? x))))
    		 (cons #f (lambda (x)
    			    (and (list? x)
    				 (< 0 (length x) 3)
    				 (every (lambda (x)
    					  (and (integer? x)
    					       (exact? x)))
    					x)))))
      (let ((port (if (eq? port #t) (current-output-port) port))
    	(str
    	 (apply
    	  string-append
    	  (cond
    	   ((and converter
    		 ((car converter) object))
    	    (let* ((str ((cdr converter) object))
    		   (pad (- (abs width) (string-length str))))
    	      (cond
    	       ((<= pad 0) str)
    	       ((< 0 width) (string-append (make-string pad char) str))
    	       (else (string-append str (make-string pad char))))))
    	   ((number? object)
    	    (arg-ors 
    	     ("cat: non-decimal cannot have a decimal point" radix
    	      (and (not (eq? radix 'decimal)) precision))
    	     ("cat: exact number cannot have a decimal point without exact sign" precision
    	      (and precision (< precision 0) (eq? exactness 'exact))))
    	    (let ((exact-sign
    		   (and precision
    			(<= 0 precision)
    			(or (eq? exactness 'exact)
    			    (and (exact? object)
    				 (not (eq? exactness 'inexact))))
    			"#e"))
    		  (inexact-sign
    		   (and (not (eq? radix 'decimal))
    			(or (and (inexact? object)
    				 (not (eq? exactness 'exact)))
    			    (eq? exactness 'inexact))
    			"#i"))
    		  (radix-sign
    		   (cdr (assq radix '((decimal . #f) (octal . "#o")
    				      (binary . "#b")
    				      (hexadecimal . "#x")))))
    		  (plus-sign (and sign (< 0 (real-part object)) "+")))
    	      (let* ((exactness-sign (or exact-sign inexact-sign))
    		     (str
    		      (if precision
    			  (let ((precision
    				 (inexact->exact (abs precision)))
    				(imag (imag-part object)))
    			    (define (e-mold num pre)
    			      (let* ((str (number->string
    					   (if (exact? num)
    					       (exact->inexact num)
    					       num)))
    				     (e-index (string-index str #\e)))
    				(if e-index
    				    (string-append
    				     (mold (substring str 0 e-index)
    					   pre)
    				     (substring str e-index
    						(string-length str)))
    				    (mold str pre))))
    			    (define (mold str pre)
    			      (let ((ind (string-index str #\.)))
    				(if ind
    				    (let ((d-len (- (string-length str)
    						    (+ ind 1))))
    				      (cond
    				       ((= d-len pre) str)
    				       ((< d-len pre)
    					(string-append str
    						       (make-string
    							(- pre d-len)
    							#\0)))
                                               ((or (char<? #\5 (string-ref str (+ 1 ind pre)))
                                                    (and (char=? #\5 (string-ref str (+ 1 ind pre)))
                                                         (or (< (+ 1 pre) d-len)
                                                             (memv (string-ref str (+ ind (if (= 0 pre) -1 pre)))
                                                                   '(#\1 #\3 #\5 #\7 #\9)))))
                                                (apply string
                                                       (let* ((minus (char=? #\- (string-ref str 0)))
                                                              (str (substring str (if minus 1 0) (+ 1 ind pre)))
                                                              (char-list
                                                               (reverse
                                                                (let lp ((index (- (string-length str) 1))
                                                                         (raise #t))
                                                                  (if (= -1 index)
                                                                      (if raise '(#\1) '())
                                                                      (let ((chr (string-ref str index)))
                                                                        (if (char=? #\. chr)
                                                                            (cons chr (lp (- index 1) raise))
                                                                            (if raise
                                                                                (if (char=? #\9 chr)
                                                                                    (cons #\0 (lp (- index 1) raise))
                                                                                    (cons (integer->char
                                                                                           (+ 1 (char->integer chr)))
                                                                                          (lp (- index 1) #f)))
                                                                                (cons chr (lp (- index 1) raise))))))))))
                                                         (if minus (cons #\- char-list) char-list))))
    				       (else
    					(substring str
    						   0 (+ 1 ind pre)))))
    				    (string-append
    				     str "." (make-string pre #\0)))))
    			    (if (= 0 imag)
    				(e-mold object precision)
    				(string-append
    				 (e-mold (real-part object) precision)
    				 (if (< 0 imag) "+" "")
    				 (e-mold imag precision)
    				 "i")))
    			  (number->string
    			   (cond
    			    (inexact-sign
    			     (if (inexact? object)
    				 (inexact->exact object) object))
    			    (exactness
    			     (if (eq? exactness 'exact)
    				 (if (inexact? object)
    				     (inexact->exact object) object)
    				 (if (exact? object)
    				     (exact->inexact object) object)))
    			    (else object))
    			   (cdr (assq radix '((decimal . 10)
    					      (octal . 8)
    					      (binary . 2)
    					      (hexadecimal . 16)))))))
    		     (str
    		      (if (and separator
    			       (not (or (and (eq? radix 'decimal)
    					     (string-index str #\e))
    					(string-index str #\i)
    					(string-index str #\/))))
 				  (let ((sep (string (car separator)))
 					(num (if (null? (cdr separator))
 						 3 (cadr separator)))
 					(dot-index (string-index str #\.)))
 				    (define (separate str sep num opt)
 				      (let* ((len (string-length str))
 					     (pos
 					      (if opt
 						  (let ((pos
 							 (remainder
 							  (if (eq? opt 'minus)
 							      (- len 1) len)
 							  num)))
 						    (if (= 0 pos) num pos))
 						  num)))
    				(apply
    				 string-append
    				 (let loop ((ini 0)
    					    (pos (if (eq? opt 'minus)
    						     (+ pos 1) pos)))
    				   (if (< pos len)
    				       (cons (substring str ini pos)
    					     (cons sep
    						   (loop pos
    							 (+ pos num))))
    				       (list (substring str
    							ini len)))))))
    			    (if dot-index
    				(string-append
    				 (separate (substring str 0 dot-index)
    					   sep num (if (< object 0)
    						       'minus #t))
    				 "."
    				 (separate (substring
    					    str (+ 1 dot-index)
    					    (string-length str))
    					   sep num #f))
    				(separate str sep num (if (< object 0)
    							  'minus #t))))
    			  str))
    		     (pad (- (abs width)
    			     (+ (string-length str)
    				(if exactness-sign 2 0)
    				(if radix-sign 2 0)
    				(if plus-sign 1 0))))
    		     (pad (if (< 0 pad) pad 0)))
    		(if (< 0 width)
    		    (if (char-numeric? char)
    			(if (< (real-part object) 0)
    			    (string-append (or exactness-sign "")
    					   (or radix-sign "")
    					   "-"
    					   (make-string pad char)
    					   (substring str 1
    						      (string-length
    						       str)))
    			    (string-append (or exactness-sign "")
    					   (or radix-sign "")
    					   (or plus-sign "")
    					   (make-string pad char)
    					   str))
    			(string-append (make-string pad char)
    				       (or exactness-sign "")
    				       (or radix-sign "")
    				       (or plus-sign "")
    				       str))
    		    (string-append (or exactness-sign "")
    				   (or radix-sign "")
    				   (or plus-sign "")
    				   str
    				   (make-string pad char))))))
    	   (else
    	    (let* ((str (cond
    			 (writer (get-output-string
    				  (let ((str-port
    					 (open-output-string)))
    				    (writer object str-port)
    				    str-port)))
    			 ((string? object) object)
    			 ((char? object) (string object))
    			 ((boolean? object) (if object "#t" "#f"))
    			 (else (get-output-string
    				(let ((str-port (open-output-string)))
    				  (write object str-port)
    				  str-port)))))
    		   (str (if pipe
    			    (let loop ((str ((car pipe) str))
    				       (fns (cdr pipe)))
    			      (if (null? fns)
    				  str
    				  (loop ((car fns) str)
    					(cdr fns))))
    			    str))
    		   (str
    		    (if take
    			(let ((left (car take))
    			      (right (if (null? (cdr take))
    					 0 (cadr take)))
    			      (len (string-length str)))
    			  (define (substr str beg end)
    			    (let ((end (cond
    					((< end 0) 0)
    					((< len end) len)
    					(else end)))
    				  (beg (cond
    					((< beg 0) 0)
    					((< len beg) len)
    					(else beg))))
    			      (if (and (= beg 0) (= end len))
    				  str
    				  (substring str beg end))))
    			  (string-append
    			   (if (< left 0)
    			       (substr str (abs left) len)
    			       (substr str 0 left))
    			   (if (< right 0)
    			       (substr str 0 (+ len right))
    			       (substr str (- len right) len))))
    			str))
    		   (pad (- (abs width) (string-length str))))
    	      (cond
    	       ((<= pad 0) str)
    	       ((< 0 width) (string-append (make-string pad char) str))
    	       (else (string-append str (make-string pad char)))))))
    	  str-list)))
        (and port (display str port))
        str)))))
