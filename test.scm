
(with-output-to-string  
  (lambda ()
    (for-each (lambda (n)
                (display n))
              ls))))
