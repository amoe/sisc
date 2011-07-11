(define (open-output-string)
  (let ([port
         ((make-custom-character-output-port
           (lambda args
             (list #f #f
              (lambda (c)
                (set-annotation! port 'string
                                 (string-append (annotation port 'string) 
                                                (make-string c))))
              (lambda (string offset length)
                   (set-annotation! port 'string
                                    (string-append (annotation port 'string)
                                                   (substring str offset length))))
              void
              void))))])
    (set-annotation! port 'string "")
    port))

(define (get-output-string port)
  (let ([str (annotation port 'string)])
    (set-annotation! port 'string "")
    str))

        

        
        