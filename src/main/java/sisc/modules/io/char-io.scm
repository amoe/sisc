(define open-character-output-port
  (let ([_open-character-output-port open-character-output-port])
    (lambda (binout . charset+autoflush)
      (cond [(null? charset+autoflush)
             (_open-character-output-port binout)]
            [(string? (car charset+autoflush))
             (let ([port (_open-character-output-port binout (car charset+autoflush))])
               (if (and (not (null? (cdr charset+autoflush)))
                        (cadr charset+autoflush))
                   (open-autoflush-character-output-port port)
                   port))]
            [else
             (let ([port (_open-character-output-port binout)])
               (if (and (not (null? charset+autoflush))
                        (car charset+autoflush))
                   (open-autoflush-character-output-port port)
                   port))]))))



               
               
      