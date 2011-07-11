(import type-system)

; Sure would be nice to have SRFI-1 in the main env.

(define (add-to-type-map proc-name old type proc)
  (if (not (procedure? proc))
      (error proc-name "expected procedure, got '~s'." proc))
  (let loop ([ls old])
    (cond [(null? ls) (list (cons type proc))]
          [(null? (cdr ls))
           (cons (car ls) (loop (cdr ls)))]
          [(equal? (caar ls) type)
           (cons (cons type proc)
                 (cdr ls))]
          [else (loop (cdr ls))])))

(define (remove-from-type-map map type)
  (cond [(null? map) '()]
        [(equal? (caar map) type) (cdr map)]
        [else (cons (car map)
                    (remove-from-type-map (cdr map) type))]))
         
(define (add-custom-display-for-type! type proc)
  (custom-display-type-map
   (add-to-type-map 'add-custom-display-for-type!
                    (custom-display-type-map) type proc)))

(define (add-custom-write-for-type! type proc)
  (custom-write-type-map
   (add-to-type-map 'add-custom-write-for-type!
                    (custom-write-type-map) type proc)))

(define (remove-custom-display-for-type! type)
  (custom-display-type-map
   (remove-from-type-map (custom-display-type-map) type)))

(define (remove-custom-write-for-type! type)
  (custom-write-type-map
   (remove-from-type-map (custom-write-type-map) type)))
  
(define (with-custom-write-for-type type proc thunk)
  (parameterize ((custom-write-type-map
                  (new-custom-printer-map 'with-custom-write-for-type
                                          (custom-write-type-map) type proc)))
    (thunk)))

(define (with-custom-display-for-type type proc thunk)
  (parameterize ((custom-display-type-map
                  (new-custom-printer-map 'with-custom-display-for-type
                                          (custom-display-type-map) type proc)))
    (thunk)))
      