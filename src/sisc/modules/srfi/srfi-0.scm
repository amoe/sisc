;; SRFI-0

(define-syntax cond-expand
  (lambda (x)
    (syntax-case x (and or not else)
      ((cond-expand)
       (syntax (syntax-error "Unfulfilled cond-expand")))
      ((cond-expand (else body ...))
       (syntax (begin body ...)))
      ((cond-expand ((and) body ...) more-clauses ...)
       (syntax (begin body ...)))
      ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
       (syntax
        (cond-expand
         (req1
          (cond-expand
           ((and req2 ...) body ...)
           more-clauses ...))
         more-clauses ...)))
      ((cond-expand ((or) body ...) more-clauses ...)
       (syntax (cond-expand more-clauses ...)))
      ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
       (syntax
        (cond-expand
         (req1
          (begin body ...))
         (else
          (cond-expand
           ((or req2 ...) body ...)
           more-clauses ...)))))
      ((cond-expand ((not req) body ...) more-clauses ...)
       (syntax
        (cond-expand
         (req
          (cond-expand more-clauses ...))
         (else body ...))))
      ((cond-expand (feature-id body ...) more-clauses ...)
       (if (has-feature? (syntax-object->datum (syntax feature-id)))
           (syntax (begin body ...))
           (syntax (cond-expand more-clauses ...)))))))
