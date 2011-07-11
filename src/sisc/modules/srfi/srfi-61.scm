; Reference implementation of SRFI-61, A more general COND clause

(define-syntax cond
  (syntax-rules (=> ELSE)

    ((COND (ELSE else1 else2 ...))
     ;; The (IF #T (BEGIN ...)) wrapper ensures that there may be no
     ;; internal definitions in the body of the clause.  R5RS mandates
     ;; this in text (by referring to each subform of the clauses as
     ;; <expression>) but not in its reference implementation of COND,
     ;; which just expands to (BEGIN ...) with no (IF #T ...) wrapper.
     (IF #T (BEGIN else1 else2 ...)))

    ((COND (test => receiver) more-clause ...)
     (LET ((T test))
       (COND/MAYBE-MORE T
                        (receiver T)
                        more-clause ...)))

    ((COND (generator guard => receiver) more-clause ...)
     (CALL-WITH-VALUES (LAMBDA () generator)
       (LAMBDA T
         (COND/MAYBE-MORE (APPLY guard    T)
                          (APPLY receiver T)
                          more-clause ...))))

    ((COND (test) more-clause ...)
     (LET ((T test))
       (COND/MAYBE-MORE T T more-clause ...)))

    ((COND (test body1 body2 ...) more-clause ...)
     (COND/MAYBE-MORE test
                      (BEGIN body1 body2 ...)
                      more-clause ...))))

(define-syntax cond/maybe-more
  (syntax-rules ()
    ((COND/MAYBE-MORE test consequent)
     (IF test
         consequent))
    ((COND/MAYBE-MORE test consequent clause ...)
     (IF test
         consequent
         (COND clause ...)))))

