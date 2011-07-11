; NB: In deviation from the spec, the dynamic environment in which
; handler is invoked is that of the call to
; |with-exception-handler|. By contrast, the spec states that "The
; handler is called in the dynamic environment of the call to |raise|,
; except that the current exception handler is that in place for the
; call to with-exception-handler that installed the handler being
; called".
(define (with-exception-handler handler thunk)
  (with/fc (lambda (m e) (handler (error-message m))) thunk))

(define raise error)

; NB: In deviation from the spec, in the fall-through case |raise| is
; re-invoked in the dynamic environment of the call to |guard|. By
; contrast the spec states that it should be "re-invoked on the raised
; object within the dynamic environment of the original call to raise
; except that the current exception handler is that of the guard
; expression."
(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     (with/fc (lambda (m e)
                (let ([var (error-message m)])
                  (guard-aux (raise var) clause ...)))
       (lambda () e1 e2 ...)))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     (let ((temp test))
       (if temp 
           (result temp)
           reraise)))
    ((guard-aux reraise (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     test)
    ((guard-aux reraise (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
         (begin result1 result2 ...)
         reraise))
    ((guard-aux reraise (test result1 result2 ...) clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (guard-aux reraise clause1 clause2 ...)))))
