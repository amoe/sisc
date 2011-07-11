;; SRFI-38: write-showing-shared, or Common-Lisp style shared structure
;; external representations.
;;
;; These are handled natively by SISC, we just need to create the 
;; write-showing-shared function.

(define write-with-shared-structure 
  (lambda args
    (let ([old-shared-state (print-shared #t)])
      (apply write args)
      (print-shared old-shared-state)
      #!void)))

(define read-with-shared-structure read)
