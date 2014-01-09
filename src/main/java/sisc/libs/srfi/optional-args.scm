(require-library 'sisc/libs/srfi/srfi-23)

(module optional-args
  (check-arg
   :optional
   (let-optionals* %let-optionals*)
   error
   bitwise-and)
  (import _srfi-23)
  (define (error msg . args)
    (cond
     ((null? args)
      (_error msg))
     ((= (length args) 1)
      (_error (string-append msg ": ~a") (car args)))
     (else
      (_error (string-append msg " in ~a: ~a") (car args) (cdr args)))))
  (define (bitwise-and . args)
    (error "bitwise-and not implemented"))
  (include "../../modules/srfi/optional-args.scm"))
