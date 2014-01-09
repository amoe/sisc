(require-library 'sisc/libs/srfi/srfi-23)
(require-library 'sisc/libs/srfi/srfi-42)

(define-interface srfi-78-exports
  (check
   check-ec
   check-report
   check-set-mode!
   check-reset!
   check-passed?))

(define-interface srfi-78-implicit-exports
  (check:mode
   check:proc
   check-ec:make
   check:proc-ec))

(define-compound-interface srfi-78-interface
  (srfi-78-exports
   srfi-78-implicit-exports
   ;;the following really shouldn't be exported, but because
   ;;comprehensions assume generators are macros in their own right we
   ;;need to do this.
   srfi-42-implicit-exports))

(define-module srfi-78 srfi-78-interface
  (import srfi-23)
  (import srfi-42)
  (include "../../modules/srfi/srfi-78.scm")
  (add-feature 'srfi-78))
