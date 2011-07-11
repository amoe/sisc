(require-library 'sisc/libs/srfi/srfi-23)

(module srfi-16 (case-lambda)
  (import srfi-23) ;ERROR
  (include "../../modules/srfi/srfi-16.scm")
  (add-feature 'srfi-16))
