(require-library 'sisc/libs/srfi/srfi-23)

(module srfi-2 (and-let*)
  (import srfi-23) ;ERROR
  (include "../../modules/srfi/srfi-2.scm")
  (add-feature 'srfi-2))
