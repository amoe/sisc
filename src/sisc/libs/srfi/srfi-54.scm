(for-each require-library
          '(sisc/libs/srfi/srfi-1
            sisc/libs/srfi/srfi-6
            sisc/libs/srfi/srfi-8
            sisc/libs/srfi/srfi-13
            sisc/libs/srfi/srfi-51))

(module srfi-54
  (cat)
  (import srfi-1)
  (import srfi-6)
  (import srfi-8)
  (import srfi-13)
  (import srfi-51)
  (include "../../modules/srfi/srfi-54.scm")
  (add-feature 'srfi-54))
