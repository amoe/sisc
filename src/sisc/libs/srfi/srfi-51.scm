(for-each require-library
          '(sisc/libs/srfi/srfi-1
            sisc/libs/srfi/srfi-23))


(module srfi-51
  (rest-values
   (arg-and caller-arg-and)
   (arg-or caller-arg-or)
   (arg-ands caller-arg-and arg-and)
   (arg-ors caller-arg-or arg-or)
   err-and
   err-or
   err-ands
   err-ors)
  (import srfi-1)
  (import srfi-23)
  (include "../../modules/srfi/srfi-51.scm")
  (add-feature 'srfi-51))
