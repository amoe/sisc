(require-library 'sisc/libs/srfi/srfi-11)

(module srfi-37
    (option
     option-names
     option-required-arg?
     option-optional-arg?
     option-processor
     args-fold)
  (import record)
  (import srfi-11)
  (include "../../modules/srfi/srfi-37.scm")
  (add-feature 'srfi-37))
