(module srfi-45
    ((lazy make-promise)
     eager
     (delay make-promise)
     force)
  (import misc)
  (import record)
  (include "../../modules/srfi/srfi-45.scm")
  (add-feature 'srfi-45))
