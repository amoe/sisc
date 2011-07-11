(module srfi-26
    ((cut srfi-26-internal-cut)
     (cute srfi-26-internal-cute))
  (include "../../modules/srfi/srfi-26.scm")
  (add-feature 'srfi-26))
