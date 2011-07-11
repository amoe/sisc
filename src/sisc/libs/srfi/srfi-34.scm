(module srfi-34
    (raise
     with-exception-handler
     (guard guard-aux))
  (include "../../modules/srfi/srfi-34.scm")
  (add-feature 'srfi-34))
