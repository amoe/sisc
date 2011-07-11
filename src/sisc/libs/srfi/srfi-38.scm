(module srfi-38
  (read-with-shared-structure write-with-shared-structure
                              read/ss write/ss)
  (include "../../modules/srfi/srfi-38.scm")
  (define read/ss read-with-shared-structure)
  (define write/ss write-with-shared-structure)
  (add-feature 'srfi-38))
