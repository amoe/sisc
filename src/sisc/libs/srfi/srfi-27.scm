(module srfi-27
    (default-random-source
      make-random-source
      random-integer 
      random-real
      random-source?
      random-source-make-integers
      random-source-make-reals
      random-source-pseudo-randomize!
      random-source-randomize!
      random-source-state-ref
      random-source-state-set!
      srfi-27-init)
  (import type-system)
  (import s2j)
  (import logicops)
  (import hashtable)
  (include "../../modules/srfi/srfi-27.scm")
  (srfi-27-init)
  (add-feature 'srfi-27))
