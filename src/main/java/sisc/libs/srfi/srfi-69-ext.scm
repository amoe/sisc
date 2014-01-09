(module srfi-69-ext
    (hash-by-eqv
     hash-table-thread-safe? hash-table-weak?
     hash-table-ref! hash-table-ref!/default)
  (import hashtable)
  (include "../../modules/srfi/srfi-69-ext.scm")
  (add-feature 'srfi-69-ext))
