(module srfi-66
  (u8vector?
   make-u8vector
   u8vector
   u8vector->list
   list->u8vector
   u8vector-length
   u8vector-ref
   u8vector-set!
   u8vector=?
   u8vector-compare
   u8vector-copy!
   u8vector-copy)
  (import buffers)
  (include "../../modules/srfi/srfi-66.scm")
  (add-feature 'srfi-66))
