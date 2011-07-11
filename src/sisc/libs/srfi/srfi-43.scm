(require-library 'sisc/libs/srfi/r5rs)
(require-library 'sisc/libs/srfi/optional-args)
(require-library 'sisc/libs/srfi/srfi-8)

(module srfi-43
    (make-vector
     vector
     vector-unfold vector-unfold-right
     vector-copy vector-reverse-copy
     vector-append vector-concatenate
     vector?
     vector-empty?
     vector=
     vector-ref
     vector-length
     vector-fold vector-fold-right
     vector-map vector-map!
     vector-for-each
     vector-count
     vector-index vector-skip
     vector-index-right vector-skip-right
     vector-binary-search
     vector-any vector-every
     vector-set!
     vector-swap!
     vector-fill!
     vector-reverse!
     vector-copy! vector-reverse-copy!
     vector-reverse!
     vector->list reverse-vector->list
     list->vector reverse-list->vector)
  (import* r5rs
           make-vector
           vector
           vector?
           vector-ref
           vector-length
           vector-set!
           (_vector-fill! vector-fill!)
           (_vector->list vector->list)
           (_list->vector list->vector))
  (import optional-args)
  (import srfi-8) ;RECEIVE
  (include "../../modules/srfi/srfi-43.scm")
  (add-feature 'srfi-43))


