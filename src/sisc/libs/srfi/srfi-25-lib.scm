(require-library 'sisc/libs/srfi/srfi-25)

(module srfi-25-lib
    (array?
     make-array
     shape
     array
     array-rank
     array-start
     array-end
     array-ref
     array-set!
     share-array
     ;;the above are all re-exported from srfi-25
     array-shape
     array-length
     array-size
     array-equal?
     shape-for-each
     array-for-each-index
     tabulate-array
     tabulate-array!
     array-retabulate!
     array-map
     array-map!
     array->vector
     array->list
     share-array/prefix
     share-array/origin
     share-array/index!
     array-append
     transpose
     share-nths)
  (import srfi-25-int)
  (include "../../modules/srfi/srfi-25/arlib.scm"))
