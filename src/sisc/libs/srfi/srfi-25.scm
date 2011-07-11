(module srfi-25-int
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
     array:apply-to-vector
     array:array?
     array:share/index!
     array:applier-to-vector)
  (import record)
  (include "../../modules/srfi/srfi-25/as-srfi-9-record.scm")
  ;;pick one of three alternative implementations
  (include "../../modules/srfi/srfi-25/ix-ctor.scm")
  (include "../../modules/srfi/srfi-25/op-ctor.scm")
  ;;(include "../../modules/srfi/srfi-25/ix-mbda.scm")
  ;;(include "../../modules/srfi/srfi-25/op-mbda.scm")
  ;;(include "../../modules/srfi/srfi-25/ix-tter.scm")
  ;;(include "../../modules/srfi/srfi-25/op-tter.scm")
  (include "../../modules/srfi/srfi-25/array.scm"))
    
(module srfi-25
    (array?
     make-array
     shape
     array
     array-rank
     array-start
     array-end
     array-ref
     array-set!
     share-array)
  (import srfi-25-int)
  (add-feature 'srfi-25))
