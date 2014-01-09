(require-library 'sisc/libs/srfi/srfi-16)
(require-library 'sisc/libs/srfi/srfi-23)
(require-library 'sisc/libs/srfi/srfi-27)

(module srfi-67
    (boolean-compare
     char-compare char-compare-ci
     string-compare string-compare-ci
     symbol-compare
     integer-compare rational-compare real-compare complex-compare
     number-compare
     vector-compare vector-compare-as-list
     list-compare list-compare-as-vector
     pair-compare-car pair-compare-cdr pair-compare
     default-compare
     refine-compare select-compare cond-compare
     if3
     (if=? compare:if-rel?)
     (if<? compare:if-rel?)
     (if>? compare:if-rel?)
     (if<=? compare:if-rel?)
     (if>=? compare:if-rel?)
     (if-not=? compare:if-rel?)
     =? <? >? <=? >=? not=?
     </<? </<=? <=/<? <=/<=? >/>? >/>=? >=/>? >=/>=?
     chain=? chain<? chain>? chain<=? chain>=?
     pairwise-not=?
     min-compare max-compare
     kth-largest
     compare-by< compare-by> compare-by<= compare-by>=
     compare-by=/< compare-by=/>
     debug-compare)
  (import srfi-16)
  (import srfi-23)
  (import* srfi-27 random-integer)
  (include "../../modules/srfi/srfi-67.scm")
  (add-feature 'srfi-67))
