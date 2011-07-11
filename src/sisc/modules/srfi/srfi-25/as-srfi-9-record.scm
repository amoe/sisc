;;; array as-srfi-9-record
;;; 2001 Jussi Piitulainen

;;; Untested.

(define-nongenerative-record-type array:srfi-9-record-type-descriptor
  sisc.srfi.srfi-25.array-type
  (array:make vec ind shp)
  array:array?
  (vec array:vector)
  (ind array:index)
  (shp array:shape))
