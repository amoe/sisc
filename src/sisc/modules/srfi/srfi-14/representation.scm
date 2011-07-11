; Data representation for SRFI-14

(define-nongenerative-record-type :char-set
  sisc.srfi.srfi-14.char-set-type
  (make-char-set v)
  char-set?
  (v char-set:s char-set:s!))

(define-nongenerative-record-type :char-set-cursor
  sisc.srfi.srfi-14.char-set-cursor-type
  (make-char-set-cursor cset mask)
  char-set-cursor?
  (cset char-set-cursor-cset)
  (mask char-set-cursor-mask char-set-cursor-mask!))


