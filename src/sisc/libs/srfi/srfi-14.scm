(require-library 'sisc/libs/srfi/srfi-1)
(require-library 'sisc/libs/srfi/srfi-60)
(require-library 'sisc/libs/srfi/optional-args)

(module srfi-14-data
  (make-char-set char-set? char-set:s char-set:s!
   make-char-set-cursor char-set-cursor?
   char-set-cursor-cset char-set-cursor-mask
   char-set-cursor-mask!)
  (import record)
  (include "../../modules/srfi/srfi-14/representation.scm")  )

(module srfi-14-charsets
  (char-set:lower-case		char-set:upper-case	char-set:title-case
   char-set:letter		char-set:digit		char-set:letter+digit
   char-set:graphic		char-set:printing	char-set:whitespace
   char-set:iso-control		char-set:punctuation	char-set:symbol
   char-set:hex-digit		char-set:blank		char-set:ascii
   char-set:empty	                  char-set:full)
  (import srfi-14-data)
  (include "../../modules/srfi/srfi-14/charsets.scm"))
   
(module srfi-14
  (char-set?
   char-set= char-set<=
   char-set-hash 
   char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
   char-set-fold char-set-unfold char-set-unfold!
   char-set-for-each char-set-map
   char-set-copy char-set
   list->char-set  string->char-set 
   list->char-set! string->char-set! 
   char-set-filter  ucs-range->char-set  ->char-set
   char-set-filter! ucs-range->char-set!
   char-set->list char-set->string
   char-set-size char-set-count char-set-contains?
   char-set-every char-set-any
   char-set-adjoin  char-set-delete 
   char-set-adjoin! char-set-delete!
   char-set-complement  char-set-union  char-set-intersection  
   char-set-complement! char-set-union! char-set-intersection! 
   char-set-difference  char-set-xor  char-set-diff+intersection
   char-set-difference! char-set-xor! char-set-diff+intersection!
   char-set:lower-case		char-set:upper-case	char-set:title-case
   char-set:letter		char-set:digit		char-set:letter+digit
   char-set:graphic		char-set:printing	char-set:whitespace
   char-set:iso-control		char-set:punctuation	char-set:symbol
   char-set:hex-digit		char-set:blank		char-set:ascii
   char-set:empty		char-set:full)
  (import srfi-1) ;List functions
  (import srfi-60) ;Bitwise operations
  (import* optional-args (check-arg))
  (import srfi-14-data)
  (import srfi-14-charsets)

  (include "../../modules/srfi/srfi-14/support.scm")
  (include "../../modules/srfi/srfi-14/srfi-14.scm")
  (add-feature 'srfi-14))
