(require-library 'sisc/libs/srfi/r5rs)
(require-library 'sisc/libs/srfi/srfi-8)
(require-library 'sisc/libs/srfi/srfi-14)
(require-library 'sisc/libs/srfi/optional-args)

(module srfi-13
  (string-map
   string-map!
   string-fold       string-unfold
   string-fold-right string-unfold-right 
   string-tabulate string-for-each string-for-each-index
   string-every string-any
   string-hash string-hash-ci
   string-compare string-compare-ci
   string=    string<    string>    string<=    string>=    string<>
   string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
   string-downcase  string-upcase  string-titlecase  
   string-downcase! string-upcase! string-titlecase! 
   string-take string-take-right
   string-drop string-drop-right
   string-pad string-pad-right
   string-trim string-trim-right string-trim-both
   string-filter string-delete
   string-index string-index-right 
   string-skip  string-skip-right
   string-count
   string-prefix-length string-prefix-length-ci
   string-suffix-length string-suffix-length-ci
   string-prefix? string-prefix-ci?
   string-suffix? string-suffix-ci?
   string-contains string-contains-ci
   string-copy! substring/shared
   string-reverse string-reverse! reverse-list->string
   string-concatenate string-concatenate/shared
   string-concatenate-reverse string-concatenate-reverse/shared
   string-append/shared
   xsubstring string-xcopy!
   string-null?
   string-join
   string-tokenize
   string-replace
   string->list string-copy string-fill! 
   string? make-string string-length string-ref string-set! 
   string string-append list->string
   make-kmp-restart-vector string-kmp-partial-search kmp-step
   string-parse-start+end
   string-parse-final-start+end
   let-string-start+end
   check-substring-spec
   substring-spec-ok?)
  (import* r5rs 
           string? make-string string-length string-ref string-set!
           string string-append list->string)
  (import srfi-8) ;RECEIVE
  (import srfi-14)
  (import optional-args)
  (define char-cased? char-alphabetic?)
  (define char-titlecase char-upcase)
  (include "../../modules/srfi/srfi-13.scm")
  (add-feature 'srfi-13))
