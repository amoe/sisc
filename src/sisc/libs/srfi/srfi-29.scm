(require-library 'sisc/libs/srfi/srfi-6)

(module srfi-29
    (format
     current-country current-language current-locale-details
     declare-bundle! store-bundle load-bundle! 
     localized-template)
  (import s2j)
  (import string-io)
  (include "../../modules/srfi/srfi-29.scm")
  (add-feature 'srfi-29))
