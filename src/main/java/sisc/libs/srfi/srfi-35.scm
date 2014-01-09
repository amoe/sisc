(require-library 'sisc/libs/srfi/srfi-1)
(require-library 'sisc/libs/srfi/srfi-23)

(module srfi-35
    (make-condition-type
     condition-type?
     make-condition
     condition?
     condition-has-type?
     condition-ref
     make-compound-condition
     extract-condition
     define-condition-type
     (condition type-field-alist->condition)
     &condition
     &message
     message-condition?
     condition-message
     &serious
     serious-condition?
     &error
     error?)
  (import srfi-1)
  (import record)
  (import srfi-23)
  (include "../../modules/srfi/srfi-35.scm")
  (add-feature 'srfi-35))
