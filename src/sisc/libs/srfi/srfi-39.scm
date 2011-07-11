;;TODO: find a way to re-export |parameterize|
(module _srfi-39 (_make_parameter)
  (define _make_parameter make-parameter))
(module srfi-39 (make-parameter)
  (import _srfi-39)
  (define make-parameter _make-parameter)
  (add-feature 'srfi-39))
