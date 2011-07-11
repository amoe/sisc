(require-library 'sisc/libs/srfi/srfi-23)

(define-interface srfi-42-exports
  (do-ec
   list-ec
   append-ec
   string-ec
   string-append-ec
   vector-ec
   vector-of-length-ec
   sum-ec
   product-ec
   min-ec
   max-ec
   any?-ec
   every?-ec
   first-ec
   last-ec
   fold-ec
   fold3-ec))

;;the following really shouldn't be exported, but because
;;comprehensions assume generators are macros in their own right we
;;need to do this.
(define-interface srfi-42-implicit-exports
  (do-ec:do
   ec-simplify
   ec-guarded-do-ec
   ec-:vector-filter
   :do
   :let
   :parallel :parallel-1
   :while :while-1 :while-2
   :until :until-1
   :list
   :string
   :vector
   :integers
   :range
   :real-range
   :char-range
   :port
   :dispatched
   :
   :-dispatch))

(define-compound-interface srfi-42-interface
  (srfi-42-exports srfi-42-implicit-exports))

(define-module srfi-42 srfi-42-interface
  (import srfi-23)
  (include "../../modules/srfi/srfi-42/ec.scm")
  (add-feature 'srfi-42))
