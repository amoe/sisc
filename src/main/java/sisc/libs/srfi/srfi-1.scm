(require-library 'sisc/libs/srfi/r5rs)
(require-library 'sisc/libs/srfi/optional-args)
(require-library 'sisc/libs/srfi/srfi-8)

(module srfi-1
  (cons
   list
   xcons cons* make-list list-tabulate list-copy circular-list iota
   pair? null?
   proper-list? circular-list? dotted-list? not-pair? null-list?
   list=
   car cdr list-ref
   caar cadr cdar cddr
   caaar caadr cadar caddr cdaar cdadr cddar cdddr
   caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
   cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
   first second third fourth fifth sixth seventh eighth ninth tenth
   car+cdr
   take drop
   take-right drop-right
   take! drop-right! split-at split-at! last last-pair
   length length+
   append concatenate reverse
   append! concatenate! reverse!
   append-reverse append-reverse!
   zip unzip1 unzip2 unzip3 unzip4 unzip5
   count
   map for-each
   fold unfold pair-fold reduce fold-right unfold-right pair-fold-right
   reduce-right append-map append-map!
   map! pair-for-each filter-map map-in-order
   filter partition remove
   filter! partition! remove!
   member memq memv
   find find-tail any every
   list-index
   take-while drop-while take-while!
   span break span! break!
   delete delete-duplicates delete! delete-duplicates!
   assoc assq assv
   alist-cons alist-copy
   alist-delete alist-delete!
   lset<= lset= lset-adjoin
   lset-union lset-union!
   lset-intersection lset-intersection!
   lset-difference lset-difference!
   lset-xor lset-xor!
   lset-diff+intersection lset-diff+intersection!)
  (import* r5rs
           for-each
           cons pair? null? car cdr set-car! set-cdr!
           list append reverse length list-ref
           memq memv assq assv
           caar cadr cdar cddr
           caaar caadr cadar caddr cdaar cdadr cddar cdddr
           caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
           cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr)
  (import optional-args)
  (import srfi-8) ;RECEIVE
  (include "../../modules/srfi/srfi-1.scm")
  (add-feature 'srfi-1))
