(import generic-procedures)
(import oo)

;;performance test

(define-class (<a>))
(define-class (<b>))
(define-class (<c> <a> <b>))
(define-class (<d> <b> <a>))

(define-generic m)
(define-method (m (<a> x) (<a> y)) 'aa)
(define-method (m (<a> x) (<b> y)) 'ab)
(define-method (m (<a> x) (<c> y)) 'ac)
(define-method (m (<a> x) (<d> y)) 'ad)
(define-method (m (<b> x) (<a> y)) 'ba)
(define-method (m (<b> x) (<b> y)) 'bb)
(define-method (m (<b> x) (<c> y)) 'bc)
(define-method (m (<b> x) (<d> y)) 'bd)
(define-method (m (<c> x) (<a> y)) 'ca)
(define-method (m (<c> x) (<b> y)) 'cb)
(define-method (m (<c> x) (<c> y)) 'cc)
(define-method (m (<c> x) (<d> y)) 'cd)
(define-method (m (<d> x) (<a> y)) 'da)
(define-method (m (<d> x) (<b> y)) 'db)
(define-method (m (<d> x) (<c> y)) 'dc)
(define-method (m (<d> x) (<d> y)) 'dd)
(define-method (m . rest) 'rr)

(define a (make <a>))
(define b (make <b>))
(define c (make <c>))
(define d (make <d>))

(time
 100
 (cons (m a b c d)
       (map m
            (list a a a a b b b b c c c c d d d d)
            (list a b c d a b c d a b c d a b c d))))
