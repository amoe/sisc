;; 
;; The contents of this file are subject to the Mozilla Public
;; License Version 1.1 (the "License"); you may not use this file
;; except in compliance with the License. You may obtain a copy of
;; the License at http://www.mozilla.org/MPL/
;; 
;; Software distributed under the License is distributed on an "AS
;; IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the License for the specific language governing
;; rights and limitations under the License.
;; 
;; The Original Code is the Second Interpreter of Scheme Code (SISC).
;; 
;; The Initial Developer of the Original Code is Scott G. Miller.
;; Portions created by Scott G. Miller are Copyright (C) 2000-2007
;; Scott G. Miller.  All Rights Reserved.
;; 
;; Contributor(s):
;; Matthias Radestock 
;; 
;; Alternatively, the contents of this file may be used under the
;; terms of the GNU General Public License Version 2 or later (the
;; "GPL"), in which case the provisions of the GPL are applicable 
;; instead of those above.  If you wish to allow use of your 
;; version of this file only under the terms of the GPL and not to
;; allow others to use your version of this file under the MPL,
;; indicate your decision by deleting the provisions above and
;; replace them with the notice and other provisions required by
;; the GPL.  If you do not delete the provisions above, a recipient
;; may use your version of this file under either the MPL or the
;; GPL.
;;
;; SISC initialization file
;; Code from various sources, about 80% original.  If you notice
;; something you wrote, let me know.
;;;;;;

(define hedged-inlining       (_make-native-parameter "hedgedInlining"))
(hedged-inlining #f)

(define-syntax boolean-or
  (syntax-rules ()
    ((_) #t)
    ((_ v) v)
    ((_ v1 v2) (if v1 #t v2))
    ((_ v1 vs ...) (if v1 #t (boolean-or vs ...)))))
    
(define (for-each proc ls1 . lists)
  (unless (null? ls1)
    (begin
      (set! lists (cons ls1 lists))
      (apply proc (map-car lists))
      (apply for-each proc (map-cdr lists)))))
  
(define (eof-object? x) (eq? x #!eof))
;;;;;;;;; Standard Scheme functions

(define (not x) (if x #f #t))

(define (newline . port)
  (apply display #\newline port))

(define reverse
   (letrec [(iter 
             (lambda (ls acc)
               (if (null? ls) acc
                   (iter (cdr ls) (cons (car ls) acc)))))]
     (lambda (ls)	
       (iter ls '()))))

(define reverse!
  (letrec [(iter (lambda (s r)
                   (if (null? s) r
                       (let ((d (cdr s)))
                         (set-cdr! s r)
                         (iter d s)))))]
    (lambda (s)
      (iter s '()))))

    
(define (map-car ls)
  (if (null? ls) '()
      (cons (caar ls) (map-car (cdr ls)))))

(define (map-cdr ls)
  (if (null? ls) '()
      (cons (cdar ls) (map-cdr (cdr ls)))))
  
(define map
  (letrec ([map1 (lambda (proc list acc)
                   (if (null? list)
                       (reverse acc)
                       (map1 proc (cdr list) 
                             (cons (proc (car list)) acc))))]
           [loop (lambda (proc list1 lists c)
                   (if (null? list1)
                       (reverse c)
                       (loop proc (cdr list1)
                             (map-cdr lists)
                             (cons (apply proc
                                          (car list1)
                                          (map-car lists))
                                   c))))])
    (lambda (proc list1 . lists)
      (if (null? lists)
          (map1 proc list1 '())
          (loop proc list1 lists '())))))

(define (compose2 f g)
  (lambda (x) (f (g x))))

(define assq (void))
(define assv (void))
(define assoc (void))
(define memq (void))
(define memv (void))
(define member (void))

(letrec ([assN
      (lambda (N obj alist)
        (cond [(null? alist) #f]
    	  [(N (caar alist) obj) (car alist)]
    	  [else (assN N obj (cdr alist))]))]
     [memN
      (lambda (N obj list)
        (cond [(null? list) #f]
    	  [(N (car list) obj) list]
    	  [else (memN N obj (cdr list))]))])
  (set! assq (lambda (obj alist) (assN eq? obj alist)))
  (set! assv (lambda (obj alist) (assN eqv? obj alist)))
  (set! assoc (lambda (obj alist) (assN equal? obj alist)))
  (set! memq (lambda (obj list) (memN eq? obj list)))
  (set! memv (lambda (obj list) (memN eqv? obj list)))
  (set! member (lambda (obj list) (memN equal? obj list))))

;c....r
(define cadr (compose2 car cdr))
(define cdar (compose2 cdr car))
(define cddr (compose2 cdr cdr))
(define caar (compose2 car car)) 
(define caaar (compose2 car caar))
(define caadr (compose2 car cadr))
(define cadar (compose2 car cdar))
(define caddr (compose2 car cddr))
(define cdaar (compose2 cdr caar))
(define cdadr (compose2 cdr cadr))
(define cddar (compose2 cdr cdar))
(define cdddr (compose2 cdr cddr))
(define caaaar (compose2 car caaar))
(define caaadr (compose2 car caadr))
(define caadar (compose2 car cadar))
(define caaddr (compose2 car caddr))
(define cadaar (compose2 car cdaar))
(define cadadr (compose2 car cdadr))
(define caddar (compose2 car cddar))
(define cadddr (compose2 car cdddr))
(define cdaaar (compose2 cdr caaar))
(define cdaadr (compose2 cdr caadr))
(define cdadar (compose2 cdr cadar))
(define cdaddr (compose2 cdr caddr))
(define cddaar (compose2 cdr cdaar))
(define cddadr (compose2 cdr cdadr))
(define cdddar (compose2 cdr cddar))
(define cddddr (compose2 cdr cdddr))

(define append2 
  (lambda (ls1 ls2)
    (if (null? ls1) ls2
    (cons (car ls1) (append2 (cdr ls1) ls2)))))
(define append append2)

(define (_make-left-pairwise-nary proc base-case)
  (letrec ([helper
            (lambda (acc argls)
              (if (null? argls)
                  acc
                  (helper (proc acc (car argls))
                          (cdr argls))))])
    (lambda args
      (if (null? args)
          base-case
          (helper (car args) (cdr args))))))

;;;;;;;;;;;;;;; Conversion functions

(define list->string
  (letrec ([l2s
            (lambda (l s n)
              (if (null? l) 
                  s 
                  (begin (string-set! s n (car l))
                         (l2s (cdr l) s (+ n 1)))))])
    (lambda (l)
      (l2s l (make-string (length l)) 0))))

(define string->list
  (letrec [(s2l
            (lambda (s h n)
              (if (< n 0) 
                  h
                  (s2l s (cons (string-ref s n) h) (- n 1)))))]
    (lambda (s)
      (s2l s '() (- (string-length s) 1)))))

;;;;;;;;;;;;; Constructors

;(if (lookup 'list) (void)
;    (define list (lambda args args))
(define (vector . elems) (list->vector elems))
(define (string . elems) (list->string elems))

;;;;;;;;;;;;; File functions

(define current-url (_make-parameter "file:."))

(define (current-directory . rest)
  (if (null? rest)
      (normalize-url (current-url) ".")
      (current-url
       (normalize-url (current-url)
                      (let* ([v (car rest)]
                             [l (string-length v)])
                        (if (eqv? (string-ref v (- l 1)) '#\/)
                            v
                            (string-append v "/")))))))

(define file-handler)
(define add-file-handler)
(let ([*FILE-HANDLERS* '()])
  (set! add-file-handler 
        (lambda (extension thunk)
          (if (not (assq extension *FILE-HANDLERS*))
              (set! *FILE-HANDLERS*
                    (cons (cons extension thunk) *FILE-HANDLERS*)))))
  (set! file-handler
    (let ([_load load])
      (lambda (extension)
        (cond [(assq (string->symbol (string-downcase extension))
                     *FILE-HANDLERS*)
               => cdr]
              [else _load])))))

(define (make-io-proc proc)
  (lambda (file . rest)
    (apply proc (normalize-url (current-url) file) rest)))

(let ([file-extension
       (lambda (url)
         (let loop ((x (reverse! (string->list url)))
                    (acc '()))
           (cond [(null? x) #f]
                 [(equal? (car x) #\.)
                  (list->string acc)]
                 [else (loop (cdr x) (cons (car x) acc))])))])
    (set! open-input-file (make-io-proc open-input-file))
    (set! open-source-input-file (make-io-proc open-source-input-file))
    (set! open-output-file (make-io-proc open-output-file))
    (set! load
      (lambda (file)
        (let ([previous-url (current-url)])
          (current-url (normalize-url previous-url file))
          (with-failure-continuation
              (lambda (m e)
                (current-url previous-url)
                (call-with-failure-continuation (lambda (fk) (fk m e))))
            (lambda () 
              (let ((fe (file-extension (current-url))))
                ((file-handler (if fe fe "scm"))
                 (current-url)))))
          (current-url previous-url))
        (void))))

(define (load-module str)
  (let* ([nl (load-native-library str)]
     [binding-names (native-library-binding-names nl)])
    (for-each (lambda (name)
    	(putprop name (native-library-binding nl name)))
          binding-names)))
    
;;;;;;;;;;;;; Optimized functions

(define append (_make-left-pairwise-nary append2 '()))

; True only if the list is proper (not circular and terminated with null)
(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
        (let ((x (cdr x)))
          (if (pair? x)
              (let ((x   (cdr x))
                    (lag (cdr lag)))
                (and (not (eq? x lag)) (lp x lag)))
              (null? x)))
        (null? x))))

(define list? proper-list?)

;  (letrec [(list-h? 
;            (trace-lambda lh (lsp1 lsp2)
;              (or (null? lsp1) (null? lsp2)
;                  (and 
;                   (pair? lsp2)
;                   (or 
;                    (null? (cdr lsp2))
;                    (and (not (eq? lsp1 lsp2))
;                         (list-h? (cdr lsp1) (cddr lsp2))))))))]
;    (lambda (lsc)
;      (or (null? lsc) (and  (pair? lsc) (list-h? lsc (cdr lsc)))))))


;;;;;;;;;;;;;; Math functions/constants
(define expt 
  (letrec ([general-expt
        (lambda (base exponent)
          (exp (* exponent (log base))))]
           [rational-expt
            (lambda (base-numerator base-denominator exponent)
              (/ (expt base-numerator exponent)
                 (expt base-denominator exponent)))]
       [integer-expt 
        (lambda (base exponent)
          (if (and (exact? base) (= base 2))
                  (if (negative? exponent)
                      (/ (ashl 1 (abs exponent)))
                      (ashl 1 exponent))
                  (let loop ([rest      (if (negative? exponent)
                                            (abs exponent)
                                            exponent)]
                             [result    1]
                             [squaring  (if (negative? exponent)
                                            (/ base)
                                            base)])
                    (if (zero? rest)
                        result
                        (loop
                         (quotient rest 2)
                         (if (odd? rest)
                             (* result squaring)
                             result)
                         (* squaring squaring))))))])
    (lambda (base exponent)
      (cond
       ((zero? exponent) 
        (if (and (exact? base) (exact? exponent)) #e1 #i1))
       ((zero? base) 
        (if (exact? exponent) base #i0))       
       ((and (exact? base) (rational? base) (not (integer? base)))
        (rational-expt (numerator base) (denominator base) exponent))
       ((and (exact? exponent) (integer? exponent))
        (integer-expt base exponent))
       (else (general-expt base exponent))))))
  
(define (modpow x y N)
  (if (= y 1) 
      (modulo x N)
      (if (even? y)
      (let ([tmp (modpow x (/ y 2) N)])
        (modulo (* tmp tmp) N))
      (let ([tmp (modpow x (/ (- y 1) 2) N)])
        (modulo (* x (modulo (* tmp tmp) N)) N)))))

(define integer?
  (lambda (n)
    (boolean-or (_integer? n) 
                (and (real? n)
                     (= (round n) n)))))

(define real? 
 (let ((oldcomp? complex?))
  (lambda (n)
    (and (number? n) (not (oldcomp? n))))))

(define rational? real?)
(define complex? number?)

(define (abs num) 
  (if (real? num)
      (if (< num 0) (- num) num)
      (let ([a (real-part num)]
            [b (imag-part num)])
        (sqrt (+ (* a a) (* b b))))))

(define min (void))
(define max (void))
(letrec ([_min_max 
      (lambda (proc mv args inexact)
        (cond [(null? args) 
    	   (if (and inexact (exact? mv)) 
    	       (exact->inexact mv)
    	       mv)]
    	  [(proc (car args) mv) 
    	   (_min_max proc (car args) (cdr args)
    		     (boolean-or inexact (inexact? (car args))))]
    	  [else (_min_max proc mv (cdr args) inexact)]))])
  (set! min (lambda (x1 . args)
          (if (null? args) 
                  x1
    	  (_min_max < x1 args
    		    (inexact? x1)))))
  (set! max (lambda (x1 . args)
          (if (null? args) 
                  x1
    	  (_min_max > x1 args
    		    (inexact? x1))))))

(define (negative? n) (< n 0))
(define (positive? n) (> n 0))
(define (even? n) (= 0 (modulo n 2)))
(define (odd? n) (not (even? n)))
(define (zero? n) (= n 0))
(define (add1 n) (+ n 1))
(define (sub1 n) (- n 1))

(define >= (void))
(define <= (void))
(let ([_comp_help
      (lambda (comparator chainer endstate)
    (lambda args
      (let loop ([x args])
        (cond [(null? x) endstate]
    	  [(null? (cdr x)) endstate]
    	  [else (chainer (comparator (car x) (cadr x))
    			 (loop (cdr x)))]))))]
      [_and2 (lambda (x y) (and x y))])
  
  (set! <= (_comp_help (lambda (a b) (boolean-or (< a b) (= a b))) _and2 #t))
  (set! >= (_comp_help (lambda (a b) (boolean-or (> a b) (= a b))) _and2 #t)))


(let ([_?= (lambda (comparator chainer)
         (lambda args
           (boolean-or (null? args) 
    	   (null? (cdr args))
    	   (and (or (= (car args) (cadr args))
    		    (comparator (car args) (cadr args)))
    		(apply chainer (cdr args))))))])
  (set! >= (_?= > >=))
  (set! <= (_?= < <=)))

(define (gcd . args)
   (cond [(null? args) 0]
	     [(null? (cdr args)) (car args)]
         [else (_gcd (car args) (apply gcd (cdr args)))]))

(define (lcm . args)
   (cond [(null? args) 1]
     	 [(null? (cdr args)) (car args)]
         [else (_lcm (car args) (apply lcm (cdr args)))]))

(define modulo
  (lambda (x y)
    (let ([r (remainder x y)])
       (if ((if (negative? y) positive? negative?) r)
           (+ r y)
           r))))

;;;;;;;;;;;;;; String functions
(define char-downcase
  (let* ((a (char->integer #\A))
         (z (char->integer #\Z))
         (lc-offset (- (char->integer #\a) a)))
    (lambda (c) 
      (let ((cv (char->integer c)))
        (if (and (>= cv a) (<= cv z))
            (integer->char (+ cv lc-offset))
            c)))))

(define char-upcase
  (let* ((a (char->integer #\a))
         (z (char->integer #\z))
         (uc-offset (- a (char->integer #\A))))
    (lambda (c) 
      (let ((cv (char->integer c)))
        (if (and (>= cv a) (<= cv z))
            (integer->char (- cv uc-offset))
            c)))))

(define (char>? c1 c2 . args) (apply > (char->integer c1) (char->integer c2)
                                     (map char->integer args)))
(define (char<? c1 c2 . args) (apply < (char->integer c1) (char->integer c2)
                                     (map char->integer args)))
(define (char>=? c1 c2) (boolean-or (char>? c1 c2) (char=? c1 c2)))
(define (char<=? c1 c2) (boolean-or (char<? c1 c2) (char=? c1 c2)))
(define (char-ci>? c1 c2 . args)
  (apply char>? (char-downcase c1) (char-downcase c2)
         (map char-downcase args)))
(define (char-ci<? c1 c2 . args)
  (apply char<? (char-downcase c1) (char-downcase c2)
         (map char-downcase args)))
(define (char-ci=? c1 c2 . args)
  (apply char=? (char-downcase c1) (char-downcase c2)
         (map char-downcase args)))
(define (char-ci>=? c1 c2) (boolean-or (char-ci>? c1 c2) (char-ci=? c1 c2)))
(define (char-ci<=? c1 c2) (boolean-or (char-ci<? c1 c2) (char-ci=? c1 c2)))

(define (char-alphabetic? c) (boolean-or (char<? #\@ c #\[)
                                         (char<? #\` c #\{)))
(define (char-numeric? c) (char<? #\/ c #\:))
(define (char-whitespace? c) (boolean-or (char=? c #\space)
                                         (char=? c #\tab)
                                         (char=? c #\newline)
                                         (char=? c #\return)))

(define (char-upper-case? c) (char<? #\@ c #\[))
(define (char-lower-case? c) (char<? #\` c #\{))

(define string-downcase (void))
(define string-upcase (void))
(letrec ([string-map
      (lambda (strsrc strdst proc n l)
        (if (< n l)
    	(begin 
    	  (string-set! strdst n (proc (string-ref strsrc n))) 
    	  (string-map strsrc strdst proc (+ n 1) l))
    	strdst))])
  (set! string-downcase 
    (lambda (str)
      (let ([newstr (make-string (string-length str))])
        (string-map str newstr char-downcase 0 
    		(string-length str)))))
  (set! string-upcase 
    (lambda (str)
      (let ([newstr (make-string (string-length str))])
        (string-map str newstr char-upcase 0 
    		(string-length str))))))

(define string<?
  (letrec ([s<? (lambda (s1 s2)
    	  (cond [(null? s1) (not (null? s2))]
    		[(null? s2) #f]
    		[else (let ([c1 (car s1)]
    			    [c2 (car s2)])
    			(cond [(char<? c1 c2) #t]
    			      [(char>? c1 c2) #f]
    			      [else (s<? (cdr s1) (cdr s2))]))]))])
    (lambda (s1 s2)
      (s<? (string->list s1) (string->list s2)))))

(define string>?
  (letrec ([s>? (lambda (s1 s2)
    	  (cond [(null? s2) (not (null? s1))]
    		[(null? s1) #f]
    		[else (let ([c1 (car s1)]
    			    [c2 (car s2)])
    			(cond [(char>? c1 c2) #t]
    			      [(char<? c1 c2) #f]
    			      [else (s>? (cdr s1) (cdr s2))]))]))])
    (lambda (s1 s2)
      (s>? (string->list s1) (string->list s2)))))

(define (string<=? s1 s2) (boolean-or (string<? s1 s2) (string=? s1 s2)))
(define (string>=? s1 s2) (boolean-or (string>? s1 s2) (string=? s1 s2)))
(define (string-ci=? s1 s2) 
  (string=? (string-downcase s1) (string-downcase s2)))
(define (string-ci<? s1 s2)
  (string<? (string-downcase s1) (string-downcase s2)))
(define (string-ci>? s1 s2)
  (string>? (string-downcase s1) (string-downcase s2)))
(define (string-ci>=? s1 s2)
  (string>=? (string-downcase s1) (string-downcase s2)))
(define (string-ci<=? s1 s2)
  (string<=? (string-downcase s1) (string-downcase s2)))

(define substring
  (letrec ([fill-string
        (lambda (sstr dstr n s e)
          (if (< s e)
    	  (begin
    	    (string-set! dstr n (string-ref sstr s))
    	    (fill-string sstr dstr (+ n 1) (+ s 1) e))))])
    (lambda (str start end)
      (let ([newstr (make-string (- end start))])
    (fill-string str newstr 0 start end)
    newstr))))

;;;;;;;;;;;;; Miscellaneous

(define (list-ref list n)
  (if (zero? n) (car list) (list-ref (cdr list) (- n 1))))

(define (values . args)
  (call-with-current-continuation
   (lambda (k) 
     (apply k args))))

