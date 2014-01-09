;;; array opt
;;; 1997 - 2001 Jussi Piitulainen

;;; This is PLT magic.
(require-library "pretty.ss")
(pretty-print-columns 60)
(print-vector-length #f)
(define (opt:file name new-type . spec)
  (set! type new-type)
  (if (file-exists? name)
      (delete-file name))
  (with-output-to-file name
    (lambda ()
      (pretty-print (apply opt:make spec)))))

(define (mk)
  (opt:file "op-ctor.scm" 'ctor '(4))
  (opt:file "op-mbda.scm" 'mbda '(3 -1 0 1) '(4 0 1))
  (opt:file "op-tter.scm" 'tter '(3 -1 0 1) '(4 0)))

; (opt:file "op-mize.scm" 'tter '(3 -1 0 1) '(4 0) '(8))
; (opt:file "op-zero.scm" 'mbda '(0))

;;; Execute (opt:make '(4 -1 0 1) '(20))
;;; to display the source code
;;; for an optimizer that handles dimensions below 4 with name
;;; "array:optimize" best and up to 20 second best.

(define (opt:lib name)
  (string->symbol (string-append"array:" name)))

;;; There are three types:
;;;     mbda uses single indexing procedure;
;;;     ctor uses vector of coefficients;
;;;     tter provides indexing procedure and setter.

(define type 'mbda)

;;; Apply depth refers to several function families that turn vector
;;; or actor elements into arguments. (An actor, of course, is an
;;; array that is a vector: 0-based, 1-dimensional.) To this depth,
;;; application does not allocate argument lists and is, therefore,
;;; efficient. (No, these are not Hewitt's actors.)

(define opt:apply-depth 10)

;;; (opt:make dcs ...)
;;; makes the optimizer code `array:optimize' for dimensions below last dcs.

(define (opt:make . dcss)
  (let ((past (caar (reverse dcss))))
    `(begin
       (define ,(opt:lib "opt-args") ',(cons type dcss)) ;comment
       (define (,(opt:lib "optimize") f r)
         (case r
           ,@(map opt:dispatch (opt:between 0 past))
           (else
            (let ((v (do ((k 0 (+ k 1))
                          (v '() (cons 0 v)))
                       ((= k r) v))))
              (let ((n0 (apply f v)))
                ,(case type
                   ((mbda ctor)
                    `(apply
                      ,(opt:lib "n")
                      n0 (,(opt:lib "coefficients") f n0 v v)))
                   ((tter)
                    `(let ((cs (,(opt:lib "coefficients") f n0 v v)))
                       "bug -- the cons should be in array:n"
                       (cons (apply ,(opt:lib "n") n0 cs)
                             (apply ,(opt:lib "n!") n0 cs))))
                   (else `(unknown type ,type for last branch))))))))
       (define (,(opt:lib "optimize-empty") r)
         ,(case type
            ((mbda) '(lambda ks
                       (if (= (length ks) r)
                           -1
                           (values -1 (car (reverse ks))))))
            ((ctor) '(let ((x (make-vector (+ r 1) 0)))
                       (vector-set! x r -1)
                       x))
            ((tter) '(cons (lambda ks -1)
                           (lambda (v . kso)
                             (vector-set! v -1 (car (reverse kso))))))
            (else `(unknown type ,type))))
       (define (,(opt:lib "coefficients") f n0 vs vp)
         (case vp
           ((()) '())
           (else
            (set-car! vp 1)
            (let ((n (- (apply f vs) n0)))
              (set-car! vp 0)
              (cons n
                    (,(opt:lib "coefficients") f n0 vs (cdr vp)))))))
       ;;
       ;; (array:vector-index x ks) to compute index to backing vector
       ;; (array:shape-index)       to represent index in shape 
       ;; (array:empty-shape-index) to represent index in empty shape
       ;; (array:shape-vector-index x r k)
       ;;   to compute index to backing vector of shape
       ;; (array:actor-index x k)
       ;;   to compute index to backing vector of array that is vector
       ;;
       ;; These could be syntax for inlining.
       ;;
       (define (,(opt:lib "vector-index") x ks)
         ,(case type
            ((mbda) '(apply x ks))
            ((tter) '(apply (car x) ks))
            ((ctor) '(do ((sum 0 (+ sum (* (vector-ref x k) (car ks))))
                          (ks ks (cdr ks))
                          (k 0 (+ k 1)))
                       ((null? ks)
                        (+ sum (vector-ref x k)))))
            (else `(unknown type ,type to vector-index))))
       (define (,(opt:lib "shape-index"))
         ,(case type
            ((mbda) '(lambda (r k . o)
                       (if (null? o)
                           (+ r r k)
                           (values (+ r r k) (car o)))))
            ((tter) '(cons (lambda (r k)
                             (+ r r k))
                           (lambda (v r k o)
                             (vector-set! v (+ r r k) o))))
            ((ctor) ''#(2 1 0))
            (else `(unknown type ,type for shape-index))))
       (define (,(opt:lib "empty-shape-index"))
         ,(case type
            ((mbda) '(lambda (r k . o)
                       (if (null? o)
                           -1
                           (values -1 (car o)))))
            ((tter) '(cons (lambda (r k)
                             -1)
                           (lambda (v r k o)
                             (vector-set! v -1 o))))
            ((ctor) ''#(0 0 -1))
            (else `(unknown type ,type for empty-shape-index))))
       (define (,(opt:lib "shape-vector-index") x r k)
         ,(case type
            ((mbda) '(x r k))
            ((tter) '((car x) r k))
            ((ctor) '(+ (* (vector-ref x 0) r)
                        (* (vector-ref x 1) k)
                        (vector-ref x 2)))
            (else `(unknown type ,type for shape-vector-index))))
       (define (,(opt:lib "actor-index") x k)
         ,(case type
            ((mbda) '(x k))
            ((tter) '((car x) k))
            ((ctor) '(+ (* (vector-ref x 0) k)
                        (vector-ref x 1)))
            (else `(unknown type ,type to actor-index))))
       ,@(opt:make-optimizers dcss)
       ;;
       ;; It is important that the end condition in mbda is
       ;; on `ns', not on `ks', for `array-set!' to work.
       ;;
       (define (,(opt:lib "n") ,@(opt:names past "n") . ns)
         ,(case type
            ((mbda tter)
             `(lambda (,@(cdr (opt:names past "k")) . ks)
                (do ((ns ns (cdr ns))
                     (ks ks (cdr ks))
                     (dx (+ n0
                            ,@(map (lambda (n k)
                                     `(* ,n ,k))
                                   (cdr (opt:names past "n"))
                                   (cdr (opt:names past "k"))))
                         (+ dx (* (car ns) (car ks)))))
                  ((null? ns)
                   ,(case type
                      ((mbda)
                       '(if (null? ks)
                            dx
                            (values dx (car ks))))
                      ((tter)
                       'dx)
                      (else `(unknown type ,type for lib:n value)))))))
            ((ctor) `(apply vector
                            ,@(cdr (opt:names past "n")) ;n1 ...
                            (append ns (list n0))))
            (else `(unknown type ,type))))
       ,@(case type
           ((tter)
            `((define (,(opt:lib "n!") ,@(opt:names past "n") . ns)
                (lambda (v ,@(cdr (opt:names past "k")) . ks)
                  (do ((ns ns (cdr ns))
                       (ks ks (cdr ks))
                       (dx (+ n0
                              ,@(map (lambda (n k)
                                       `(* ,n ,k))
                                     (cdr (opt:names past "n"))
                                     (cdr (opt:names past "k"))))
                           (+ dx (* (car ns) (car ks)))))
                    ((null? ns)
                     (vector-set! v dx (car ks))))))))
           (else '()))
       (define (,(opt:lib "maker") r)
         (case r
           ,@(map (lambda (k)
                    `((,k) ,(opt:case-name k '() "")))
                  (opt:between 0 past))
           (else
            ,(case type
               ((mbda ctor)
                (opt:lib "n"))
               ((tter)
                (let ((rgs (opt:names past "n")))
                  `(lambda (,@rgs . ns)
                     "bug -- the cons should be in array:n"
                     (cons
                      (apply ,(opt:lib "n") ,@rgs ns)
                      (apply ,(opt:lib "n!") ,@rgs ns)))))
               (else `(unknown type ,type for maker))))))
       (define ,(opt:lib "indexer/vector")
         (let ((em (vector ,@(map opt:vector-indexer
                                  (opt:between 0 opt:apply-depth))))
               (it ,(opt:past-vector-indexer opt:apply-depth)))
           (lambda (r)
             (if (< r ,opt:apply-depth)
                 (vector-ref em r)
                 (it r)))))
       (define ,(opt:lib "indexer/array")
         (let ((em (vector ,@(map opt:array-indexer
                                  (opt:between 0 opt:apply-depth))))
               (it ,(opt:past-array-indexer opt:apply-depth)))
           (lambda (r)
             ;; av is (array:vector a)
             ;; ai is (array:index a)
             (if (< r ,opt:apply-depth)
                 (vector-ref em r)
                 (it r)))))
       (define ,(opt:lib "applier-to-vector")
         (let ((em (vector ,@(map opt:vector-applier
                                  (opt:between 0 opt:apply-depth))))
               (it ,(opt:past-vector-applier opt:apply-depth)))
           (lambda (r)
             (if (< r ,opt:apply-depth)
                 (vector-ref em r)
                 (it r)))))
       (define ,(opt:lib "applier-to-actor")
         (let ((em (vector ,@(map opt:actor-applier
                                  (opt:between 0 opt:apply-depth))))
               (it ,(opt:past-actor-applier opt:apply-depth)))
           (lambda (r)
             "These are high level, hiding implementation at call site."
             (if (< r ,opt:apply-depth)
                 (vector-ref em r)
                 (it r)))))
       (define ,(opt:lib "applier-to-backing-vector")
         (let ((em (vector ,@(map opt:backing-vector-applier
                                  (opt:between 0 opt:apply-depth))))
               (it ,(opt:past-backing-vector-applier opt:apply-depth)))
           (lambda (r)
             "These are low level, exposing implementation at call site."
             (if (< r ,opt:apply-depth)
                 (vector-ref em r)
                 (it r)))))
       (define (,(opt:lib "index/vector") r x v)
         ((,(opt:lib "indexer/vector") r) x v))
       (define (,(opt:lib "index/array") r x av ai)
         ((,(opt:lib "indexer/array") r) x av ai))
       (define (,(opt:lib "apply-to-vector") r p v)
         ((,(opt:lib "applier-to-vector") r) p v))
       (define (,(opt:lib "apply-to-actor") r p a)
         ((,(opt:lib "applier-to-actor") r) p a)))))

(define (opt:between low past)
   (if (< low past)
      (cons low (opt:between (+ low 1) past))
      '()))

(define (opt:dispatch d)
  (let ((ks (opt:between 1 (+ d 1))))
    `((,d) (let ((n0 (f ,@(map (lambda (_) 0) ks))))
             (,(opt:case-name d '() "")
              n0
              ,@(map (lambda (j)
                       `(- (f ,@(map (lambda (k)
                                       (if (= j k) 1 0))
                                     ks))
                           n0))
                     ks))))))

;;; Expect dcss not empty.

(define (opt:make-optimizers dcss)
  (let ((es (map car dcss))
        (css (map cdr dcss)))
    (let ((bs (cons 0 (reverse (cdr (reverse es))))))
      (apply append
             (map (lambda (b e cs)
                    (apply append
                           (map (lambda (d) (opt:make-optimizer d cs))
                                (opt:between b e))))
                  bs
                  es
                  css)))))

;;; (opt:make-optimizer dcs)
;;; returns linear optimizer code for d dimensions with constants cs.

(define (opt:make-optimizer d cs)
  (let ((ns (opt:names d "n"))
        (cs (append cs '(()))))
    `((define (,(opt:case-name d '() "") ,@ns)
        ,(case type
           ((mbda tter)
            (if (memv 0 cs)
                `(if (= n0 0)
                     ,(opt:make-giant-case d cs '(0) (cdr ns) ns)
                     ,(opt:make-giant-case d cs '(()) (cdr ns) ns))
                (opt:make-giant-case d cs '(()) (cdr ns) ns)))
           ((ctor) `(vector ,@(cdr (opt:names d "n")) ;n1 ...
                            n0))
           (else `(unknown type ,type for giant case))))
      ,@(case type
          ((mbda tter) (opt:make-little-cases d cs ns (opt:names d "k")))
          ((ctor) '())
          (else `((unknown type ,type for little cases)))))))

(define (opt:names d n)
   (do      ((ns '() (cons (string-append n (number->string k)) ns))
         (k d (- k 1)))
     ((= k 0)
      (map string->symbol (cons (string-append n "0") ns)))))

(define (opt:make-giant-case d cs rvs ms ns)
   (if (pair? ms)
       `(case ,(car ms)
      ,@(map (lambda (v)
                   `(,(if (number? v)
                          (list v)
                          'else)
                     ,(opt:make-giant-case
                       d cs
                       (cons v rvs)
                       (cdr ms)
                       ns)))
                 cs))
       (let ((vs (reverse rvs)))
         (let ((rgs (apply append
                           (map (lambda (n v)
                                  (if (number? v)
                                      '()
                                      (list n)))
                                ns
                                vs))))
           (case type
             ((mbda) `(,(opt:case-name d vs "") ,@rgs))
             ((tter) `(cons (,(opt:case-name d vs "") ,@rgs)
                            (,(opt:case-name d vs "!") ,@rgs)))
             (else `(unknown type ,type for giant case)))))))

(define (opt:case-name d vs sx)
   (do      ((s (symbol->string (opt:lib (number->string d)))
    	(string-append
                 s
                 (if (and (number? (car ns))
                          (negative? (car ns)))
                     ""
                     "+")
                  (if (number? (car ns))
                      (number->string (car ns))
                      "n")))
         (ns vs (cdr ns)))
     ((null? ns)
      (string->symbol (string-append s sx)))))

;;; At end of known constants cs the unknown constant marker ().

(define (opt:make-little-cases d cs ns ks)
  (apply
   append
   (map (lambda (vs)
          (opt:make-little-case d ns vs ks))
        (apply opt:cross (if (memv 0 cs) '(0 ()) '(()))
               (vector->list (make-vector d cs))))))

;;; (opt:cross list ...)
;;; returns a list of lists that are all combinations of elements of
;;; lists.

(define (opt:cross . lists)
   (if (null? lists)
      '(())
      (apply append
     (map (lambda (pre)
    	 (map (lambda (suf)
    		 (cons pre suf))
    	    (apply opt:cross (cdr lists))))
        (car lists)))))

(define (opt:make-little-case d ns vs ks)
   (let ((exp `(+ ,(if (number? (car vs))
                       (car vs)
                       (car ns))
    	  ,@(map (lambda (n v k)
    		   `(* ,(if (number? v) v n) ,k))
    		 (cdr ns)
    		 (cdr vs)
    		 (cdr ks))))
         (rgs (append
    	     (if (number? (car vs))
    		'()
    		`(,(car ns)))
    	     (apply append
    		(map (lambda (n v)
    			(if (number? v) '() (list n)))
    		   (cdr ns)
    		   (cdr vs))))))
     `((define (,(opt:case-name d vs "") ,@rgs)
         ,(case type
            ((mbda)
             `(lambda (,@(cdr ks) . o)
                (if (null? o)
                    ,(opt:opt exp)
                    (values ,(opt:opt exp) (car o)))))
            ((tter)
             `(lambda (,@(cdr ks))
                ,(opt:opt exp)))
            (else `(unknown type ,type))))
       ,@(case type
           ((tter)
            `((define (,(opt:case-name d vs "!") ,@rgs)
                (lambda (v ,@(cdr ks) o)
                  (vector-set! v ,(opt:opt exp) o)))))
           (else '())))))


;;; (opt:opt sum)
;;; returns an optimized version of the sum. The sum is of the
;;; form (+ n0 (* n1 k1) ...) with some of nk constants.

;(define (opt:opt sum) sum)

(define (opt:opt sum)
  (apply (lambda (+ n0 . products)
           (let ((terms (apply append
                               (case n0
                                 ((0) '())
                                 (else `(,n0)))
                               (map (lambda (term)
                                      (apply (lambda (* n k)
                                               (case n
                                                 ((-1) `((- ,k)))
                                                 ((0) '())
                                                 ((1) `(,k))
                                                 (else `((* ,n ,k)))))
                                             term))
                                    products))))
             (case (length terms)
               ((0) 0)
               ((1) (car terms))
               (else `(+ . ,terms)))))
         sum))

(define (opt:vector-indexer r)
  (opt:index-indexer 'vector r))

(define (opt:past-vector-indexer r)
  (opt:past-index-indexer 'vector r))

(define (opt:array-indexer r)
  (opt:index-indexer 'actor r))

(define (opt:past-array-indexer r)
  (opt:past-index-indexer 'actor r))

(define (opt:index-indexer index-type r)
  (case type
    ((mbda tter)
     (let ((axe (case type
                  ((mbda) 'x)
                  ((tter) '(car x))
                  (else 'unknown-x))))
       (case index-type
         ((vector)
          `(lambda (x i)
             (,axe ,@(map (lambda (k)
                            `(vector-ref i ,k))
                          (opt:between 0 r)))))
         ((actor)
          `(lambda (x v i)
             (,axe ,@(map (lambda (k)
                            `(vector-ref v (array:actor-index i ,k)))
                          (opt:between 0 r)))))
         (else `(unknown index-type ,index-type for ,type indexer)))))
    ((ctor)
     (case index-type
       ((vector)
        `(lambda (x i)
           (+ ,@(map (lambda (k)
                       `(* (vector-ref x ,k)
                           (vector-ref i ,k)))
                     (opt:between 0 r))
              (vector-ref x ,r))))
       ((actor)
        `(lambda (x v i)
           (+ ,@(map (lambda (k)
                       `(* (vector-ref x ,k)
                           (vector-ref v (array:actor-index i ,k))))
                     (opt:between 0 r))
              (vector-ref x ,r))))
       (else `(unknown index-type ,index-type for ,type indexer))))
    (else `(unknown type ,type))))

(define (opt:past-index-indexer index-type r)
  (case type
    ((mbda tter)
     (let ((axe (case type
                  ((mbda) 'x)
                  ((tter) '(car x))
                  (else 'unknown-x))))
       (case index-type
         ((vector)
          `(lambda (w)
             (lambda (x i)
               (apply ,axe ,@(map (lambda (k)
                                    `(vector-ref i ,k))
                                  (opt:between 0 r))
                      (do ((ks '() (cons (vector-ref i u) ks))
                           (u (- w 1) (- u 1)))
                        ((< u ,r)
                         ks))))))
         ((actor)
          `(lambda (w)
             (lambda (x v i)
               (apply ,axe ,@(map (lambda (k)
                                    `(vector-ref v (array:actor-index i ,k)))
                                  (opt:between 0 r))
                      (do ((ks '() (cons (vector-ref
                                          v
                                          (array:actor-index i u))
                                         ks))
                           (u (- w 1) (- u 1)))
                        ((< u ,r)
                         ks))))))
         (else `(unknown index-type ,index-type for ,type past indexer)))))
    ((ctor)
     (case index-type
       ((vector)
        `(lambda (w)
           (lambda (x i)
             (+ ,@(map (lambda (k)
                         `(* (vector-ref x ,k)
                             (vector-ref i ,k)))
                       (opt:between 0 r))
                (do ((xi 0 (+ (* (vector-ref x u) (vector-ref i u)) xi))
                     (u (- w 1) (- u 1)))
                  ((< u ,r)
                   xi))
                (vector-ref x w)))))
       ((actor)
        `(lambda (w)
           (lambda (x v i)
             (+ ,@(map (lambda (k)
                         `(* (vector-ref x ,k)
                             (vector-ref v (array:actor-index i ,k))))
                       (opt:between 0 r))
                (do ((xi 0 (+ (* (vector-ref x u)
                                 (vector-ref v (array:actor-index i u)))
                              xi))
                     (u (- w 1) (- u 1)))
                  ((< u ,r)
                   xi))
                (vector-ref x w)))))
       (else `(unknown index-type ,index-type for ,type past indexer))))
    (else
     `(unknown type ,type))))

(define (opt:vector-applier r)
  `(lambda (p v)
     (p ,@(map (lambda (k)
                 `(vector-ref v ,k))
               (opt:between 0 r)))))

(define (opt:actor-applier r)
  `(lambda (p a)
     (p ,@(map (lambda (k)
                 `(array-ref a ,k))
               (opt:between 0 r)))))

(define (opt:backing-vector-applier r)
  `(lambda (p ai av)
     (p ,@(map (lambda (k)
                 `(vector-ref av (array:actor-index ai ,k)))
               (opt:between 0 r)))))

(define (opt:past-vector-applier r)
  `(lambda (r)
     (lambda (p v)
       (apply p
              ,@(map (lambda (k)
                       `(vector-ref v ,k))
                     (opt:between 0 r))
              (do ((k r (- k 1))
                   (r '() (cons (vector-ref v (- k 1)) r)))
                ((= k ,r) r))))))

(define (opt:past-actor-applier r)
  `(lambda (r)
     (lambda (p a)
       (apply a
              ,@(map (lambda (k)
                       `(array-ref a ,k))
                     (opt:between 0 r))
              (do ((k r (- k 1))
                   (r '() (cons (array-ref a (- k 1)) r)))
                ((= k ,r) r))))))

(define (opt:past-backing-vector-applier r)
  `(lambda (r)
     (lambda (p ai av)
       (apply p
              ,@(map (lambda (k)
                       `(vector-ref av (array:actor-index ai ,k)))
                     (opt:between 0 r))
              (do ((k r (- k 1))
                   (r '() (cons (vector-ref
                                 av
                                 (array:actor-index ai (- k 1)))
                                r)))
                ((= k ,r) r))))))
