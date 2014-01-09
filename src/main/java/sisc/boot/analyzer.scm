;; analyzer.scm - Scans and transforms input primitive
;; s-expressions (output from the expander) into s-expressions
;; which are annotated with metadata required by the compiler.
;
;;
;; (lambda formals body) => (lambda #t formals (lexicals ...) body)
;; (letrec bindings body) => (letrec #t bindings (lexicals ...) body)
;;

(define _analyze!
  (let ()
    (import misc)
    (define PROGRAM 0)
    (define APPLICATION 1)
    (define _LAMBDA 2)
    (define _IF 3)
    (define _BEGIN 4)
    (define _QUOTE 5)
    (define SET 6)
    (define _DEFINE 7)
    (define MAKEANNOTATION 8)
    (define _LETREC 9)
    (define UNKNOWN -1)

    (define (union ls1 ls2)
      (let loop ([ls1 ls1] [tail #f]
                 [acc '()])
        (cond [(null? ls1)
               (if tail
                   (begin
                     (set-cdr! tail ls2)
                     acc)
                   ls2)]
              [(memq (car ls1) ls2)
               (loop (cdr ls1) tail acc)]
              [else (let ([cc (cons (car ls1) acc)])
                      (loop (cdr ls1)
                            (or tail cc) cc))])))
    (define (difference ls1 ls2)
      (let loop ([ls1 ls1] [acc '()])
        (cond [(null? ls1) acc]
              [(memq (car ls1) ls2)
               (loop (cdr ls1) acc)]
              [else (loop (cdr ls1) (cons (car ls1) acc))])))
    (define (intersection ls1 ls2)
      (let loop ([ls1 ls1] [acc '()])
        (cond [(null? ls1) acc]
              [(memq (car ls1) ls2)
               (loop (cdr ls1) (cons (car ls1) acc))]
              [else (loop (cdr ls1) acc)])))
    (define (in ls1 ls2) (reverse (intersection ls1 ls2)))
    (define-simple-syntax (arecord-refs arec)
      (vector-ref arec 0))
    (define-simple-syntax (arecord-sets arec)
      (vector-ref arec 1))
    (define-simple-syntax (arecord-frees arec)
      (vector-ref arec 2))
    (define-simple-syntax (arecord-value arec)
      (vector-ref arec 3))

    (define (inc-cdr! v) (set-cdr! v (+ 1 (cdr v))))
    (define-simple-syntax (inc! vcount var)
      (cond [(assq var (cdr vcount)) => inc-cdr!]
            [else (set-cdr! vcount
                            (cons (cons var 1) (cdr vcount)))]))
    (define-simple-syntax (inc-refs! arec var)
      (inc! (arecord-refs arec) var))
    (define-simple-syntax (inc-sets! arec var)
      (inc! (arecord-sets arec) var))
    (define (arecord-add-free! arec newfree)
      (let ([afrees (arecord-frees arec)])
        (unless (memq newfree (cdr afrees))
          (set-cdr! afrees (cons newfree (cdr afrees))))))
    (define (arecord-union-frees! arec newfrees)
      (set-cdr! (arecord-frees arec)
                (union (cdr (arecord-frees arec))
                       newfrees)))
    (define-syntax int-case
      (syntax-rules (else)
        ((_ val (else expr ...))
         (begin expr ...))
        ((_ val (const expr ...) tail ...)
         (if (= val const)
             (begin expr ...)
             (int-case val tail ...)))))
    
    (define (_cdr>0 v) (if (> (cdr v) 0) (cdr v) #f))
    (define (non-zero? vcount var)
      (cond [(assq var (cdr vcount)) => _cdr>0]
            [else #f]))
    (define (->proper-list elem)
      (cond [(null? elem) '()]
            [(pair? elem)
             (cons (car elem) (->proper-list (cdr elem)))]
            [else (list elem)]))
    (define (make-arecord v)
      (vector (list '*refs*) (list '*sets*) (list '*frees*) v))

    (define (build-refs vals)
      (cond [(null? vals) '()]
            [(zero? (cdar vals))
             (build-refs (cdr vals))]
            [else (cons (car vals)
                        (build-refs (cdr vals)))]))
    (define analyze! 
      (lambda (v arec env lxs)
        (cond [(symbol? v)
               (inc-refs! arec v)
               (unless (memq v lxs)
                 (arecord-add-free! arec v))
               (list v)]
              [(pair? v)
               (analyze-app! v arec env lxs)]
              [else '()])))
    (define analyze-app! 
      (lambda (v arec env lxs)
        (let ([oper (car v)]
              [v (cdr v)]
              [exp-type (_expression-type env (car v))])
          (int-case exp-type
            (_QUOTE '())
            (PROGRAM
             (map-car (car v)))
            (_LAMBDA
             (let* ([analyzed (eq? (car v) #t)]
                    [v (if analyzed (cdr v) v)]
                    [locals (->proper-list (car v))]
                    [v (if analyzed (cdr v) v)]
                    [refs (analyze! (cadr v) arec env
                                    (union locals lxs))]
                    [locally-frees (difference refs locals)]
                    [frees (difference locally-frees lxs)]
                    [lexicals (in lxs refs)])
              (arecord-union-frees!
               arec (difference locally-frees lxs))
              (unless analyzed
                (set-cdr! v (cons (car v) (cons lexicals (cdr v))))
                (set-car! v #t))
              locally-frees))
          (_LETREC
            (let* ([analyzed (eq? (car v) #t)]
                   [v (if analyzed (cdr v) v)]
                   [letrec-remainder v]
                   [locals (map (lambda (binding)
                                  (inc-sets! arec (car binding))
                                  (car binding))
                                (car v))]
                   [newlxs (union locals lxs)]
                   [refs (let loop ([bindings (car v)]
                                    [refs '()])
                           (if (null? bindings) refs
                               (loop
                                (cdr bindings)
                                (union (analyze!
                                        (cadar bindings) arec
                                        env newlxs)
                                       refs))))]
                   [v ((if analyzed cddr cdr) v)]
                   [refs (union (analyze! (car v) arec env newlxs) refs)]
                   [locally-frees (difference refs locals)]
                   [lexicals (in lxs refs)])
              (arecord-union-frees!
               arec (difference locally-frees lxs))          
              (unless analyzed
                (set-cdr! letrec-remainder
                          (cons (car letrec-remainder)
                                (cons lexicals
                                      (cdr letrec-remainder))))
                (set-car! letrec-remainder #t))
              locally-frees))
           (SET
            (let ([s (car v)])
              (when (symbol? s)
                (inc-refs! arec s)
                (inc-sets! arec s))
              (cons s (analyze! (cadr v) arec env lxs))))
           (_DEFINE (analyze! (cadr v) arec env lxs))
           (MAKEANNOTATION (analyze! (car v) arec env lxs))
           (else
            (let loop ([v v]
                       [rv (if (= exp-type APPLICATION)
                               (analyze! oper arec env lxs)
                               '())])
              (if (or (null? v) (not (pair? v)))
                  rv
                  (loop (cdr v)
                        (union (analyze! (car v) arec env lxs)
                               rv)))))))))
    (lambda (v env)
      (let ([arec (make-arecord v)])
        (analyze! v arec env '())
        `(#%program ,(build-refs (cdr (arecord-refs arec)))
                  ,(build-refs (cdr (arecord-sets arec)))
                  ,(cdr (arecord-frees arec))
                  ,(arecord-value arec))))))
        
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

               
            
          
        
