(define *VOID-MARKER* (list #f))

(define make-hash-table make-hashtable)

(define hash-table? hashtable?)

(define (alist->hash-table alist . rest)
  (apply alist->hashtable (reverse alist) rest))

(define hash-table-equivalence-function hashtable/equivalence-function)

(define hash-table-hash-function hashtable/hash-function)

(define (hash-table-ref ht key . rest)
  (let ([default-thunk
         (if (null? rest)
             (lambda () (error 'hash-table-ref
                               (string-append
                                "hash-table ~a does not contain key ~a "
                                "and no default thunk was specified")
                               ht key))
             (if (null? (cdr rest))
                 (let ([p (car rest)])
                   (if (procedure? p)
                       p
                       (error 'hash-table-ref "~a is not a procedure"
                              p)))
                 (error 'hash-table-ref
                        "invalid number of args: expected 2-3, got ~a"
                        (apply list ht key rest))))]
        [res (hashtable/get ht key *VOID-MARKER*)])
    (if (eq? res *VOID-MARKER*)
        (default-thunk)
        res)))

(define (hash-table-ref/default ht key def)
  (hashtable/get ht key def))

(define (hash-table-set! ht key val)
  (hashtable/put! ht key val))

(define (hash-table-delete! ht key)
  (hashtable/remove! ht key))

(define hash-table-exists? hashtable/contains?)

(define (hash-table-update! ht key fn . rest)
  (define (helper)
    (hash-table-set! ht key (fn (apply hash-table-ref ht key rest))))
  (if (hashtable/thread-safe? ht)
      (synchronized ht helper)
      (helper)))

(define (hash-table-update!/default ht key fn def)
  (define (helper)
    (hash-table-set! ht key (fn (hash-table-ref/default ht key def))))
  (if (hashtable/thread-safe? ht)
      (synchronized ht helper)
      (helper)))

(define hash-table-size hashtable/size)

(define hash-table-keys hashtable/keys)

(define (hash-table-values ht)
  (map cdr (hashtable->alist ht)))

(define (hash-table-walk ht proc)
  (hashtable/for-each proc ht))

(define (hash-table-fold ht proc acc)
  (let loop ([l (hashtable->alist ht)]
             [acc acc])
    (if (null? l)
        acc
        (loop (cdr l) (let ([k/v (car l)])
                        (proc (car k/v) (cdr k/v) acc))))))

(define hash-table->alist hashtable->alist)

(define (hash-table-copy ht)
  (alist->hashtable (hashtable->alist ht)
                    (hashtable/equivalence-function ht)
                    (hashtable/hash-function ht)
                    (hashtable/thread-safe? ht)
                    (hashtable/weak? ht)))

(define (hash-table-merge! ht1 ht2)
  (hashtable/add-alist! ht1 (hashtable->alist ht2)))

(define hash hash-by-equal)

(define string-hash hash-by-string=)

(define string-ci-hash hash-by-string-ci=)

(define hash-by-identity hash-by-eq)
