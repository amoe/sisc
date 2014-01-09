(define hash-table-thread-safe? hashtable/thread-safe?)

(define hash-table-weak? hashtable/weak?)

(define (hash-table-ref! ht key thunk)
  (hashtable/get! ht key thunk))

(define (hash-table-ref!/default ht key def)
  (hashtable/get! ht key (lambda () def) #f))
