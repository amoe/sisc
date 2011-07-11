;; ex 1: simple loop
(define (ex1)
  (let loop ((p (cons 1 2)))
    (let ((p2 (cons 1 2)))
      (set-cdr! p p2)
      (loop p2))))

;; ok | ok | ok | ok | ok


;; ex 2: almost the same but not quite ...
(define (ex2)
  (let ((p (cons 1 2)))
    (let loop ((p p))
      (let ((p2 (cons 1 2)))
        (set-cdr! p p2)
        (loop p2)))))

;; leaks | ok | ok | ok | ok


;; ex 3: closure check

(define (ex3)
  (let ((p (cons 1 2)))
    (let ((fun (lambda (x) x)))
      (let loop ((p p))
        (let ((p2 (cons 1 2)))
          (set-cdr! p p2)
          (fun 42)
          (loop p2))))))

;; leaks | ok | ok | ok | ok


;; ex 4: continuation frame check
(define (ex4)
  (let ((p1 (cons 1 2)))
    (let loop ((p p1))
      (let ((p2 (cons 1 2)))
        (set-cdr! p p2)
        (loop p2)))
    '42))

; SISC Leaks on examples 2-4 in versions 1.8.5 and earlier.  
; In 1.9.0-alpha and up, we leak only on 4.

;; an example using call/cc - this works fine in SISC

(let loop ([x 0]) (call/cc loop))
