(define (program-vicinity)
  (current-directory))

(define (library-vicinity)
  (string-append
   (or (getenv "sisc.slib")
       (getenv "sisc.lib")
       (error "You must define the sisc.slib or sisc.lib property"))
   "/"))

(define (implementation-vicinity)
  (string-append
   (or (getenv "sisc.home")
       (error "You must define the sisc.home property"))
   "/"))

(define (user-vicinity) "")

(define (home-vicinity)
  (string-append
   (or (getenv "user.home")
       (error "You must define the user.home property"))
   "/"))

(define in-vicinity normalize-url)

(define (sub-vicinity vicinity name)
  (string-append vicinity name "/"))

(define (make-vicinity pathname) pathname)

(define (pathname->vicinity pathname)
  (let loop ((i (- (string-length pathname) 1)))
    (cond ((negative? i) "")
      ((vicinity:suffix? (string-ref pathname i))
       (substring pathname 0 (+ i 1)))
      (else (loop (- i 1))))))

(define (vicinity:suffix? chr)
  (eqv? chr #\/))
