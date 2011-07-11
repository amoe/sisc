
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(define (make-queue) (vector '() '() 0))

(define (front-ptr queue) (vector-ref queue 0))

(define (rear-ptr queue) (vector-ref queue 1))

(define (queue-length queue) (vector-ref queue 2))

(define (set-front-ptr! queue item) (vector-set! queue 0 item))

(define (set-rear-ptr! queue item) (vector-set! queue 1 item))

(define (set-queue-length! queue n) (vector-set! queue 2 n))

(define (inc-queue-length! queue)
   (set-queue-length! queue (+ (queue-length queue) 1)))

(define (dec-queue-length! queue)
   (set-queue-length! queue (- (queue-length queue) 1)))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
   (if (empty-queue? queue)
       (error "FRONT called with an empty queue" queue 1)
       (car (front-ptr queue))))

(define (insert-queue! queue item)
   (let ((new-pair (cons item '())))
      (cond ((empty-queue? queue)
             (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             (set-queue-length! queue 1)
             queue)
            (else
             (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             (inc-queue-length! queue)
             queue))))

(define (delete-queue! queue)
   (cond ((empty-queue? queue)
          (error "DELETE! called with an empty queue" queue 1))
         (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          (dec-queue-length! queue)
          (if (empty-queue? queue)
              (set-rear-ptr! queue '())
              )
          queue)))

(define (list-last l)
   (if (null? (cdr l)) (car l)
       (list-last (cdr l))))

(define (cc3 amount coin-values)
   (let ((fifos (map make-ccfifo coin-values)))
      (map (lambda (f) (cc-push! f 1)) fifos)
      (let loop ((n 1))
         (cc-push! (car fifos) (cc-front (car fifos)))
         (let row-loop ((last (car fifos)) (rest (cdr fifos)))
            (if (not (null? rest))
                (begin
                   (cc-push! (car rest) (+ (cc-last last)
                                           (cc-front (car rest))))
                   (row-loop (car rest) (cdr rest)))))
         
         (if (= n amount) (cc-last (list-last fifos))
             (loop (+ n 1))))))


(define (make-ccfifo len)
   (cons len (make-queue)))

(define (ccfifo-len fifo)
   (car fifo))

(define (ccfifo-queue fifo)
   (cdr fifo))

(define (cc-front fifo)
   (if (< (queue-length (ccfifo-queue fifo))
          (ccfifo-len fifo))
       0
       (front-queue (ccfifo-queue fifo))))

(define (cc-last fifo)
   (car (rear-ptr (ccfifo-queue fifo))))

;; push item into the fifo, if the fifo is full then delete
;; the oldest item
(define (cc-push! fifo item)
   (insert-queue! (ccfifo-queue fifo) item)
   (if (> (queue-length (ccfifo-queue fifo))
          (ccfifo-len fifo))
       (delete-queue! (ccfifo-queue fifo))))

(define (test-coins)
  (cc3 100000 us-coins))
