; SISC implementation of SRFI-18
;
; Compliance issues:
; thread-terminate!,
; relies on interrupts to be enabled (using the sisc.interrupts 
; system property)
;

; Time objects
(define (time->seconds to) (time-second to))
(define (seconds->time seconds) (make-time time-utc 0 seconds))

(define (time->ms to)
  (cond [(number? to) (inexact->exact (* 1000 to))]
        [(time? to) (+ (* (time-second to) 1000)
                       (quotient (time-nanosecond to) 1000))]
        [else (error 'time->ms "Unsupported time value ~a." to)]))

;Threading
(define current-thread thread/current)

(define (make-thread thunk . name)
  (let ([thread (thread/new (lambda ()
                              (with/fc
                               (lambda (m e)
                                 (abandon-mutexes (current-thread))
                                 (throw m e))
                               (lambda () 
                                 (let ([rv (thunk)])
                                   (abandon-mutexes (current-thread))
                                   rv)))))])
    (unless (null? name)
      (set-annotation! thread 'name (car name)))
    thread))

(define (thread-name thread)
  (annotation thread 'name))

(define (*-specific obj)
  (annotation obj 'specific))
(define (*-specific-set! obj val)
  (set-annotation! obj 'specific val))

(define (named-constructor real-c init-thunk)
  (lambda args
    (let ([rv (real-c)])
      (unless (null? args)
        (set-annotation! rv 'name (car args)))
      (init-thunk rv)
      rv)))
(define (*-name obj)
  (annotation obj 'name))

(define thread-specific *-specific)
(define thread-specific-set! *-specific-set!)

(define (thread-start! thread)
  (thread/start thread)
  thread)

(define thread-yield! thread/yield)
(define (thread-sleep! t)
  (sleep (time->ms t)))

(define (thread-terminate! thread)
  (thread/interrupt thread)
  (thread/join thread)
  (thread/_set-result! thread 
                       (with/fc values (lambda () 
                                         (raise 'thread-terminated thread)))))

(define (thread-join! thread . args)
  (let* ([timeout (let ([v (and (not (null? args))
                                (car args))])
                    (and v (time->ms v)))])
    (let loop ()
      (when (eq? (thread/state thread) 'ready)
        (sleep 200)
        (loop)))
    (if timeout
        (if (not (thread/join thread timeout))
            (if (null? (cdr args))
                (raise 'join-timeout timeout)
                (cadr args))
            (thread/result thread))
        (begin
          (thread/join thread)
          (thread/result thread)))))

;; Mutexes

(define (abandon-mutexes thread)
  (for-each (lambda (mutex)
              (mutex-unlock! mutex)
              (set-annotation! mutex 'state 'abandoned))
            (annotation thread 'mutexes '())))
              
(define make-mutex (named-constructor mutex/new 
                                      (lambda (v)
                                        (set-annotation! v 'state
                                                         'not-abandoned))))

(define mutex-name *-name)
(define mutex-specific *-specific)
(define mutex-specific-set! *-specific-set!)

(define (mutex-state mutex)
  (annotation mutex 'state))

(define mutex-lock!
  (let ([finish-lock
         (lambda (mutex thread)
           (let ([oldstate (annotation mutex 'state)]
                 [threads-mutexes (annotation thread 'mutexes '())])
             (if thread
                 (begin
                   (set-annotation! mutex 'owner thread)
                   (set-annotation! mutex 'state thread)
                   (unless (memq mutex threads-mutexes)
                     (set-annotation! thread 'mutexes 
                                      (cons mutex threads-mutexes))))
                 (set-annotation! mutex 'state 'not-owned))
             (if (eq? oldstate 'abandoned)
                 (raise 'abandoned-mutex mutex)))
           #t)])
    (lambda (mutex . args)
      (let ([timeout (if (or (null? args) (not (car args))) #f
                         (time->ms (car args)))]
            [thread (if (or (null? args) (null? (cdr args)))
                        (current-thread)
                        (cadr args))])
        (if timeout
            (and (mutex/lock! mutex timeout)
                 (finish-lock mutex thread))
            (begin
              (mutex/lock! mutex)
              (finish-lock mutex thread)))))))

(define mutex-unlock!
  (let ([finish-unlock
         (lambda (mutex owner)
           (set-annotation!
            owner 'mutexes
            (delete! mutex (annotation owner 'mutexes '()) eq?)))])
    (lambda (mutex . args)
      (let ([condvar (if (null? args) #f (car args))])
        (let ([timeout (and condvar (and (not (null? (cdr args)))
                                         (cadr args)
                                         (time->ms (cadr args))))]
              [owner (annotation mutex 'owner)]
              [old-state (annotation mutex 'state)])
          (set-annotation! mutex 'state
                           (if (or owner (eq? old-state 'not-owned))
                               'not-abandoned
                               'abandoned))
          (if condvar
              (if timeout
                  (if (mutex/unlock! mutex condvar timeout)
                      (begin (finish-unlock mutex owner) #t)
                      (begin
                        (set-annotation! mutex 'state old-state)
                        #f))
                  (begin (mutex/unlock! mutex condvar)
                         (finish-unlock mutex owner)
                         #t))
              (begin
                (mutex/unlock! mutex)
                (finish-unlock mutex owner)
                #t)))))))
                

; Condition Variables

(define condition-variable? condvar?)

(define make-condition-variable (named-constructor condvar/new 
                                                   (lambda (v) (void))))
(define condition-variable-name *-name)
(define condition-variable-specific *-specific)
(define condition-variable-specific-set! *-specific-set!)

(define condition-variable-signal! condvar/notify)
(define condition-variable-broadcast! condvar/notify-all)

; Map SRFI-18's exception handling to SISC's failure continuations
(define (current-exception-handler)
  (call/fc (lambda (f) f)))

(define (with-exception-handler handler thunk)
  (with/fc 
   (lambda (m e)
     (handler m))
   thunk))

(define (raise obj . val)
  (error (cons obj val)))

; Define some exceptions specific to this SRFI
(define-nongenerative-record-type srfi18-exception
  sisc.srfi.srfi-18.exception-type
  (make-srfi18-exception type val)
  srfi18-exception?
  (type srfi18-exception-type)
  (val srfi18-exception-val))

;Threading
(define (*-mutex-exception? type)
  (lambda (obj)
    (let ([v (error-message obj)])
      (and v (pair? v) (eq? (car v) type)))))

(define join-timeout-exception? (*-mutex-exception? 'join-timeout))
(define abandoned-mutex-exception? (*-mutex-exception? 'abandoned-mutex))
(define terminated-thread-exception? (*-mutex-exception? 'thread-terminated))
(define uncaught-exception? (*-mutex-exception? 'uncaught-exception))
(define (uncaught-exception-reason exc)
  (srfi18-exception-val exc))
