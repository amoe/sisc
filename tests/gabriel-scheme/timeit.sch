;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         timer.sch
; Description:  The timer function for Gabriel's test suite.
; Author:       Robert Kessler, Will Galway and Stan Shebs
; Created:      05-Mar-84
; Modified:     16-Dec-85 (Stan Shebs)
;               4-Aug-87 (Will Clinger)
;               28-Mar-88 (Eric Ost)
; Language:     Chez Scheme
; Status:       Experimental
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; Invoke this function to run a benchmark.  The first argument is a string
;;; identifying the benchmark, while the second is a thunk to be called.
(define-macro! comment () #t)
 
(comment
(define (run-benchmark name thunk)
  (newline)
  (display "--------------------------------------------------------")
  (newline)
  (display name)
  (newline)
  (display "Timing performed on an Apple Macintosh II with 5 Mby RAM")
  (newline)
  (display "running Finder 6.0 System 4.2 at Semantic Microsystems.")
  (newline)
  (gc)
  ; timeit is a macro supplied by MacScheme
  (timeit (thunk)))
)

(define (run-benchmark name thunk)
  (newline)
  (display "--------------------------------------------------------")
  (newline)
  (display name)
  (newline)
  (display "Timing performed on a Vax-8800 with 32MB RAM, Ultrix 2.0")
  (newline)
  (collect)
  ; time is a macro supplied by Chez
  (time (thunk)))
