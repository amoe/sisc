;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         fread.sch
; Description:  FREAD benchmark
; Author:       Richard Gabriel
; Created:      11-Apr-85
; Modified:     11-Apr-85 20:39:09 (Bob Shaw)
;               24-Jul-87 (Will Clinger)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; FREAD -- Benchmark to read from a file.
;;; Requires the existence of FPRINT.TST which is created
;;; by FPRINT.
 
(define (fread)
  (call-with-input-file
   "fprint.tst"
    (lambda (stream)
      (read stream))))
 
;;; call: (fread))

 
(run-benchmark "Fread" (lambda () (fread)))

 
