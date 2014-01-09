(define (srfi-22-prepare environment source-file)
  (case environment
    ((scheme-srfi-0)
     (eval '(import srfi-0)))
    ((scheme-srfi-7)
     (eval '(import srfi-7))))
  (call-with-input-file source-file
    (lambda (in)
      (let loop ((e (read-code in)))
        (unless (eof-object? e)
          (eval e)
          (loop (read-code in)))))))

(define (main-hook . args)
  ((eval 'main (get-symbolic-environment '*toplevel*)) args))

;; Default main, so ordinary "whole bunch 'o scheme code" scripts
;; will work
(define (main args) 0)
    
