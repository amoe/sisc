(define (load-test file)
  (load (string-append "gabriel-scheme/" (symbol->string file) ".sch")))
;;use this for compiled Kawa
;(define (load-test file)
;  (load (string-append (symbol->string file) ".class")))

(load-test 'runbenchmark)
;(load-test 'runbenchmark_kawa)
;(load-test 'runbenchmark_jscheme)
;(load-test 'runbenchmark_mzscheme)

;;the following steps are required for the compiled-Kawa test
;;1) compile all test files, including runbenchmark_kawa using
;;     java -jar kawa.jar -d kawa -C gabriel-scheme/<thefile>
;;   (add the --full-tailcalls flag for the tailcall-mode version,
;;    note however that runbenchmark_kawa does not compile in that
;;    mode and that the non-tc version should be used instead)
;;2) run kawa as follows
;;     java -classpath kawa:kawa.jar kawa.repl

;;the following steps are required for the compiled-JScheme test
;;1) cat prop.scm runbenchmark_jscheme_c.sch tak.sch ... jscheme_final.sch >
;;   jscheme_gabriel.sch
;;2) java -classpath jscheme.jar silk.Compile jscheme_gabriel.sch
;;3) javac -classpath jscheme.jar -O -g:none jscheme_gabriel.java
;;4) java -classpath .:jscheme.jar jscheme_gabriel

(for-each load-test
          '(
            tak
            takl
            takr
            ctak
            deriv   ;Kawa-CTC
            dderiv
            div
            destruct
            fft		;Kawa-(C)TC, JScheme
            puzzle	;Kawa-(C)TC
            cpstack	;Kawa
            fprint	;Kawa-(C)TC
            fread
;            tprint	;Kawa-(C)TC
            ))

;Kawa		- breaks in Kawa non-tailcall mode
;Kawa-TC	- breaks in Kawa tailcall-mode
;Kawa-CTC	- doesn't compile in Kawa tailcall-mode
;JScheme	- breaks in JScheme

(display benchmark-results)
(display (list 'total (apply + (map cadr benchmark-results)) 'ms))
(newline)
(define repl (lambda args (void)))
