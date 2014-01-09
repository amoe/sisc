(include "srfis.scm")

(for-each (lambda (name)
            (require-library (string->symbol
                              (string-append "sisc/libs/srfi/"
                                             (symbol->string name)))))
          srfis)
