; SISC implementation of SRFI 55: require-extension

(define-syntax require-srfi
  (lambda (ctx)
    (syntax-case ctx ()
      [(_ id)
       (let* ([srfi-name-symb
               (string->symbol
                (string-append "srfi-"
                               (number->string
                                (syntax-object->datum
                                 (syntax id)))))]
              [lib-name-symb
               (string->symbol
                (string-append "sisc/libs/srfi/"
                               (symbol->string srfi-name-symb)))])
         (require-library lib-name-symb)
         (with-syntax ([srfi-name
                        (datum->syntax-object
                         (syntax ctx) srfi-name-symb)]
                       [lib-name
                        (datum->syntax-object
                         (syntax ctx) lib-name-symb)])
           (syntax (begin (require-library 'lib-name)
                          (import srfi-name)))))])))

(define-syntax require-lib
  (lambda (ctx)
    (syntax-case ctx ()
      [(_ id)
       (let ([lib-name-symb (syntax-object->datum (syntax id))])
         (require-library lib-name-symb)
         (with-syntax ([lib-name
                        (datum->syntax-object
                         (syntax ctx) lib-name-symb)])
           (syntax (begin (require-library 'lib-name)
                          (import lib-name)))))])))

(define-syntax require-legacy
  (lambda (ctx)
    (define (module-exists? id)
      (with/fc (lambda (m e) #f)
        (lambda ()
          (eval `(let () (import ,id) #t) (interaction-environment))
          #t)))
    (display "{warning: use of deprecated require-extension syntax}\n")
    (syntax-case ctx ()
      [(_ prefix id)
       (let* ([id-symb
               (syntax-object->datum (syntax id))]
              [lib-name-symb
               (string->symbol
                (format "~a/~a"
                        (syntax-object->datum (syntax prefix))
                        id-symb))])
         (require-library lib-name-symb)
         (with-syntax ([lib-name
                        (datum->syntax-object
                         (syntax ctx) lib-name-symb)]
                       [import-name
                        (datum->syntax-object
                         (syntax ctx)
                         (if (module-exists? id-symb)
                             id-symb
                             lib-name-symb))])
           (syntax (begin (require-library 'lib-name)
                          (import import-name)))))])))

(define-syntax require-extension
  (syntax-rules (srfi lib)
    [(_)
     #!void]
    [(_ (_) clause ...)
     (require-extension clause ...)]
    [(_ (srfi id0 id ...) clause ...)
     (begin (require-srfi id0)
            (require-extension (srfi id ...) clause ...))]
    [(_ (lib id0 id ...) clause ...)
     (begin (require-lib id0)
            (require-extension (lib id ...) clause ...))]
    [(_ (prefix id0 id ...) clause ...)
     (begin (require-legacy prefix id0)
            (require-extension (prefix id ...) clause ...))]))
