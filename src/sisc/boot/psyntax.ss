;;; Portable implementation of syntax-case
;;; Extracted from Chez Scheme Version 6.9 (Jul 12, 2002)
;;; Authors: R. Kent Dybvig, Oscar Waddell, Bob Hieb, Carl Bruggeman

;;; Copyright (c) 1992-2002 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;;; Before attempting to port this code to a new implementation of
;;; Scheme, please read the notes below carefully.

;;; This file defines the syntax-case expander, sc-expand, and a set
;;; of associated syntactic forms and procedures.  Of these, the
;;; following are documented in The Scheme Programming Language,
;;; Second Edition (R. Kent Dybvig, Prentice Hall, 1996), which can be
;;; found online at http://www.scheme.com.  Most are also documented
;;; in the R4RS and draft R5RS.
;;;
;;;   bound-identifier=?
;;;   datum->syntax-object
;;;   define-syntax
;;;   fluid-let-syntax
;;;   free-identifier=?
;;;   generate-temporaries
;;;   identifier?
;;;   identifier-syntax
;;;   let-syntax
;;;   letrec-syntax
;;;   syntax
;;;   syntax-case
;;;   syntax-object->datum
;;;   syntax-rules
;;;   with-syntax
;;;
;;; All standard Scheme syntactic forms are supported by the expander
;;; or syntactic abstractions defined in this file.  Only the R4RS
;;; delay is omitted, since its expansion is implementation-dependent.

;;; Also defined are three forms that support modules: module, import,
;;; and import-only.  These are documented in the Chez Scheme User's
;;; Guide (R. Kent Dybvig, Cadence Research Systems, 1998), which can
;;; also be found online at http://www.scheme.com.  They are described
;;; briefly here as well.
;;;
;;; Both are definitions and may appear where and only where other
;;; definitions may appear.  modules may be named:
;;;
;;;   (module id (ex ...) defn ... init ...)
;;;
;;; or anonymous:
;;;
;;;   (module (ex ...) defn ... init ...)
;;;
;;; The latter form is semantically equivalent to:
;;;
;;;   (module T (ex ...) defn ... init ...)
;;;   (import T)
;;;
;;; where T is a fresh identifier.
;;;
;;; In either form, each of the exports in (ex ...) is either an
;;; identifier or of the form (id ex ...).  In the former case, the
;;; single identifier ex is exported.  In the latter, the identifier
;;; id is exported and the exports ex ... are "implicitly" exported.
;;; This listing of implicit exports is useful only when id is a
;;; keyword bound to a transformer that expands into references to
;;; the listed implicit exports.  In the present implementation,
;;; listing of implicit exports is necessary only for top-level
;;; modules and allows the implementation to avoid placing all
;;; identifiers into the top-level environment where subsequent passes
;;; of the compiler will be unable to deal effectively with them.
;;;
;;; Named modules may be referenced in import statements, which
;;; always take one of the forms:
;;;
;;;   (import id)
;;;   (import-only id)
;;;
;;; id must name a module.  Each exported identifier becomes visible
;;; within the scope of the import form.  In the case of import-only,
;;; all other identifiers become invisible in the scope of the
;;; import-only form, except for those established by definitions
;;; that appear textually after the import-only form.

;;; The remaining exports are listed below.  sc-expand, eval-when, and
;;; syntax-error are described in the Chez Scheme User's Guide.
;;;
;;;   (sc-expand datum)
;;;      if datum represents a valid expression, sc-expand returns an
;;;      expanded version of datum in a core language that includes no
;;;      syntactic abstractions.  The core language includes begin,
;;;      define, if, lambda, letrec, quote, and set!.
;;;   (eval-when situations expr ...)
;;;      conditionally evaluates expr ... at compile-time or run-time
;;;      depending upon situations
;;;   (syntax-error object message)
;;;      used to report errors found during expansion
;;;   ($syntax-dispatch e p)
;;;      used by expanded code to handle syntax-case matching
;;;   ($sc-put-cte symbol val)
;;;      used to establish top-level compile-time (expand-time) bindings.

;;; The following nonstandard procedures must be provided by the
;;; implementation for this code to run.
;;;
;;; (void)
;;; returns the implementation's cannonical "unspecified value".  The
;;; following usually works:
;;;
;;; (define void (lambda () (if #f #f))).
;;;
;;; (andmap proc list1 list2 ...)
;;; returns true if proc returns true when applied to each element of list1
;;; along with the corresponding elements of list2 ....  The following
;;; definition works but does no error checking:
;;;
;;; (define andmap
;;;   (lambda (f first . rest)
;;;     (or (null? first)
;;;         (if (null? rest)
;;;             (let andmap ((first first))
;;;               (let ((x (car first)) (first (cdr first)))
;;;                 (if (null? first)
;;;                     (f x)
;;;                     (and (f x) (andmap first)))))
;;;             (let andmap ((first first) (rest rest))
;;;               (let ((x (car first))
;;;                     (xr (map car rest))
;;;                     (first (cdr first))
;;;                     (rest (map cdr rest)))
;;;                 (if (null? first)
;;;                     (apply f (cons x xr))
;;;                     (and (apply f (cons x xr)) (andmap first rest)))))))))
;;;
;;; (ormap proc list1)
;;; returns the first non-false return result of proc applied to
;;; the elements of list1 or false if none.  The following definition
;;; works but does no error checking:
;;;
;;; (define ormap
;;;   (lambda (proc list1)
;;;     (and (not (null? list1))
;;;          (or (proc (car list1)) (ormap proc (cdr list1))))))
;;;
;;; The following nonstandard procedures must also be provided by the
;;; implementation for this code to run using the standard portable
;;; hooks and output constructors.  They are not used by expanded code,
;;; and so need be present only at expansion time.
;;;
;;; (eval x)
;;; where x is always in the form ("noexpand" expr).
;;; returns the value of expr.  the "noexpand" flag is used to tell the
;;; evaluator/expander that no expansion is necessary, since expr has
;;; already been fully expanded to core forms.
;;;
;;; eval will not be invoked during the loading of psyntax.pp.  After
;;; psyntax.pp has been loaded, the expansion of any macro definition,
;;; whether local or global, results in a call to eval.  If, however,
;;; sc-expand has already been registered as the expander to be used
;;; by eval, and eval accepts one argument, nothing special must be done
;;; to support the "noexpand" flag, since it is handled by sc-expand.
;;;
;;; (error who format-string why what)
;;; where who is either a symbol or #f, format-string is always "~a ~s",
;;; why is always a string, and what may be any object.  error should
;;; signal an error with a message something like
;;;
;;;    "error in <who>: <why> <what>"
;;;
;;; (gensym)
;;; returns a unique symbol each time it's called.  In Chez Scheme, gensym
;;; returns a symbol with a "globally" unique name so that gensyms that
;;; end up in the object code of separately compiled files cannot conflict.
;;; This is necessary only if you intend to support compiled files.
;;;
;;; (putprop symbol key value)
;;; (getprop symbol key)
;;; (remprop symbol key)
;;; key is always a symbol; value may be any object.  putprop should
;;; associate the given value with the given symbol and key in some way
;;; that it can be retrieved later with getprop.  getprop should return
;;; #f if no value is associated with the given symbol and key.  remprop
;;; should remove the association between the given symbol and key.

;;; When porting to a new Scheme implementation, you should define the
;;; procedures listed above, load the expanded version of psyntax.ss
;;; (psyntax.pp, which should be available whereever you found
;;; psyntax.ss), and register sc-expand as the current expander (how
;;; you do this depends upon your implementation of Scheme).  You may
;;; change the hooks and constructors defined toward the beginning of
;;; the code below, but to avoid bootstrapping problems, do so only
;;; after you have a working version of the expander.

;;; Chez Scheme allows the syntactic form (syntax <template>) to be
;;; abbreviated to #'<template>, just as (quote <datum>) may be
;;; abbreviated to '<datum>.  The #' syntax makes programs written
;;; using syntax-case shorter and more readable and draws out the
;;; intuitive connection between syntax and quote.  If you have access
;;; to the source code of your Scheme system's reader, you might want
;;; to implement this extension.

;;; If you find that this code loads or runs slowly, consider
;;; switching to faster hardware or a faster implementation of
;;; Scheme.  In Chez Scheme on a 200Mhz Pentium Pro, expanding,
;;; compiling (with full optimization), and loading this file takes
;;; between one and two seconds.

;;; In the expander implementation, we sometimes use syntactic abstractions
;;; when procedural abstractions would suffice.  For example, we define
;;; top-wrap and top-marked? as
;;;   (define-syntax top-wrap (identifier-syntax '((top))))
;;;   (define-syntax top-marked?
;;;     (syntax-rules ()
;;;       ((_ w) (memq 'top (wrap-marks w)))))
;;; rather than
;;;   (define top-wrap '((top)))
;;;   (define top-marked?
;;;     (lambda (w) (memq 'top (wrap-marks w))))
;;; On ther other hand, we don't do this consistently; we define make-wrap,
;;; wrap-marks, and wrap-subst simply as
;;;   (define make-wrap cons)
;;;   (define wrap-marks car)
;;;   (define wrap-subst cdr)
;;; In Chez Scheme, the syntactic and procedural forms of these
;;; abstractions are equivalent, since the optimizer consistently
;;; integrates constants and small procedures.  Some Scheme
;;; implementations, however, may benefit from more consistent use
;;; of one form or the other.


;;; Implementation notes:

;;; "begin" is treated as a splicing construct at top level and at
;;; the beginning of bodies.  Any sequence of expressions that would
;;; be allowed where the "begin" occurs is allowed.

;;; "let-syntax" and "letrec-syntax" are also treated as splicing
;;; constructs, in violation of the R5RS.  A consequence is that let-syntax
;;; and letrec-syntax do not create local contours, as do let and letrec.
;;; Although the functionality is greater as it is presently implemented,
;;; we will probably change it to conform to the R5RS.  modules provide
;;; similar functionality to nonsplicing letrec-syntax when the latter is
;;; used as a definition.

;;; Objects with no standard print syntax, including objects containing
;;; cycles and syntax objects, are allowed in quoted data as long as they
;;; are contained within a syntax form or produced by datum->syntax-object.
;;; Such objects are never copied.

;;; When the expander encounters a reference to an identifier that has
;;; no global or lexical binding, it treats it as a global-variable
;;; reference.  This allows one to write mutually recursive top-level
;;; definitions, e.g.:
;;;
;;;   (define f (lambda (x) (g x)))
;;;   (define g (lambda (x) (f x)))
;;;
;;; but may not always yield the intended when the variable in question
;;; is later defined as a keyword.

;;; Top-level variable definitions of syntax keywords are permitted.
;;; In order to make this work, top-level define not only produces a
;;; top-level definition in the core language, but also modifies the
;;; compile-time environment (using $sc-put-cte) to record the fact
;;; that the identifier is a variable.

;;; Top-level definitions of macro-introduced identifiers are visible
;;; only in code produced by the macro.  That is, a binding for a
;;; hidden (generated) identifier is created instead, and subsequent
;;; references within the macro output are renamed accordingly.  For
;;; example:
;;;
;;; (define-syntax a
;;;   (syntax-rules ()
;;;     ((_ var exp)
;;;      (begin
;;;        (define secret exp)
;;;        (define var
;;;          (lambda ()
;;;            (set! secret (+ secret 17))
;;;            secret))))))
;;; (a x 0)
;;; (x) => 17
;;; (x) => 34
;;; secret => Error: variable secret is not bound
;;;
;;; The definition above would fail if the definition for secret
;;; were placed after the definition for var, since the expander would
;;; encounter the references to secret before the definition that
;;; establishes the compile-time map from the identifier secret to
;;; the generated identifier.

;;; Identifiers and syntax objects are implemented as vectors for
;;; portability.  As a result, it is possible to "forge" syntax
;;; objects.

;;; The input to sc-expand may contain "annotations" describing, e.g., the
;;; source file and character position from where each object was read if
;;; it was read from a file.  These annotations are handled properly by
;;; sc-expand only if the annotation? hook (see hooks below) is implemented
;;; properly and the operators make-annotation, annotation-expression,
;;; annotation-source, annotation-stripped, and set-annotation-stripped!
;;; are supplied.  If annotations are supplied, the proper annotation
;;; source is passed to the various output constructors, allowing
;;; implementations to accurately correlate source and expanded code.
;;; Contact one of the authors for details if you wish to make use of
;;; this feature.

;;; Implementation of modules:
;;;
;;; The implementation of modules requires that implicit top-level exports
;;; be listed with the exported macro at some level where both are visible,
;;; e.g.,
;;;
;;;   (module M (alpha (beta b))
;;;     (module ((alpha a) b)
;;;       (define-syntax alpha (identifier-syntax a))
;;;       (define a 'a)
;;;       (define b 'b))
;;;     (define-syntax beta (identifier-syntax b)))
;;;
;;; Listing of implicit imports is not needed for macros that do not make
;;; it out to top level, including all macros that are local to a "body".
;;; (They may be listed in this case, however.)  We need this information
;;; for top-level modules since a top-level module expands into a letrec
;;; for non-top-level variables and top-level definitions (assignments) for
;;; top-level variables.  Because of the general nature of macro
;;; transformers, we cannot determine the set of implicit exports from the
;;; transformer code, so without the user's help, we'd have to put all
;;; variables at top level.
;;;
;;; Each such top-level identifier is given a generated name (gensym).
;;; When a top-level module is imported at top level, a compile-time
;;; alias is established from the top-level name to the generated name.
;;; The expander follows these aliases transparently.  When any module is
;;; imported anywhere other than at top level, the id-var-name of the
;;; import identifier is set to the id-var-name of the export identifier.
;;; Since we can't determine the actual labels for identifiers defined in
;;; top-level modules until we determine which are placed in the letrec
;;; and which make it to top level, we give each an "indirect" label---a
;;; pair whose car will eventually contain the actual label.  Import does
;;; not follow the indirect, but id-var-name does.
;;;
;;; All identifiers defined within a local module are folded into the
;;; letrec created for the enclosing body.  Visibility is controlled in
;;; this case and for nested top-level modules by introducing a new wrap
;;; for each module.


;;; Bootstrapping:

;;; When changing syntax-object representations, it is necessary to support
;;; both old and new syntax-object representations in id-var-name.  It
;;; should be sufficient to redefine syntax-object-expression to work for
;;; both old and new representations and syntax-object-wrap to return the
;;; empty-wrap for old representations.


(let ()
(define-syntax self-evaluating?
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (not (or (pair? x) (vector? x) (and (null? x)
                                           (strict-r5rs-compliance))))))))
(define-syntax when
  (syntax-rules ()
    ((_ test e1 e2 ...) (if test (begin e1 e2 ...)))))
(define-syntax unless
  (syntax-rules ()
    ((_ test e1 e2 ...) (if test #!void (begin e1 e2 ...)))))
(define-syntax define-structure
  (lambda (x)
    (define construct-name
      (lambda (template-identifier . args)
        (datum->syntax-object
          template-identifier
          (string->symbol
            (apply string-append
                   (map (lambda (x)
                          (if (string? x)
                              x
                              (symbol->string (syntax-object->datum x))))
                        args))))))
    (syntax-case x ()
      ((_ (name id1 ...))
       (andmap identifier? (syntax (name id1 ...)))
       (with-syntax
         ((constructor (construct-name (syntax name) "make-" (syntax name)))
          (predicate (construct-name (syntax name) (syntax name) "?"))
          ((access ...)
           (map (lambda (x) (construct-name x (syntax name) "-" x))
                (syntax (id1 ...))))
          ((assign ...)
           (map (lambda (x)
                  (construct-name x "set-" (syntax name) "-" x "!"))
                (syntax (id1 ...))))
          (structure-length
           (+ (length (syntax (id1 ...))) 1))
          ((index ...)
           (let f ((i 1) (ids (syntax (id1 ...))))
              (if (null? ids)
                  '()
                  (cons i (f (+ i 1) (cdr ids)))))))
         (syntax (begin
                   (define constructor
                     (lambda (id1 ...)
                       (vector 'name id1 ... )))
                   (define predicate
                     (lambda (x)
                       (and (vector? x)
                            (= (vector-length x) structure-length)
                            (eq? (vector-ref x 0) 'name))))
                   (define access
                     (lambda (x)
                       (vector-ref x index)))
                   ...
                   (define assign
                     (lambda (x update)
                       (vector-set! x index update)))
                   ...)))))))
(define-syntax let-values ; impoverished one-clause version
  (syntax-rules ()
    ((_ ((formals expr)) form1 form2 ...)
     (call-with-values (lambda () expr) (lambda formals form1 form2 ...)))))

(define noexpand "analyzeonly")

;;; hooks to nonportable run-time helpers
(begin
(define-syntax fx+ (identifier-syntax +))
(define-syntax fx- (identifier-syntax -))
(define-syntax fx= (identifier-syntax =))
(define-syntax fx< (identifier-syntax <))
(define-syntax fx> (identifier-syntax >))
(define-syntax fx<= (identifier-syntax <=))
(define-syntax fx>= (identifier-syntax >=))

(define top-level-eval-hook
  (lambda (x)
    (eval `(,noexpand ,x))))

(define local-eval-hook
  (lambda (x)
    (eval `(,noexpand ,x))))

(define error-hook
  (lambda (who why what)
    (error who "~a ~s" why what)))

(define-syntax gensym-hook
  (syntax-rules ()
    ((_) (gensym))))

(define put-global-definition-hook
  (lambda (symbol val)
    ($sc-put-cte symbol val)))

(define get-global-definition-hook
  (lambda (symbol)
    (getprop symbol '*sc-expander*)))

(define get-import-binding
  (lambda (symbol token)
    (getprop symbol token)))

;;; generate-id ideally produces globally unique symbols, i.e., symbols
;;; unique across system runs, to support separate compilation/expansion.
;;; Use gensym-hook if you do not need to support separate compilation/
;;; expansion or if your system's gensym creates globally unique
;;; symbols (as in Chez Scheme).  Otherwise, use the following code
;;; as a starting point.  session-key should be a unique string for each
;;; system run to support separate compilation; the default value given
;;; is satisfactory during initial development only.

(define generate-id gen-sym)
#|  (let ([vmem '()]
        [anon 0])
    (lambda var
      (cond [(null? var)
             (set! anon (+ anon 1))
             (string->symbol (string-append "%_" (number->string anon 32)))]
            [(assq (car var) vmem)
             (lambda (binding)
               (set-cdr! binding (+ (cdr binding 1)))
               (string->symbol
                (string-append (symbol->string (car var))
                               "_"
                               (number->string (cdr binding) 32))))]
            [else (set! vmem (cons (cons (car var) 0) vmem))
                  (string->symbol
                   (string-append (symbol->string (car var))
                                  "_0"))]))))|#

;; A SISC Specific modification, generate-id-in-module deliberately
;; does not create a unique id, but rather one based on the module-id,
;; so that code dependent on a module won't depend on a specific
;; compilation of that module. This also handles nested modules.
(define (generate-module-id id)
  (if id
      (string->symbol (string-append "@" (symbol->string id)))
      (generate-id id)))
(define (generate-id-in-module module-id path id)
  (if (not module-id) (set! module-id (generate-id #f)))
  (if (not id) (set! id (generate-id #f)))
  (string->symbol
   (string-append
    (apply string-append
           "@"
           (symbol->string module-id)
           "::"
           (let loop ((path path) (res '()))
             (if (null? path)
                 res
                 (loop (cdr path) (cons (symbol->string (car path))
                                        (cons "::" res))))))
    (symbol->string id))))


;; SISC contains native functions map-car and map-cdr.  The following macro
;; translates these into ordinary maps when its not present
;(define-syntax map-car
; (syntax-rules ()
;  ((_ ls)
;   (map car ls))))
;
;(define-syntax map-cdr
; (syntax-rules ()
;  ((_ ls)
;   (map cdr ls))))


;  (let ((digits "0123456789abcdefghijklmnopqrstuvwxyz"))
;    (let ((base (string-length digits)) (session-key "_"))
;      (define make-digit (lambda (x) (string-ref digits x)))
;      (define fmt
;        (lambda (n)
;          (let fmt ((n n) (a '()))
;            (if (< n base)
;                (list->string (cons (make-digit n) a))
;                (let ((r (modulo n base)) (rest (quotient n base)))
;                  (fmt rest (cons (make-digit r) a)))))))
;      (let ((n -1))
;        (lambda (name)
;          (set! n (+ n 1))
;          (string->symbol (string-append (symbol->string name)
;                                         session-key
;                                         (fmt n))))))))
)


;;; output constructors
(begin
(define-syntax build-application
  (syntax-rules ()
    ((_ source fun-exp arg-exps)
     (if source 
         `(#%annotate (,fun-exp . ,arg-exps) ,source)
         `(,fun-exp . ,arg-exps)))))

(define-syntax build-conditional
  (syntax-rules ()
    ((_ source test-exp then-exp else-exp)
     (if source
       	 `(#%annotate (#%if ,test-exp ,then-exp ,else-exp) ,source)
         `(#%if ,test-exp ,then-exp ,else-exp)))))

(define-syntax build-lexical-reference
  (syntax-rules ()
    ((_ type source var)
     (if source 
         `(#%annotate ,var ,source)
         var))))

(define-syntax build-lexical-assignment
  (syntax-rules ()
    ((_ source var exp)
     (if source
         `(#%annotate (#%set! ,var ,exp) ,source)
         `(#%set! ,var ,exp)))))

(define-syntax build-global-reference
  (syntax-rules ()
    ((_ source var)
     (if source 
         `(#%annotate ,var ,source)
         var))))

(define-syntax build-global-assignment
  (syntax-rules ()
    ((_ source var exp)
     (if source
         `(#%annotate (#%set! ,var ,exp) ,source)
         `(#%set! ,var ,exp)))))

(define-syntax build-global-definition
  (syntax-rules ()
    ((_ source var exp)
     (if source
         `(#%annotate (#%define ,var ,exp) ,source)
         `(#%define ,var ,exp)))))

(define-syntax build-module-definition
 ; should have the effect of a global definition but may not appear at top level
  (identifier-syntax build-global-assignment))

(define-syntax build-cte-install
 ; should build a call that has the same effect as calling the
 ; global definition hook
  (syntax-rules ()
    ((_ sym exp) `($sc-put-cte ',sym ,exp))))

(define-syntax build-visit-only
 ; should mark the result as "visit only" for compile-file
 ; in implementations that support visit/revisit
  (syntax-rules ()
    ((_ exp) exp)))

(define-syntax build-revisit-only
 ; should mark the result as "revisit only" for compile-file,
 ; in implementations that support visit/revisit
  (syntax-rules ()
    ((_ exp) exp)))

(define-syntax build-lambda
  (syntax-rules ()
    ((_ src vars exp)
     (if src 
         `(#%annotate (#%lambda ,vars ,exp) ,src)
         `(#%lambda ,vars ,exp)))))

(define-syntax build-primref
  (syntax-rules ()
    ((_ src name) name)
    ((_ src level name) name)))

(define-syntax do-build-data
  (syntax-rules () 
    ((_ data)
     (if (atom? data) 
         data 
         `(#%quote ,data)))))
         
(define-syntax build-data
  (syntax-rules ()
    ((_ src exp) 
     (if src
         `(#%annotate ,(do-build-data exp) ,src)
         (do-build-data exp)))))

(define build-sequence
  (lambda (src exps)
    (cond [(null? exps) 
           (chi-void)]
          [(equal? '(void) (car exps))
           (build-sequence src (cdr exps))]
          [(null? (cdr exps)) 
           (if src 
               `(annotate ,(car exps) ,src)
               (car exps))]
          [else
            (if src
                `(#%annotate (#%begin ,@exps) ,src)
                `(#%begin ,@exps))])))

(define build-letrec
  (lambda (src vars val-exps body-exp)
    (if (null? vars)
        (if src
            `(#%annotate ,body-exp ,src)
            body-exp)
        (if src
            `(#%annotate 
              (#%letrec ,(map list vars val-exps) ,body-exp)
              ,src)
            `(#%letrec ,(map list vars val-exps) ,body-exp)))))

(define build-body
  (lambda (src vars val-exps body-exp)
    (build-letrec src vars val-exps body-exp)))

(define-syntax build-lexical-var
  (syntax-rules ()
    ((_ src id) (if src `(annotate ,(gen-sym id) ,src) 
        (gen-sym id)))))

)


(define-structure (syntax-object expression wrap))


(define-syntax unannotate
  (syntax-rules ()
    ((_ x)
     (let ((e x))
       (if (annotation? e)
           (annotation-expression e)
           e)))))

(define-syntax no-source (identifier-syntax #f))

(define source-annotation
  (lambda (x)
     (cond
       ((annotation? x) (annotation-source x))
       ((syntax-object? x) (source-annotation (syntax-object-expression x)))
       (else no-source))))

(define-syntax arg-check
  (syntax-rules ()
    ((_ pred? e who)
     (let ((x e))
       (unless (pred? x) (error-hook who "invalid argument" x))))))

;;; compile-time environments

;;; wrap and environment comprise two level mapping.
;;;   wrap : id --> label
;;;   env : label --> <element>

;;; environments are represented in two parts: a lexical part and a global
;;; part.  The lexical part is a simple list of associations from labels
;;; to bindings.  The global part is implemented by
;;; {put,get}-global-definition-hook and associates symbols with
;;; bindings.

;;; global (assumed global variable) and displaced-lexical (see below)
;;; do not show up in any environment; instead, they are fabricated by
;;; lookup when it finds no other bindings.

;;; <environment>              ::= ((<label> . <binding>)*)

;;; identifier bindings include a type and a value

;;; <binding> ::= (macro . <procedure>)           macros
;;;               (deferred . <expanded code>)    lazy-evaluation of transformers
;;;               (core . <procedure>)            core forms
;;;               (begin)                         begin
;;;               (define)                        define
;;;               (define-syntax)                 define-syntax
;;;               (local-syntax . rec?)           let-syntax/letrec-syntax
;;;               (eval-when)                     eval-when
;;;               (syntax . (<var> . <level>))    pattern variables
;;;               (global . <symbol>)             assumed global variable
;;;               (lexical . <var>)               lexical variables
;;;               (displaced-lexical . #f)        id-var-name not found in store
;;; <level>   ::= <nonnegative integer>
;;; <var>     ::= variable returned by build-lexical-var

;;; a macro is a user-defined syntactic-form.  a core is a system-defined
;;; syntactic form.  begin, define, define-syntax, and eval-when are
;;; treated specially since they are sensitive to whether the form is
;;; at top-level and (except for eval-when) can denote valid internal
;;; definitions.

;;; a pattern variable is a variable introduced by syntax-case and can
;;; be referenced only within a syntax form.

;;; any identifier for which no top-level syntax definition or local
;;; binding of any kind has been seen is assumed to be a global
;;; variable.

;;; a lexical variable is a lambda- or letrec-bound variable.

;;; a displaced-lexical identifier is a lexical identifier removed from
;;; it's scope by the return of a syntax object containing the identifier.
;;; a displaced lexical can also appear when a letrec-syntax-bound
;;; keyword is referenced on the rhs of one of the letrec-syntax clauses.
;;; a displaced lexical should never occur with properly written macros.

(define make-binding (lambda (x y) (cons x y)))
(define binding-type car)
(define binding-value cdr)
(define set-binding-type! set-car!)
(define set-binding-value! set-cdr!)
(define binding? (lambda (x) (and (pair? x) (symbol? (car x)))))

(define-syntax null-env (identifier-syntax '()))

(define extend-env
  (lambda (label binding r)
    (cons (cons label binding) r)))

(define extend-env*
  (lambda (labels bindings r)
    (if (null? labels)
        r
        (extend-env* (cdr labels) (cdr bindings)
          (extend-env (car labels) (car bindings) r)))))

(define extend-var-env*
  ; variant of extend-env* that forms "lexical" binding
  (lambda (labels vars r)
    (if (null? labels)
        r
        (extend-var-env* (cdr labels) (cdr vars)
          (extend-env (car labels) (make-binding 'lexical (car vars)) r)))))

;;; we use a "macros only" environment in expansion of local macro
;;; definitions so that their definitions can use local macros without
;;; attempting to use other lexical identifiers.
;;;
;;; - can make this null-env if we don't want to allow macros to use other
;;;   macros in defining their transformers
;;; - can add a cache here if it pays off
(define transformer-env
  (lambda (r)
    (if (null? r)
        '()
        (let ((a (car r)))
          (if (eq? (cadr a) 'lexical)       ; only strip out lexical so that (transformer x) works
              (transformer-env (cdr r))
              (cons a (transformer-env (cdr r))))))))

(define displaced-lexical-error
  (lambda (id)
    (syntax-error id
      (if (id-var-name id empty-wrap)
          "identifier out of context"
          "identifier not visible"))))

(define lookup*
  ; x may be a label or a symbol
  ; although symbols are usually global, we check the environment first
  ; anyway because a temporary binding may have been established by
  ; fluid-let-syntax
  (lambda (x r)
    (cond
      ((assq x r) => cdr)
      ((symbol? x)
       (or (get-global-definition-hook x) (make-binding 'global x)))
      (else (make-binding 'displaced-lexical #f)))))

(define sanitize-binding
  (lambda (b)
    (cond
      ((procedure? b) (make-binding 'macro b))
      ((binding? b)
       (case (binding-type b)
         ((core macro macro!) (and (procedure? (binding-value b)) b))
         ((module) (and (interface? (binding-value b)) b))
         (else b)))
      (else #f))))

(define lookup
  (lambda (x r)
    (define whack-binding!
      (lambda (b *b)
        (set-binding-type! b (binding-type *b))
        (set-binding-value! b (binding-value *b))))
    (let ((b (lookup* x r)))
      (case (binding-type b)
        ((deferred)
         (whack-binding! b
           (let ((*b (local-eval-hook (binding-value b))))
             (or (sanitize-binding *b)
                 (syntax-error *b "invalid transformer"))))
         (case (binding-type b)
           (else b)))
        (else b)))))

(define global-extend
  (lambda (type sym val)
    (put-global-definition-hook sym (make-binding type val))))


;;; Conceptually, identifiers are always syntax objects.  Internally,
;;; however, the wrap is sometimes maintained separately (a source of
;;; efficiency and confusion), so that symbols are also considered
;;; identifiers by id?.  Externally, they are always wrapped.

(define nonsymbol-id?
  (lambda (x)
    (and (syntax-object? x)
         (symbol? (unannotate (syntax-object-expression x))))))

;; Enforces <variable> in the R5RS lexical structure.  This
;; is more strict than <identifier>, as it excludes syntactic keywords
;; (sgm)
(define-syntax var? 
  (syntax-rules () 
    ((_ v)
     (id? v))))

;(define var?
 ; (lambda (x)
  ;  (cond
   ;   ((and (symbol? x)
    ;        (not (memq x '(quote lambda if set! begin cond and or case
     ;                            let let* letrec do delay quasiquote
      ;                           else => define unquote unquote-splicing))))
;       #t)
 ;     ((syntax-object? x) (var? (unannotate (syntax-object-expression x))))
  ;    ((annotation? x) (var? (annotation-expression x)))
   ;   (else #f))))

(define id?
  (lambda (x)
    (cond
      ((symbol? x) #t)
      ((syntax-object? x) (symbol? (unannotate (syntax-object-expression x))))
      ((annotation? x) (symbol? (annotation-expression x)))
      (else #f))))

(define-syntax id-sym-name
  (syntax-rules ()
    ((_ e)
     (let ((x e))
       (unannotate (if (syntax-object? x) (syntax-object-expression x) x))))))

(define id-sym-name&marks
  (lambda (x w)
    (if (syntax-object? x)
        (values
          (unannotate (syntax-object-expression x))
          (join-marks (wrap-marks w) (wrap-marks (syntax-object-wrap x))))
        (values (unannotate x) (wrap-marks w)))))

;;; syntax object wraps

;;;         <wrap> ::= ((<mark> ...) . (<subst> ...))
;;;        <subst> ::= <ribcage> | <shift>
;;;      <ribcage> ::= #((<ex-symname> ...) (<mark> ...) (<label> ...)) ; extensible, for chi-internal/external
;;;                  | #(#(<symname> ...) #(<mark> ...) #(<label> ...)) ; nonextensible
;;;   <ex-symname> ::= <symname> | <import token> | <barrier>
;;;        <shift> ::= shift
;;;      <barrier> ::= #f                                               ; inserted by import-only
;;; <import token> ::= #<"import-token" <token>>
;;;        <token> ::= <generated id>

(define make-wrap cons)
(define wrap-marks car)
(define wrap-subst cdr)


(define-syntax empty-wrap (identifier-syntax '(())))

(define-syntax top-wrap (identifier-syntax '((top))))

(define-syntax top-marked?
  (syntax-rules ()
    ((_ w) (memq 'top (wrap-marks w)))))

(define-syntax only-top-marked?
  (syntax-rules ()
    ((_ id) (same-marks? (wrap-marks (syntax-object-wrap id)) (wrap-marks top-wrap)))))

;;; labels

;;; simple labels must be comparable with "eq?" and distinct from symbols
;;; and pairs.

;;; indirect labels, which are implemented as pairs, are used to support
;;; import aliasing for identifiers exported (explictly or implicitly) from
;;; top-level modules.  chi-external creates an indirect label for each
;;; defined identifier, import causes the pair to be shared aliases it
;;; establishes, and chi-top-module whacks the pair to hold the top-level
;;; identifier name (symbol) if the id is to be placed at top level, before
;;; expanding the right-hand sides of the definitions in the module.

(module (gen-indirect-label indirect-label? get-indirect-label set-indirect-label!)
  (define-structure (indirect-label label))
  (define gen-indirect-label
    (lambda ()
      (make-indirect-label (gen-label))))
  (define get-indirect-label (lambda (x) (indirect-label-label x)))
  (define set-indirect-label! (lambda (x v) (set-indirect-label-label! x v))))

(define gen-label
  (lambda () (string #\i)))
(define label?
  (lambda (x)
    (or (string? x) ; normal lexical labels
        (symbol? x) ; global labels (symbolic names)
        (indirect-label? x))))

(define gen-labels
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (gen-label) (gen-labels (cdr ls))))))

(define-structure (ribcage symnames marks labels))

;;; Marks must be comparable with "eq?" and distinct from pairs and
;;; the symbol top.  We do not use integers so that marks will remain
;;; unique even across file compiles.

(define-syntax the-anti-mark (identifier-syntax #f))

(define anti-mark
  (lambda (w)
    (make-wrap (cons the-anti-mark (wrap-marks w))
               (cons 'shift (wrap-subst w)))))

(define-syntax new-mark
  (syntax-rules ()
    ((_) (string #\m))))

(define barrier-marker #f)

(define-structure (import-token key))

;;; make-empty-ribcage and extend-ribcage maintain list-based ribcages for
;;; internal definitions, in which the ribcages are built incrementally
(define-syntax make-empty-ribcage
  (syntax-rules ()
    ((_) (make-ribcage '() '() '()))))

(define extend-ribcage!
 ; must receive ids with complete wraps
 ; ribcage guaranteed to be list-based
  (lambda (ribcage id label)
    (set-ribcage-symnames! ribcage
      (cons (unannotate (syntax-object-expression id))
            (ribcage-symnames ribcage)))
    (set-ribcage-marks! ribcage
      (cons (wrap-marks (syntax-object-wrap id))
            (ribcage-marks ribcage)))
    (set-ribcage-labels! ribcage
      (cons label (ribcage-labels ribcage)))))

(define extend-ribcage-barrier!
 ; must receive ids with complete wraps
 ; ribcage guaranteed to be list-based
  (lambda (ribcage killer-id)
    (extend-ribcage-barrier-help! ribcage (syntax-object-wrap killer-id))))

(define extend-ribcage-barrier-help!
  (lambda (ribcage wrap)
    (set-ribcage-symnames! ribcage
      (cons barrier-marker (ribcage-symnames ribcage)))
    (set-ribcage-marks! ribcage
      (cons (wrap-marks wrap) (ribcage-marks ribcage)))))

(define extend-ribcage-subst!
 ; ribcage guaranteed to be list-based
  (lambda (ribcage token)
    (set-ribcage-symnames! ribcage
      (cons (make-import-token token) (ribcage-symnames ribcage)))))

(define lookup-import-binding-name
  (lambda (sym key marks)
    (let ((new (get-import-binding sym key)))
      (and new
           (let f ((new new))
             (cond
               ((pair? new) (or (f (car new)) (f (cdr new))))
               ((symbol? new)
                (and (same-marks? marks (wrap-marks top-wrap)) new))
               ((same-marks? marks (wrap-marks (syntax-object-wrap new))) new)
               (else #f)))))))

;;; make-binding-wrap creates vector-based ribcages
(define make-binding-wrap
  (lambda (ids labels w)
    (if (null? ids)
        w
        (make-wrap
          (wrap-marks w)
          (cons
            (let ((labelvec (list->vector labels)))
              (let ((n (vector-length labelvec)))
                (let ((symnamevec (make-vector n)) (marksvec (make-vector n)))
                  (let f ((ids ids) (i 0))
                    (unless (null? ids)
                      (let-values (((symname marks) (id-sym-name&marks (car ids) w)))
                        (vector-set! symnamevec i symname)
                        (vector-set! marksvec i marks)
                        (f (cdr ids) (fx+ i 1)))))
                  (make-ribcage symnamevec marksvec labelvec))))
            (wrap-subst w))))))

;;; make-trimmed-syntax-object is used by make-resolved-interface to support
;;; creation of module export lists whose constituent ids do not contain
;;; unnecessary substitutions or marks.
(define make-trimmed-syntax-object
  (lambda (id)
    (let-values (((tosym marks) (id-var-name&marks id empty-wrap)))
      (unless tosym
        (syntax-error id "identifier not visible for export"))
      (let ((fromsym (id-sym-name id)))
        (make-syntax-object fromsym
          (make-wrap marks
            (list (make-ribcage (vector fromsym) (vector marks) (vector tosym)))))))))

;;; Scheme's append should not copy the first argument if the second is
;;; nil, but it does, so we define a smart version here.
(define smart-append
  (lambda (m1 m2)
    (if (null? m2)
        m1
        (append m1 m2))))

(define join-wraps
  (lambda (w1 w2)
    (let ((m1 (wrap-marks w1)) (s1 (wrap-subst w1)))
      (if (null? m1)
          (if (null? s1)
              w2
              (make-wrap
                (wrap-marks w2)
                (smart-append s1 (wrap-subst w2))))
          (make-wrap
            (smart-append m1 (wrap-marks w2))
            (smart-append s1 (wrap-subst w2)))))))

(define join-marks
  (lambda (m1 m2)
    (smart-append m1 m2)))

(define same-marks?
  (lambda (x y)
    (or (eq? x y)
        (and (not (null? x))
             (not (null? y))
             (eq? (car x) (car y))
             (same-marks? (cdr x) (cdr y))))))

(define id-var-name-loc&marks
  (lambda (id w)
    (define search
      (lambda (sym subst marks)
        (if (null? subst)
            (values sym marks)
            (let ((fst (car subst)))
              (if (eq? fst 'shift)
                  (search sym (cdr subst) (cdr marks))
                  (let ((symnames (ribcage-symnames fst)))
                    (if (vector? symnames)
                        (search-vector-rib sym subst marks symnames fst)
                        (search-list-rib sym subst marks symnames fst))))))))
    (define search-list-rib
      (lambda (sym subst marks symnames ribcage)
        (let f ((symnames symnames) (i 0))
          (cond
            ((null? symnames) (search sym (cdr subst) marks))
            ((and (eq? (car symnames) sym)
                  (same-marks? marks (list-ref (ribcage-marks ribcage) i)))
             (values (list-ref (ribcage-labels ribcage) i) marks))
            ((import-token? (car symnames))
             (cond
               ((lookup-import-binding-name sym (import-token-key (car symnames)) marks) =>
                (lambda (id)
                  (if (symbol? id)
                      (values id marks)
                      (id-var-name&marks id empty-wrap))))   ; could be more efficient:  new is a resolved id
               (else (f (cdr symnames) i))))
            ((and (eq? (car symnames) barrier-marker)
                  (same-marks? marks (list-ref (ribcage-marks ribcage) i)))
             (values #f marks))
            (else (f (cdr symnames) (fx+ i 1)))))))
    (define search-vector-rib
      (lambda (sym subst marks symnames ribcage)
        (let ((n (vector-length symnames)))
          (let f ((i 0))
            (cond
              ((fx= i n) (search sym (cdr subst) marks))
              ((and (eq? (vector-ref symnames i) sym)
                    (same-marks? marks (vector-ref (ribcage-marks ribcage) i)))
               (values (vector-ref (ribcage-labels ribcage) i) marks))
              (else (f (fx+ i 1))))))))
    (cond
      ((symbol? id) (search id (wrap-subst w) (wrap-marks w)))
      ((syntax-object? id)
       (let ((sym (unannotate (syntax-object-expression id)))
             (w1 (syntax-object-wrap id)))
         (let ((marks (join-marks (wrap-marks w) (wrap-marks w1))))
           (let-values (((new-id marks) (search sym (wrap-subst w) marks)))
             (if (eq? new-id sym)
                 (search sym (wrap-subst w1) marks)
                 (values new-id marks))))))
      ((annotation? id) (search (unannotate id) (wrap-subst w) (wrap-marks w)))
      (else (error-hook 'id-var-name "invalid id" id)))))

(define id-var-name&marks
 ; this version follows indirect labels
  (lambda (id w)
    (let-values (((label marks) (id-var-name-loc&marks id w)))
      (values (if (indirect-label? label) (get-indirect-label label) label) marks))))

(define id-var-name-loc
 ; this version doesn't follow indirect labels
  (lambda (id w)
    (let-values (((label marks) (id-var-name-loc&marks id w)))
      label)))

(define id-var-name
 ; this version follows indirect labels
  (lambda (id w)
    (let-values (((label marks) (id-var-name-loc&marks id w)))
      (if (indirect-label? label) (get-indirect-label label) label))))

;;; free-id=? must be passed fully wrapped ids since (free-id=? x y)
;;; may be true even if (free-id=? (wrap x w) (wrap y w)) is not.

(define free-id=?
  (lambda (i j)
    (and (eq? (id-sym-name i) (id-sym-name j)) ; accelerator
         (eq? (id-var-name i empty-wrap) (id-var-name j empty-wrap)))))

;-(define-syntax literal-id=? (identifier-syntax free-id=?))
(define literal-id=?
  (lambda (id literal)
    (and (eq? (id-sym-name id) (id-sym-name literal))
         (let ((n-id (id-var-name id empty-wrap))
               (n-literal (id-var-name literal empty-wrap)))
           (or (eq? n-id n-literal)
               (and (or (not n-id) (symbol? n-id))
                    (or (not n-literal) (symbol? n-literal))))))))

;;; bound-id=? may be passed unwrapped (or partially wrapped) ids as
;;; long as the missing portion of the wrap is common to both of the ids
;;; since (bound-id=? x y) iff (bound-id=? (wrap x w) (wrap y w))

(define bound-id=?
  (lambda (i j)
    (if (and (syntax-object? i) (syntax-object? j))
        (and (eq? (unannotate (syntax-object-expression i))
                  (unannotate (syntax-object-expression j)))
             (same-marks? (wrap-marks (syntax-object-wrap i))
                  (wrap-marks (syntax-object-wrap j))))
        (eq? (unannotate i) (unannotate j)))))

;;; "valid-bound-ids?" returns #t if it receives a list of distinct ids.
;;; valid-bound-ids? may be passed unwrapped (or partially wrapped) ids
;;; as long as the missing portion of the wrap is common to all of the
;;; ids.

(define valid-bound-ids?
  (lambda (ids)
     (and (let all-ids? ((ids ids))
            (or (null? ids)
                (and (id? (car ids))
                     (all-ids? (cdr ids)))))
          (distinct-bound-ids? ids))))

;; "valid-bound-vars?" returns #t in the same conditions as 
;; valid-bound-ids?, except that it also enforces <variable>
;; instead of just <identifier>,  see vars? (sgm)
(define-syntax valid-bound-vars?
  (syntax-rules () 
    ((_ v)
     (valid-bound-ids? v))))

;  (lambda (vars)
 ;    (and (let all-vars? ((vars vars))
  ;          (or (null? vars)
   ;             (and (var? (car vars))
    ;                 (all-vars? (cdr vars)))))
     ;     (distinct-bound-ids? vars))))

;;; distinct-bound-ids? expects a list of ids and returns #t if there are
;;; no duplicates.  It is quadratic on the length of the id list; long
;;; lists could be sorted to make it more efficient.  distinct-bound-ids?
;;; may be passed unwrapped (or partially wrapped) ids as long as the
;;; missing portion of the wrap is common to all of the ids.

(define distinct-bound-ids?
  (lambda (ids)
    (let distinct? ((ids ids))
      (or (null? ids)
          (and (not (bound-id-member? (car ids) (cdr ids)))
               (distinct? (cdr ids)))))))

(define invalid-ids-error
 ; find first bad one and complain about it
  (lambda (ids exp class)
    (let find ((ids ids) (gooduns '()))
      (if (null? ids)
          (syntax-error exp) ; shouldn't happen
          (if (id? (car ids))
              (if (bound-id-member? (car ids) gooduns)
                  (syntax-error (car ids) "duplicate " class)
                  (find (cdr ids) (cons (car ids) gooduns)))
              (syntax-error (car ids) "invalid " class))))))

(define bound-id-member?
   (lambda (x list)
      (and (not (null? list))
           (or (bound-id=? x (car list))
               (bound-id-member? x (cdr list))))))

;;; wrapping expressions and identifiers

(define wrap
  (lambda (x w)
    (cond
      ((and (null? (wrap-marks w)) (null? (wrap-subst w))) x)
      ((syntax-object? x)
       (make-syntax-object
         (syntax-object-expression x)
         (join-wraps w (syntax-object-wrap x))))
      ((null? x) x)
      (else (make-syntax-object x w)))))

(define source-wrap
  (lambda (x w s)
    (wrap (if s (make-annotation x s #f) x) w)))

;;; expanding

(define chi-sequence
  (lambda (body r w s)
    (build-sequence s
      (let dobody ((body body) (r r) (w w))
        (if (null? body)
            '()
            (let ((first (chi (car body) r w)))
              (cons first (dobody (cdr body) r w))))))))

(define chi-top-sequence
  (lambda (body r w s ctem rtem ribcage)
    (build-sequence s
      (let dobody ((body body) (r r) (w w) (ctem ctem) (rtem rtem))
        (if (null? body)
            '()
            (let ((first (chi-top (car body) r w ctem rtem ribcage)))
              (cons first (dobody (cdr body) r w ctem rtem))))))))

(define chi-when-list
  (lambda (when-list w)
    ; when-list is syntax'd version of list of situations
    (map (lambda (x)
           (cond
             ((literal-id=? x (syntax compile)) 'compile)
             ((literal-id=? x (syntax load)) 'load)
             ((literal-id=? x (syntax visit)) 'visit)
             ((literal-id=? x (syntax revisit)) 'revisit)
             ((literal-id=? x (syntax eval)) 'eval)
             (else (syntax-error (wrap x w) "invalid eval-when situation"))))
         when-list)))

; The subsequent core definitions must be bound in psyntax as they
; need to be available to pass the appropriate implementation to
; the immutable syntax forms

(define core-quote
   (lambda (e r w s)
      (syntax-case e ()
         ((_ e) (build-data s (strip (syntax e) w)))
         (_ (syntax-error (source-wrap e w s))))))

(define core-if
   (lambda (e r w s)
      (syntax-case e ()
         ((_ test then)
          (build-conditional s
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (chi-void)))
         ((_ test then else)
          (build-conditional s
             (chi (syntax test) r w)
             (chi (syntax then) r w)
             (chi (syntax else) r w)))
         (_ (syntax-error (source-wrap e w s))))))

(define core-lambda   
  (lambda (e r w s)
     (syntax-case e ()
        ((_ . c)
         (chi-lambda-clause (source-wrap e w s) (syntax c) r w
           (lambda (vars body) (build-lambda s vars body)))))))

(define core-letrec
  (lambda (e r w s)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-vars? ids))
             (invalid-ids-error (map (lambda (x) (wrap x w)) ids)
               (source-wrap e w s) "bound variable")
             (let ((labels (gen-labels ids))
                   (new-vars (map gen-var ids)))
               (let ((w (make-binding-wrap ids labels w))
                    (r (extend-var-env* labels new-vars r)))
                 (build-letrec s
                   new-vars
                   (map (lambda (x) (chi x r w)) (syntax (val ...)))
                   (chi-body (syntax (e1 e2 ...)) (source-wrap e w s) r w)))))))
      (_ (syntax-error (source-wrap e w s))))))

;;; syntax-type returns five values: type, value, e, w, and s.  The first
;;; two are described in the table below.
;;;
;;;    type                   value         explanation
;;;    -------------------------------------------------------------------
;;;    begin                  none          begin keyword
;;;    begin-form             none          begin expression
;;;    call                   none          any other call
;;;    constant               none          self-evaluating datum
;;;    core                   procedure     core form (including singleton)
;;;    define                 none          define keyword
;;;    define-form            none          variable definition
;;;    define-syntax          none          define-syntax keyword
;;;    define-syntax-form     none          syntax definition
;;;    displaced-lexical      none          displaced lexical identifier
;;;    eval-when              none          eval-when keyword
;;;    eval-when-form         none          eval-when form
;;;    global                 name          global variable reference
;;;    import                 none          import keyword
;;;    import-form            none          import form
;;;    lexical                name          lexical variable reference
;;;    lexical-call           name          call to lexical variable
;;;    local-syntax           rec?          letrec-syntax/let-syntax keyword
;;;    local-syntax-form      rec?          syntax definition
;;;    module                 none          module keyword
;;;    module-form            none          module definition
;;;    other                  none          anything else
;;;    syntax                 level         pattern variable
;;;
;;; For all forms, e is the form, w is the wrap for e. and s is the source.
;;;
;;; syntax-type expands macros and unwraps as necessary to get to
;;; one of the forms above.

(define syntax-type
  (lambda (e r w s rib)
    (cond
      ((symbol? e)
       (let* ((n (id-var-name e w))
              (b (lookup n r))
              (type (binding-type b)))
         (case type
           ((lexical) (values type (binding-value b) e w s))
          ((global) (values type (binding-value b) e w s))
           ((macro macro!) (syntax-type (chi-macro (binding-value b) e r w s rib) r empty-wrap #f rib))
           (else (values type (binding-value b) e w s)))))
      ((pair? e)
       (let ((first (car e)))
         (cond ((id? first)
                (let* ((n (id-var-name first w))
                       (b (lookup n r))
                       (type (binding-type b)))
                  (case type
                    ((lexical) (values 'lexical-call (binding-value b) e w s))
                    ((macro macro!)
                     (syntax-type (chi-macro (binding-value b) e r w s rib)
                       r empty-wrap #f rib))
                    ((core) (values type (binding-value b) e w s))
                    ((local-syntax)
                     (values 'local-syntax-form (binding-value b) e w s))
                    ((begin) (values 'begin-form #f e w s))
                    ((eval-when) (values 'eval-when-form #f e w s))
                    ((define) (values 'define-form #f e w s))
                    ((define-syntax) (values 'define-syntax-form #f e w s))
                    ((module-key) (values 'module-form #f e w s))
                    ((import) (values 'import-form (and (binding-value b) (wrap first w)) e w s))
                    ((set!) (chi-set! e r w s rib))
                    (else (values 'call #f e w s)))))
               ((syntactic-token? first)
                (case first
                 ((#%begin) (values 'begin-form #f e w s))
                 ((#%define) (values 'define-form #f e w s))
                 ((#%set!) (chi-set! e r w s rib))
                 ((#%quote) (values 'core core-quote e w s))
                 ((#%if) (values 'core core-if e w s))
                 ((#%lambda) (values 'core core-lambda e w s))
                 ((#%letrec) (values 'core core-letrec e w s))
                 (else (values 'call #f e w s))))
               (else (values 'call #f e w s)))))
      ((syntax-object? e)
       ;; s can't be valid source if we've unwrapped
       (syntax-type (syntax-object-expression e)
                    r
                    (join-wraps w (syntax-object-wrap e))
                    no-source rib))
      ((annotation? e)
       (syntax-type (annotation-expression e) r w (annotation-source e) rib))
      ((self-evaluating? e) (values 'constant #f e w s))
      (else (values 'other #f e w s)))))

(define chi-top-expr
  (lambda (e r w top-ribcage)
    (let-values (((type value e w s) (syntax-type e r w no-source top-ribcage)))
      (chi-expr type value e r w s))))

(define chi-top
  (lambda (e r w ctem rtem top-ribcage)
    (let-values (((type value e w s) (syntax-type e r w no-source top-ribcage)))
      (case type
        ((begin-form)
         (syntax-case e ()
           ((_) (chi-void))
           ((_ e1 e2 ...)
            (chi-top-sequence (syntax (e1 e2 ...)) r w s ctem rtem top-ribcage))))
        ((local-syntax-form)
         (chi-local-syntax value e r w s
           (lambda (body r w s)
             (chi-top-sequence body r w s ctem rtem top-ribcage))))
        ((eval-when-form)
         (syntax-case e ()
           ((_ (x ...) e1 e2 ...)
            (let ((when-list (chi-when-list (syntax (x ...)) w))
                  (body (syntax (e1 e2 ...))))
              (let ((ctem (update-mode-set when-list ctem))
                    (rtem (update-mode-set when-list rtem)))
                (if (and (null? ctem) (null? rtem))
                    (chi-void)
                    (chi-top-sequence body r w s ctem rtem top-ribcage)))))))
        ((define-syntax-form)
         (parse-define-syntax e w s
           (lambda (id rhs w)
             (let ((id (wrap id w)))
               (let ((n (id-var-name id empty-wrap)))
                 (let ((b (lookup n r)))
                   (case (binding-type b)
                     ((displaced-lexical) (displaced-lexical-error id)))))
               (ct-eval/residualize ctem
                 (lambda ()
                   (build-cte-install
                     (let ((sym (id-sym-name id)))
                       (if (only-top-marked? id)
                           sym
                           (let ((marks (wrap-marks (syntax-object-wrap id))))
                             (make-syntax-object sym
                               (make-wrap marks
                                 (list (make-ribcage (vector sym)
                                         (vector marks) (vector (generate-id sym)))))))))
                     (chi rhs (transformer-env r) w))))))))
        ((define-form)
         (parse-define e w s
           (lambda (id rhs w)
             (let ((id (wrap id w)))
               (let ((n (id-var-name id empty-wrap)))
                 (let ((b (lookup n r)))
                   (case (binding-type b)
                     ((displaced-lexical) (displaced-lexical-error id)))))
               (let ((sym (id-sym-name id)))
                 (let ((valsym (if (only-top-marked? id) sym (generate-id sym))))
                   (build-sequence no-source
                    ; make sure compile-time definitions occur before we
                    ; expand the run-time code
                     (let ((x (ct-eval/residualize ctem
                                (lambda ()
                                  (build-cte-install
                                   (if (eq? sym valsym)
                                       sym
                                       (let ((marks (wrap-marks (syntax-object-wrap id))))
                                         (make-syntax-object sym
                                            (make-wrap marks
                                              (list (make-ribcage (vector sym)
                                                      (vector marks) (vector valsym)))))))
                                   (build-data no-source (make-binding 'global valsym)))))))
                       (list
                         x
                         (rt-eval/residualize rtem
                           (lambda ()
                             (build-global-definition s valsym (chi rhs r w))))))))
                 )))))
        ((module-form)
         (let ((r (cons '("top-level module placeholder" . (placeholder)) r))
               (ribcage (make-empty-ribcage)))
           (parse-module e w s (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w)))
             (lambda (id exports forms)
               (if id
                   (begin
                     (let ((n (id-var-name id empty-wrap)))
                       (let ((b (lookup n r)))
                         (case (binding-type b)
                           ((displaced-lexical) (displaced-lexical-error (wrap id w))))))
                     (chi-top-module e r ribcage w s ctem rtem id exports forms))
                   (chi-top-module e r ribcage w s ctem rtem #f exports forms))))))
        ((import-form)
         (parse-import e w s
           (lambda (mid)
             (ct-eval/residualize ctem
               (lambda ()
                 (when value (syntax-error (source-wrap e w s) "not valid at top-level"))
                 (let ((binding (lookup (id-var-name mid empty-wrap) null-env)))
                   (case (binding-type binding)
                     ((module) (do-top-import mid (interface-token (binding-value binding))))
                     ((displaced-lexical) (displaced-lexical-error mid))
                     (else (syntax-error mid "import from unknown module")))))))))
        (else (rt-eval/residualize rtem
                (lambda ()
                  (chi-expr type value e r w s))))))))

(define flatten-exports
  (lambda (exports)
    (let loop ((exports exports) (ls '()))
      (if (null? exports)
          ls
          (loop (cdr exports)
                (if (pair? (car exports))
                    (loop (car exports) ls)
                    (cons (car exports) ls)))))))


(define-structure (interface exports token))

(define make-trimmed-interface
 ; trim out implicit exports
  (lambda (exports)
    (make-interface
      (list->vector (map (lambda (x) (if (pair? x) (car x) x)) exports))
      #f)))

(define make-resolved-interface
 ; trim out implicit exports & resolve others to actual top-level symbol
  (lambda (exports import-token)
    (make-interface
      (list->vector (map (lambda (x) (make-trimmed-syntax-object (if (pair? x) (car x) x))) exports))
      import-token)))

(define-structure (module-binding type id label imps val))

(define chi-top-module
  (lambda (e r ribcage w s ctem rtem id exports forms)
    (let ((fexports (flatten-exports exports))
          (module-id (and id (id-sym-name id))))
      (chi-external ribcage (source-wrap e w s)
                    (map (lambda (d) (cons r d)) forms) r exports fexports ctem
                    (lambda (bindings inits)
                      ;; dvs & des: "defined" (letrec-bound) vars & rhs expressions
                      ;; svs & ses: "set!" (top-level) vars & rhs expressions
                      (let partition ((fexports (map (lambda (x) (cons x '())) fexports)) (bs bindings) (svs '()) (ses '()) (ctdefs '()) (allimps '()))
                        (if (null? fexports)
                            ;; remaining bindings are either local vars or local macros/modules
                            (let partition ((bs bs) (dvs '()) (des '()))
                              (if (null? bs)
                                  (let ((ses (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) ses))
                                        (des (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) des))
                                        (inits (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) inits)))
                                    ;; we wait to do this here so that expansion of des & ses use
                                    ;; local versions, which in particular, allows us to use macros
                                    ;; locally even if ctem tells us not to eval them
                                    (for-each (lambda (x)
                                                (apply (lambda (t label sym val)
                                                         (when label (set-indirect-label! label sym)))
                                                       x))
                                              ctdefs)
                                    (build-sequence no-source
                                                    (list (ct-eval/residualize
                                                           ctem
                                                           (lambda ()
                                                             (if (null? ctdefs)
                                                                 (chi-void)
                                                                 (build-sequence no-source
                                                                                 (map (lambda (x)
                                                                                        (apply (lambda (t label sym val)
                                                                                                 (build-cte-install sym
                                                                                                                    (if (eq? t 'define-syntax-form)
                                                                                                                        val
                                                                                                                        (build-data no-source
                                                                                                                                    (make-binding 'module
                                                                                                                                                  (make-resolved-interface val sym))))))
                                                                                               x))
                                                                                      ctdefs)))))
                                                          (ct-eval/residualize
                                                           ctem
                                                           (lambda ()
                                                             (let ((n (if id (id-sym-name id) #f)))
                                                               (let* ((token (generate-module-id n))
                                                                      (b (build-data no-source
                                                                                     (make-binding 'module
                                                                                                   (make-resolved-interface exports token)))))
                                                                 (if n
                                                                     (build-cte-install
                                                                      (if (only-top-marked? id)
                                                                          n
                                                                          (let ((marks (wrap-marks (syntax-object-wrap id))))
                                                                            (make-syntax-object n
                                                                                                (make-wrap marks
                                                                                                           (list (make-ribcage (vector n)
                                                                                                                               (vector marks) (vector (generate-module-id n))))))))
                                                                      b)
                                                                     (let ((n (generate-id 'tmp)))
                                                                       (build-sequence no-source
                                                                                       (list (build-cte-install n b)
                                                                                             (do-top-import n token)))))))))
                                                          ;; Some systems complain when undefined variables are assigned.
                                                          (if (null? svs)
                                                              (chi-void)
                                                              (build-sequence no-source
                                                                              (map (lambda (v) (build-global-definition no-source v (chi-void))) svs)))
                                                          (rt-eval/residualize
                                                           rtem
                                                           (lambda ()
                                                             (build-body no-source
                                                                         dvs
                                                                         des
                                                                         (build-sequence no-source
                                                                                         (list
                                                                                          (if (null? svs)
                                                                                              (chi-void)
                                                                                              (build-sequence no-source
                                                                                                              (map (lambda (v e)
                                                                                                                     (build-module-definition no-source v e))
                                                                                                                   svs
                                                                                                                   ses)))
                                                                                          (if (null? inits)
                                                                                              (chi-void)
                                                                                              (build-sequence no-source inits)))))))
                                                          (chi-void))))
                                  (let ((b (car bs)))
                                    (case (module-binding-type b)
                                      ((define-form)
                                       (let ((var (gen-var (module-binding-id b))))
                                         (extend-store! r
                                                        (get-indirect-label (module-binding-label b))
                                                        (make-binding 'lexical var))
                                         (partition (cdr bs) (cons var dvs)
                                                    (cons (module-binding-val b) des))))
                                      ((define-syntax-form module-form) (partition (cdr bs) dvs des))
                                      (else (error 'sc-expand-internal "unexpected module binding type"))))))
                            (let ((exp (car fexports)) (fexports (cdr fexports)))
                              (define pluck-binding
                                (lambda (id bs succ fail)
                                  (let loop ((bs bs) (new-bs '()))
                                    (if (null? bs)
                                        (fail)
                                        ;; this previously used bound-id=?, but free-id=?
                                        ;; can prevent false positives and is okay since the
                                        ;; substitutions have already been applied
                                        (if (free-id=? (module-binding-id (car bs)) id)
                                            (succ (car bs) (smart-append (reverse new-bs) (cdr bs)))
                                            (loop (cdr bs) (cons (car bs) new-bs)))))))
                              (pluck-binding (car exp) bs
                                             (lambda (b bs)
                                               (let ((t (module-binding-type b))
                                                     (label (module-binding-label b))
                                                     (imps (module-binding-imps b))
                                                     (id (car exp)))
                                                 (let ((fexports (append (map (lambda (x) (cons x '())) imps) fexports))
                                                       (sym
                                                        (begin
                                                          (generate-id-in-module
                                                             (and module-id
                                                                  (not (member id allimps))
                                                                  module-id)
                                                             (cdr exp)
                                                             (id-sym-name id)))))
                                                   (case t
                                                     ((define-form)
                                                      (set-indirect-label! label sym)
                                                      (partition fexports bs (cons sym svs)
                                                                 (cons (module-binding-val b) ses)
                                                                 ctdefs
                                                                 (append imps allimps)))
                                                     ((define-syntax-form)
                                                      (partition fexports bs svs ses
                                                                 (cons (list t label sym (module-binding-val b))
                                                                       ctdefs)
                                                                 (append imps allimps)))
                                                     ((module-form)
                                                      (let ((exports (module-binding-val b))
                                                            (id-path (cons (id-sym-name id) (cdr exp))))
                                                        (partition (append (map (lambda (x) (cons x id-path)) (flatten-exports exports))
                                                                           fexports)
                                                                   bs svs ses
                                                                   (cons (list t label sym exports)
                                                                         ctdefs)
                                                                   (append imps allimps))))
                                                     (else (error 'sc-expand-internal "unexpected module binding type"))))))
                                             (lambda () (partition fexports bs svs ses ctdefs allimps)))))))))))

(define id-set-diff
  (lambda (exports defs)
    (cond
      ((null? exports) '())
      ((bound-id-member? (car exports) defs) (id-set-diff (cdr exports) defs))
      (else (cons (car exports) (id-set-diff (cdr exports) defs))))))

(define extend-store!
  (lambda (r label binding)
    (set-cdr! r (extend-env label binding (cdr r)))))

(define check-module-exports
  ; After processing the definitions of a module this is called to verify that the
  ; module has defined or imported each exported identifier.  Because ids in fexports are
  ; wrapped with the given ribcage, they will contain substitutions for anything defined
  ; or imported here.  These subsitutions can be used by do-import! and do-import-top! to
  ; provide access to reexported bindings, for example.
  (lambda (source-exp fexports ids)
    (define defined?
      (lambda (e ids)
        (ormap (lambda (x)
                 (if (interface? x)
                     (let ((token (interface-token x)))
                       (if token
                           (lookup-import-binding-name (id-sym-name e) token (wrap-marks (syntax-object-wrap e)))
                           (let ((v (interface-exports x)))
                             (let lp ((i (fx- (vector-length v) 1)))
                               (and (fx>= i 0)
                                    (or (bound-id=? e (vector-ref v i))
                                        (lp (fx- i 1))))))))
                     (bound-id=? e x)))
               ids)))
    (let loop ((fexports fexports) (missing '()))
      (if (null? fexports)
          (unless (null? missing) (syntax-error missing "missing definition for export(s)"))
          (let ((e (car fexports)) (fexports (cdr fexports)))
            (if (defined? e ids)
                (loop fexports missing)
                (loop fexports (cons e missing))))))))

(define check-defined-ids
  (lambda (source-exp ls)
    (define b-i=?
      ; cope with fat-fingered top-level
      (lambda (x y)
        (if (symbol? x)
            (if (symbol? y)
                (eq? x y)
                (and (eq? x (id-sym-name y))
                     (same-marks? (wrap-marks (syntax-object-wrap y)) (wrap-marks top-wrap))))
            (if (symbol? y)
                (and (eq? y (id-sym-name x))
                     (same-marks? (wrap-marks (syntax-object-wrap x)) (wrap-marks top-wrap)))
                (bound-id=? x y)))))
    (define vfold
      (lambda (v p cls)
        (let ((len (vector-length v)))
          (let lp ((i 0) (cls cls))
            (if (fx= i len)
                cls
                (lp (fx+ i 1) (p (vector-ref v i) cls)))))))
    (define conflicts
      (lambda (x y cls)
        (if (interface? x)
            (if (interface? y)
                (let-values (((iface exports)
                              (let ((xe (interface-exports x)) (ye (interface-exports y)))
                                (if (fx> (vector-length xe) (vector-length ye))
                                    (values x ye)
                                    (values y xe)))))
                  (vfold exports (lambda (id cls) (id-iface-conflicts id iface cls)) cls))
                (id-iface-conflicts y x cls))
            (if (interface? y)
                (id-iface-conflicts x y cls)
                (if (b-i=? x y) (cons x cls) cls)))))
     (define id-iface-conflicts
       (lambda (id iface cls)
         (let ((token (interface-token iface)))
           (if token
               (if (lookup-import-binding-name (id-sym-name id) token
                     (if (symbol? id)
                         (wrap-marks top-wrap)
                         (wrap-marks (syntax-object-wrap id))))
                   (cons id cls)
                   cls)
               (vfold (interface-exports iface)
                      (lambda (*id cls) (if (b-i=? *id id) (cons *id cls) cls))
                      cls)))))
     (unless (null? ls)
       (let lp ((x (car ls)) (ls (cdr ls)) (cls '()))
         (if (null? ls)
             (unless (null? cls)
               (let ((cls (syntax-object->datum cls)))
                 (syntax-error source-exp "duplicate definition for "
                  (symbol->string (car cls))
                   " in")))
             (let lp2 ((ls2 ls) (cls cls))
               (if (null? ls2)
                   (lp (car ls) (cdr ls) cls)
                   (lp2 (cdr ls2) (conflicts x (car ls2) cls)))))))))

(define chi-external
  (lambda (ribcage source-exp body r exports fexports ctem k)
    (define return
      (lambda (bindings ids inits)
        (check-defined-ids source-exp ids)
        (check-module-exports source-exp fexports ids)
        (k bindings inits)))
    (define get-implicit-exports
      (lambda (id)
        (let f ((exports exports))
          (if (null? exports)
              '()
              (if (and (pair? (car exports)) (bound-id=? id (caar exports)))
                  (flatten-exports (cdar exports))
                  (f (cdr exports)))))))
    (define update-imp-exports
      (lambda (bindings exports)
        (let ((exports (map (lambda (x) (if (pair? x) (car x) x)) exports)))
          (map (lambda (b)
                 (let ((id (module-binding-id b)))
                   (if (not (bound-id-member? id exports))
                       b
                       (make-module-binding
                        (module-binding-type b)
                        id
                        (module-binding-label b)
                        (append (get-implicit-exports id)
                                (module-binding-imps b))
                        (module-binding-val b)))))
               bindings))))
    (let parse ((body body) (ids '()) (bindings '()) (inits '()))
      (if (null? body)
          (return bindings ids inits)
          (let ((e (cdar body)) (er (caar body)))
            (let-values (((type value e w s) (syntax-type e er empty-wrap no-source ribcage)))
              (case type
                ((define-form)
                 (parse-define e w s
                               (lambda (id rhs w)
                                 (let* ((id (wrap id w))
                                        (label (gen-indirect-label))
                                        (imps (get-implicit-exports id)))
                                   (extend-ribcage! ribcage id label)
                                   (parse
                                    (cdr body)
                                    (cons id ids)
                                    (cons (make-module-binding type id label
                                                               imps (cons er (wrap rhs w)))
                                          bindings)
                                    inits)))))
                ((define-syntax-form)
                 (parse-define-syntax e w s
                                      (lambda (id rhs w)
                                        (let* ((id (wrap id w))
                                               (label (gen-indirect-label))
                                               (imps (get-implicit-exports id))
                                               (exp (chi rhs (transformer-env er) w)))
                                        ; arrange to evaluate the transformer lazily
                                          (extend-store! r (get-indirect-label label) (cons 'deferred exp))
                                          (extend-ribcage! ribcage id label)
                                          (parse
                                           (cdr body)
                                           (cons id ids)
                                           (cons (make-module-binding type id label imps exp)
                                                 bindings)
                                           inits)))))
                ((module-form)
                 (let* ((*ribcage (make-empty-ribcage))
                        (*w (make-wrap (wrap-marks w) (cons *ribcage (wrap-subst w)))))
                   (parse-module e w s *w
                                 (lambda (id *exports forms)
                                   (chi-external *ribcage (source-wrap e w s)
                                                 (map (lambda (d) (cons er d)) forms)
                                                 r *exports (flatten-exports *exports) ctem
                                                 (lambda (*bindings *inits)
                                                   (let* ((iface (make-trimmed-interface *exports))
                                                          (bindings (append (if id
                                                                                *bindings
                                                                                (update-imp-exports
                                                                                 *bindings *exports))
                                                                            bindings))
                                                          (inits (append inits *inits)))
                                                     (if id
                                                         (let ((label (gen-indirect-label))
                                                               (imps (get-implicit-exports id)))
                                                           (extend-store! r (get-indirect-label label)
                                                                          (make-binding 'module iface))
                                                           (extend-ribcage! ribcage id label)
                                                           (parse
                                                            (cdr body)
                                                            (cons id ids)
                                                            (cons (make-module-binding type id label imps *exports) bindings)
                                                            inits))
                                                         (let ()
                                                           (do-import! iface ribcage)
                                                           (parse (cdr body) (cons iface ids) bindings inits))))))))))
                ((import-form)
                 (parse-import e w s
                               (lambda (mid)
                                 (let ((mlabel (id-var-name mid empty-wrap)))
                                   (let ((binding (lookup mlabel r)))
                                     (case (binding-type binding)
                                       ((module)
                                        (let ((iface (binding-value binding)))
                                          (when value (extend-ribcage-barrier! ribcage value))
                                          (do-import! iface ribcage)
                                          (parse
                                           (cdr body)
                                           (cons iface ids)
                                           (update-imp-exports bindings (vector->list (interface-exports iface)))
                                           inits)))
                                       ((displaced-lexical) (displaced-lexical-error mid))
                                       (else (syntax-error mid "import from unknown module"))))))))
                ((begin-form)
                 (syntax-case e ()
                   ((_ e1 ...)
                    (parse (let f ((forms (syntax (e1 ...))))
                             (if (null? forms)
                                 (cdr body)
                                 (cons (cons er (wrap (car forms) w))
                                       (f (cdr forms)))))
                           ids bindings inits))))
                ((eval-when-form)
                 (syntax-case e ()
                   ((_ (x ...) e1 ...)
                                        ; mode set is implicitly (E)
                    (parse (if (memq 'eval (chi-when-list (syntax (x ...)) w))
                               (let f ((forms (syntax (e1 ...))))
                                 (if (null? forms)
                                     (cdr body)
                                     (cons (cons er (wrap (car forms) w))
                                           (f (cdr forms)))))
                               (cdr body))
                           ids bindings inits))))
                ((local-syntax-form)
                 (chi-local-syntax value e er w s
                                   (lambda (forms er w s)
                                     (parse (let f ((forms forms))
                                              (if (null? forms)
                                                  (cdr body)
                                                  (cons (cons er (wrap (car forms) w))
                                                        (f (cdr forms)))))
                                            ids bindings inits))))
                (else                   ; found an init expression
                  (return bindings ids
                          (append inits (cons (cons er (source-wrap e w s)) (cdr body))))))))))))

(define vmap
  (lambda (fn v)
    (do ((i (fx- (vector-length v) 1) (fx- i 1))
         (ls '() (cons (fn (vector-ref v i)) ls)))
        ((fx< i 0) ls))))

(define vfor-each
  (lambda (fn v)
    (let ((len (vector-length v)))
      (do ((i 0 (fx+ i 1)))
          ((fx= i len))
        (fn (vector-ref v i))))))

(define do-top-import
  (lambda (mid token)
    (build-cte-install mid
      (build-data no-source
        (make-binding 'do-import token)))))

(define update-mode-set
  (let ((table
         '((L (load . L) (compile . C) (visit . V) (revisit . R) (eval . -))
           (C (load . -) (compile . -) (visit . -) (revisit . -) (eval . C))
           (V (load . V) (compile . C) (visit . V) (revisit . -) (eval . -))
           (R (load . R) (compile . C) (visit . -) (revisit . R) (eval . -))
           (E (load . -) (compile . -) (visit . -) (revisit . -) (eval . E)))))
    (lambda (when-list mode-set)
      (remq '-
        (apply append
          (map (lambda (m)
                 (let ((row (cdr (assq m table))))
                   (map (lambda (s) (cdr (assq s row)))
                        when-list)))
               mode-set))))))

(define initial-mode-set
  (lambda (when-list compiling-a-file)
    (apply append
      (map (lambda (s)
             (if compiling-a-file
                 (case s
                   ((compile) '(C))
                   ((load) '(L))
                   ((visit) '(V))
                   ((revisit) '(R))
                   (else '()))
                 (case s
                   ((eval) '(E))
                   (else '()))))
           when-list))))

(define rt-eval/residualize
  (lambda (rtem thunk)
    (if (memq 'E rtem)
        (thunk)
        (let ((thunk (if (memq 'C rtem)
                         (let ((x (thunk)))
                           (top-level-eval-hook x)
                           (lambda () x))
                         thunk)))
          (if (memq 'V rtem)
              (if (or (memq 'L rtem) (memq 'R rtem))
                  (thunk) ; visit-revisit
                  (build-visit-only (thunk)))
              (if (or (memq 'L rtem) (memq 'R rtem))
                  (build-revisit-only (thunk))
                  (chi-void)))))))

(define ct-eval/residualize
  (lambda (ctem thunk)
    (if (memq 'E ctem)
        (begin (top-level-eval-hook (thunk)) (chi-void))
        (let ((thunk (if (memq 'C ctem)
                         (let ((x (thunk)))
                           (top-level-eval-hook x)
                           (lambda () x))
                         thunk)))
          (if (memq 'R ctem)
              (if (or (memq 'L ctem) (memq 'V ctem))
                  (thunk) ; visit-revisit
                  (build-revisit-only (thunk)))
              (if (or (memq 'L ctem) (memq 'V ctem))
                  (build-visit-only (thunk))
                  (chi-void)))))))

(define chi
  (lambda (e r w)
    (let-values (((type value e w s) (syntax-type e r w no-source #f)))
      (chi-expr type value e r w s))))

(define chi-expr
  (lambda (type value e r w s)
    (case type
      ((lexical)
       (build-lexical-reference 'value s value))
      ((core) (value e r w s))
      ((lexical-call)
       (chi-application
         (build-lexical-reference 'fun (source-annotation (car e)) value)
         e r w s))
      ((constant) (build-data s (strip (source-wrap e w s) empty-wrap)))
      ((global) (build-global-reference s value))
      ((call) (chi-application (chi (car e) r w) e r w s))
      ((begin-form)
       (syntax-case e ()
         ((_ e1 e2 ...) (chi-sequence (syntax (e1 e2 ...)) r w s))))
      ((local-syntax-form)
       (chi-local-syntax value e r w s chi-sequence))
      ((eval-when-form)
       (syntax-case e ()
         ((_ (x ...) e1 e2 ...)
          ; mode set is implicitly (E)
          (if (memq 'eval (chi-when-list (syntax (x ...)) w))
              (chi-sequence (syntax (e1 e2 ...)) r w s)
              (chi-void)))))
      ((define-form define-syntax-form module-form import-form)
       (syntax-error (source-wrap e w s) "invalid context for definition"))
      ((syntax)
       (syntax-error (source-wrap e w s)
         "reference to pattern variable outside syntax form"))
      ((displaced-lexical) (displaced-lexical-error (source-wrap e w s)))
      (else (syntax-error (source-wrap e w s))))))

(define chi-application
  (lambda (x e r w s)
    (syntax-case e ()
      ((e0 e1 ...)
       (build-application s x
         (map (lambda (e) (chi e r w)) (syntax (e1 ...)))))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-set!
  (lambda (e r w s rib)
    (syntax-case e ()
      ((_ id val)
       (id? (syntax id))
       (let ((n (id-var-name (syntax id) w)))
         (let ((b (lookup n r)))
           (case (binding-type b)
             ((macro!)
              (let ((id (wrap (syntax id) w)) (val (wrap (syntax val) w)))
                (syntax-type (chi-macro (binding-value b)
                               `(,(syntax set!) ,id ,val)
                               r empty-wrap s rib) r empty-wrap s rib)))
             (else
              (values 'core
                (lambda (e r w s)
                 ; repeat lookup in case we were first expression (init) in
                 ; module or lambda body.  we repeat id-var-name as well,
                 ; although this is only necessary if we allow inits to
                 ; preced definitions
                  (let ((val (chi (syntax val) r w))
                        (n (id-var-name (syntax id) w)))
                    (let ((b (lookup n r)))
                      (case (binding-type b)
                        ((lexical) (build-lexical-assignment s (binding-value b) val))
                        ((global) (build-global-assignment s (binding-value b) val))
                        ((displaced-lexical)
                         (syntax-error (wrap (syntax id) w) "identifier out of context"))
                        (else (syntax-error (source-wrap e w s)))))))
                e w s))))))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-macro
  (lambda (p e r w s rib)
    (define rebuild-macro-output
      (lambda (x m)
        (cond ((pair? x)
               (cons (rebuild-macro-output (car x) m)
                     (rebuild-macro-output (cdr x) m)))
              ((syntax-object? x)
               (let ((w (syntax-object-wrap x)))
                 (let ((ms (wrap-marks w)) (s (wrap-subst w)))
                   (make-syntax-object (syntax-object-expression x)
                     (if (and (pair? ms) (eq? (car ms) the-anti-mark))
                         (make-wrap (cdr ms) (cdr s))
                         (make-wrap (cons m ms)
                           (if rib
                               (cons rib (cons 'shift s))
                               (cons 'shift s))))))))
              ((vector? x)
               (let* ((n (vector-length x)) (v (make-vector n)))
                 (do ((i 0 (fx+ i 1)))
                     ((fx= i n) v)
                     (vector-set! v i
                       (rebuild-macro-output (vector-ref x i) m)))))
              ((symbol? x)
               (syntax-error (source-wrap e w s)
                 "encountered raw symbol "
                 (format "~s" x)
                 " in output of macro"))
              (else x))))
    (rebuild-macro-output
      (let ((out (p (source-wrap e (anti-mark w) s))))
        (if (procedure? out)
            (out (lambda (id)
                   (unless (identifier? id)
                     (syntax-error id
                       "environment argument is not an identifier"))
                   (lookup (id-var-name id empty-wrap) r)))
            out))
      (new-mark))))

(define chi-body
  ;; Here we create the empty wrap and new environment with placeholder
  ;; as required by chi-internal.  On return we extend the environment
  ;; to recognize the var-labels as lexical variables and build a letrec
  ;; binding them to the var-vals which we expand here.
  (lambda (body outer-form r w)
    (let* ((r (cons '("placeholder" . (placeholder)) r))
           (ribcage (make-empty-ribcage))
           (w (make-wrap (wrap-marks w) (cons ribcage (wrap-subst w))))
           (body (map (lambda (x) (cons r (wrap x w))) body)))
      (chi-internal ribcage outer-form body r
        (lambda (exprs ids vars vals inits)
          (when (null? exprs) (syntax-error outer-form "no expressions in body"))
          (build-body no-source
            vars
            (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) vals)
            (build-sequence no-source
              (map (lambda (x) (chi (cdr x) (car x) empty-wrap)) (append inits exprs)))))))))

(define chi-internal
  ;; In processing the forms of the body, we create a new, empty wrap.
  ;; This wrap is augmented (destructively) each time we discover that
  ;; the next form is a definition.  This is done:
  ;;
  ;;   (1) to allow the first nondefinition form to be a call to
  ;;       one of the defined ids even if the id previously denoted a
  ;;       definition keyword or keyword for a macro expanding into a
  ;;       definition;
  ;;   (2) to prevent subsequent definition forms (but unfortunately
  ;;       not earlier ones) and the first nondefinition form from
  ;;       confusing one of the bound identifiers for an auxiliary
  ;;       keyword; and
  ;;   (3) so that we do not need to restart the expansion of the
  ;;       first nondefinition form, which is problematic anyway
  ;;       since it might be the first element of a begin that we
  ;;       have just spliced into the body (meaning if we restarted,
  ;;       we'd really need to restart with the begin or the macro
  ;;       call that expanded into the begin, and we'd have to give
  ;;       up allowing (begin <defn>+ <expr>+), which is itself
  ;;       problematic since we don't know if a begin contains only
  ;;       definitions until we've expanded it).
  ;;
  ;; Before processing the body, we also create a new environment
  ;; containing a placeholder for the bindings we will add later and
  ;; associate this environment with each form.  In processing a
  ;; let-syntax or letrec-syntax, the associated environment may be
  ;; augmented with local keyword bindings, so the environment may
  ;; be different for different forms in the body.  Once we have
  ;; gathered up all of the definitions, we evaluate the transformer
  ;; expressions and splice into r at the placeholder the new variable
  ;; and keyword bindings.  This allows let-syntax or letrec-syntax
  ;; forms local to a portion or all of the body to shadow the
  ;; definition bindings.
  ;;
  ;; Subforms of a begin, let-syntax, or letrec-syntax are spliced
  ;; into the body.
  ;;
  ;; outer-form is fully wrapped w/source
  (lambda (ribcage source-exp body r k)
    (define return
      (lambda (exprs ids vars vals inits)
        (check-defined-ids source-exp ids)
        (k exprs ids (reverse vars) (reverse vals) inits)))
    (let parse ((body body) (ids '()) (vars '()) (vals '()) (inits '()))
      (if (null? body)
          (return body ids vars vals inits)
          (let ((e (cdar body)) (er (caar body)))
            (let-values (((type value e w s) (syntax-type e er empty-wrap no-source ribcage)))
              (case type
                ((define-form)
                 (parse-define e w s
                   (lambda (id rhs w)
                     (let ((id (wrap id w)) (label (gen-label)))
                       (let ((var (gen-var id)))
                         (extend-ribcage! ribcage id label)
                         (extend-store! r label (make-binding 'lexical var))
                         (parse
                           (cdr body)
                           (cons id ids)
                           (cons var vars)
                           (cons (cons er (wrap rhs w)) vals)
                           inits))))))
                ((define-syntax-form)
                 (parse-define-syntax e w s
                   (lambda (id rhs w)
                     (let ((id (wrap id w))
                           (label (gen-label))
                           (exp (chi rhs (transformer-env er) w)))
                       (extend-ribcage! ribcage id label)
                       (extend-store! r label (make-binding 'deferred exp))
                       (parse (cdr body) (cons id ids) vars vals inits)))))
                ((module-form)
                 (let* ((*ribcage (make-empty-ribcage))
                        (*w (make-wrap (wrap-marks w) (cons *ribcage (wrap-subst w)))))
                   (parse-module e w s *w
                     (lambda (id exports forms)
                       (chi-internal *ribcage (source-wrap e w s)
                         (map (lambda (d) (cons er d)) forms) r
                         (lambda (*body *ids *vars *vals *inits)
                           ; valid bound ids checked already by chi-internal
                           (check-module-exports source-exp (flatten-exports exports) *ids)
                           (let ((iface (make-trimmed-interface exports))
                                 (vars (append *vars vars))
                                 (vals (append *vals vals))
                                 (inits (append inits *inits *body)))
                             (if id
                                 (let ((label (gen-label)))
                                   (extend-ribcage! ribcage id label)
                                   (extend-store! r label (make-binding 'module iface))
                                   (parse (cdr body) (cons id ids) vars vals inits))
                                 (let ()
                                   (do-import! iface ribcage)
                                   (parse (cdr body) (cons iface ids) vars vals inits))))))))))
               ((import-form)
                (parse-import e w s
                  (lambda (mid)
                    (let ((mlabel (id-var-name mid empty-wrap)))
                      (let ((binding (lookup mlabel r)))
                        (case (car binding)
                          ((module)
                           (let ((iface (cdr binding)))
                             (when value (extend-ribcage-barrier! ribcage value))
                             (do-import! iface ribcage)
                             (parse (cdr body) (cons iface ids) vars vals inits)))
                          ((displaced-lexical) (displaced-lexical-error mid))
                          (else (syntax-error mid "import from unknown module"))))))))
                ((begin-form)
                 (syntax-case e ()
                   ((_ e1 ...)
                    (parse (let f ((forms (syntax (e1 ...))))
                             (if (null? forms)
                                 (cdr body)
                                 (cons (cons er (wrap (car forms) w))
                                       (f (cdr forms)))))
                      ids vars vals inits))))
                ((eval-when-form)
                 (syntax-case e ()
                   ((_ (x ...) e1 ...)
                    ; mode set is implicitly (E)
                    (parse (if (memq 'eval (chi-when-list (syntax (x ...)) w))
                               (let f ((forms (syntax (e1 ...))))
                                 (if (null? forms)
                                     (cdr body)
                                     (cons (cons er (wrap (car forms) w))
                                           (f (cdr forms)))))
                               (cdr body))
                      ids vars vals inits))))
                ((local-syntax-form)
                 (chi-local-syntax value e er w s
                   (lambda (forms er w s)
                     (parse (let f ((forms forms))
                              (if (null? forms)
                                  (cdr body)
                                  (cons (cons er (wrap (car forms) w))
                                        (f (cdr forms)))))
                       ids vars vals inits))))
                (else ; found a non-definition
                 (return (cons (cons er (source-wrap e w s)) (cdr body))
                         ids vars vals inits)))))))))

(define do-import!
  (lambda (interface ribcage)
    (let ((token (interface-token interface)))
      (if token
          (extend-ribcage-subst! ribcage token)
          (vfor-each
            (lambda (id)
              (let ((label1 (id-var-name-loc id empty-wrap)))
                (unless label1
                  (syntax-error id "exported identifier not visible"))
                (extend-ribcage! ribcage id label1)))
            (interface-exports interface))))))

(define parse-module
  (lambda (e w s *w k)
    (define listify
      (lambda (exports)
        (if (null? exports)
            '()
            (cons (syntax-case (car exports) ()
                    ((ex ...) (listify (syntax (ex ...))))
                    (x (if (id? (syntax x))
                           (wrap (syntax x) *w)
                           (syntax-error (source-wrap e w s)
                             "invalid exports list in"))))
                  (listify (cdr exports))))))
    (define return
      (lambda (id exports forms)
        (k id (listify exports) (map (lambda (x) (wrap x *w)) forms))))
    (syntax-case e ()
      ((_ (ex ...) form ...)
       (return #f (syntax (ex ...)) (syntax (form ...))))
      ((_ mid (ex ...) form ...)
       (id? (syntax mid))
      ; id receives old wrap so it won't be confused with id of same name
      ; defined within the module
       (return (wrap (syntax mid) w) (syntax (ex ...)) (syntax (form ...))))
      (_ (syntax-error (source-wrap e w s))))))

(define parse-import
  (lambda (e w s k)
    (syntax-case e ()
      ((_ mid)
       (id? (syntax mid))
       (k (wrap (syntax mid) w)))
      (_ (syntax-error (source-wrap e w s))))))

(define parse-define
  (lambda (e w s k)
    (syntax-case e ()
      ((_ name val)
       (var? (syntax name))
       (k (syntax name) (syntax val) w))
      ((_ (name . args) e1 e2 ...)
       (and (var? (syntax name))
            (valid-bound-vars? (lambda-var-list (syntax args))))
       (k (wrap (syntax name) w)
          (cons (syntax lambda) (wrap (syntax (args e1 e2 ...)) w))
          empty-wrap))
      ((_ name)
       (var? (syntax name))
       (k (wrap (syntax name) w) (syntax (void)) empty-wrap))
      (_ (syntax-error (source-wrap e w s))))))

(define parse-define-syntax
  (lambda (e w s k)
    (syntax-case e ()
      ((_ name val)
       (id? (syntax name))
       (k (syntax name) (syntax val) w))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-lambda-clause
  (lambda (e c r w k)
    (syntax-case c ()
      (((id ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-vars? ids))
             (syntax-error e "invalid parameter list in")
             (let ((labels (gen-labels ids))
                   (new-vars (map gen-var ids)))
               (k new-vars
                  (chi-body (syntax (e1 e2 ...))
                            e
                            (extend-var-env* labels new-vars r)
                            (make-binding-wrap ids labels w)))))))
      ((ids e1 e2 ...)
       (let ((old-ids (lambda-var-list (syntax ids))))
         (if (not (valid-bound-vars? old-ids))
             (syntax-error e "invalid parameter list in")
             (let ((labels (gen-labels old-ids))
                   (new-vars (map gen-var old-ids)))
               (k (let f ((ls1 (cdr new-vars)) (ls2 (car new-vars)))
                    (if (null? ls1)
                        ls2
                        (f (cdr ls1) (cons (car ls1) ls2))))
                  (chi-body (syntax (e1 e2 ...))
                            e
                            (extend-var-env* labels new-vars r)
                            (make-binding-wrap old-ids labels w)))))))
      (_ (syntax-error e)))))

(define chi-local-syntax
  (lambda (rec? e r w s k)
    (syntax-case e ()
      ((_ ((id val) ...) e1 e2 ...)
       (let ((ids (syntax (id ...))))
         (if (not (valid-bound-ids? ids))
             (invalid-ids-error (map (lambda (x) (wrap x w)) ids)
               (source-wrap e w s)
               "keyword")
             (let ((labels (gen-labels ids)))
               (let ((new-w (make-binding-wrap ids labels w)))
                 (k (syntax (e1 e2 ...))
                    (extend-env*
                      labels
                      (let ((w (if rec? new-w w))
                            (trans-r (transformer-env r)))
                        (map (lambda (x) (make-binding 'deferred (chi x trans-r w))) (syntax (val ...))))
                      r)
                    new-w
                    s))))))
      (_ (syntax-error (source-wrap e w s))))))

(define chi-void
  (lambda ()
  	(build-data #f #!void)))

(define ellipsis?
  (lambda (x)
    (and (nonsymbol-id? x)
         (literal-id=? x (syntax (... ...))))))

;;; data

;;; strips all annotations from potentially circular reader output

(define strip-annotation
  (lambda (x parent)
    (cond
      ((and (pair? x) (not (circular? x)))
       (let ((new ((if (immutable-pair? x)
                       cons-immutable
                       cons)
                   #f #f)))
         (when parent (set-annotation-stripped! parent new))
         (set-car! new (strip-annotation (car x) #f))
         (set-cdr! new (strip-annotation (cdr x) #f))
         (if (immutable-pair? x) (seal-immutable-pair! new))
         new))
      ((annotation? x)
       (or (annotation-stripped x)
           (strip-annotation (annotation-expression x) x)))
      ((vector? x)
       (let ((new ((if (immutable-vector? x)
                       make-immutable-vector
                       make-vector)
                   (vector-length x))))
         (when parent (set-annotation-stripped! parent new))
         (let loop ((i (- (vector-length x) 1)))
           (unless (fx< i 0)
             (vector-set! new i (strip-annotation (vector-ref x i) #f))
             (loop (fx- i 1))))
         (if (immutable-vector? x) (seal-immutable-vector! new))
         new))
      ((box? x)
       (box (strip-annotation (unbox x) #f)))
      (else x))))

;;; strips syntax-objects down to top-wrap; if top-wrap is layered directly
;;; on an annotation, strips the annotation as well.

(define strip*
  (lambda (x w fn)
    (if (top-marked? w)
        (fn x)
        (let f ((x x))
          (cond
            ((syntax-object? x)
             (strip* (syntax-object-expression x) (syntax-object-wrap x) fn))
            ((and (pair? x) (not (circular? x)))
             (let ((a (f (car x))) (d (f (cdr x))))
               (if (and (eq? a (car x)) (eq? d (cdr x)))
                   x
                   (cons a d))))
            ((vector? x)
             (let ((old (vector->list x)))
                (let ((new (map f old)))
                   (if (andmap eq? old new) x (list->vector new)))))
            (else x))))))

(define strip
  (lambda (x w)
    (strip* x w (lambda (x) (strip-annotation x #f)))))

;;; lexical variables

(define gen-var
  (lambda (id)
    (let ((id (if (syntax-object? id) (syntax-object-expression id) id)))
      (if (annotation? id)
          (build-lexical-var (annotation-source id) (annotation-expression id))
          (build-lexical-var no-source id)))))

(define lambda-var-list
  (lambda (vars)
    (let lvl ((vars vars) (ls '()) (w empty-wrap))
       (cond
         ((pair? vars) (lvl (cdr vars) (cons (wrap (car vars) w) ls) w))
         ((id? vars) (cons (wrap vars w) ls))
         ((null? vars) ls)
         ((syntax-object? vars)
          (lvl (syntax-object-expression vars)
               ls
               (join-wraps w (syntax-object-wrap vars))))
         ((annotation? vars)
          (lvl (annotation-expression vars) ls w))
       ; include anything else to be caught by subsequent error
       ; checking
         (else (cons vars ls))))))


; must precede global-extends

(set! $sc-put-cte
  (lambda (id b)
    (define put-token
      (lambda (id token)
        (define cons-id
          (lambda (id x)
            (if (not x) id (cons id x))))
        (define weed
          (lambda (id x)
            (if (pair? x)
                (if (bound-id=? (car x) id) ; could just check same-marks
                    (weed id (cdr x))
                    (cons-id (car x) (weed id (cdr x))))
                (if (or (not x) (bound-id=? x id))
                    #f
                    x))))
        (let ((sym (id-sym-name id)))
          (let ((x (weed id (getprop sym token))))
            (if (and (not x) (symbol? id))
               ; don't pollute property list when all we have is a plain
               ; top-level binding, since that's what's assumed anyway
                (remprop sym token)
                (putprop sym token (cons-id id x)))))))
    (define sc-put-module
      (lambda (exports token)
        (vfor-each
          (lambda (id) (put-token id token))
          exports)))
    (define (put-cte id binding)
      (put-token id '*top*)
      (let ((sym (if (symbol? id) id (id-var-name id empty-wrap))))
        (putprop sym '*sc-expander* binding)))
    (let ((binding (or (sanitize-binding b) (error 'define-syntax "invalid transformer ~s" b))))
      (case (binding-type binding)
        ((module)
         (let ((iface (binding-value binding)))
           (sc-put-module (interface-exports iface) (interface-token iface)))
         (put-cte id binding))
        ((do-import) ; fake binding: id is module id, binding-value is import token
         (let ((token (binding-value b)))
           (let ((b (lookup (id-var-name id empty-wrap) null-env)))
             (case (binding-type b)
               ((module)
                (let ((iface (binding-value b)))
                  (unless (eq? (interface-token iface) token)
                    (syntax-error id "import mismatch for module"))
                  (sc-put-module (interface-exports iface) '*top*)))
               (else (syntax-error id "import from unknown module"))))))
        (else (put-cte id binding))))))


;;; core transformers

(global-extend 'local-syntax 'letrec-syntax #t)
(global-extend 'local-syntax 'let-syntax #f)


(global-extend 'core 'fluid-let-syntax
  (lambda (e r w s)
    (syntax-case e ()
      ((_ ((var val) ...) e1 e2 ...)
       (valid-bound-ids? (syntax (var ...)))
       (let ((names (map (lambda (x) (id-var-name x w)) (syntax (var ...)))))
         (for-each
           (lambda (id n)
             (case (binding-type (lookup n r))
               ((displaced-lexical) (displaced-lexical-error (wrap id w)))))
           (syntax (var ...))
           names)
         (chi-body
           (syntax (e1 e2 ...))
           (source-wrap e w s)
           (extend-env*
             names
             (let ((trans-r (transformer-env r)))
               (map (lambda (x) (make-binding 'deferred (chi x trans-r w))) (syntax (val ...))))
             r)
           w)))
      (_ (syntax-error (source-wrap e w s))))))

(global-extend 'core 'quote core-quote)
(global-extend 'core 'lambda core-lambda)
(global-extend 'core 'letrec core-letrec)
(global-extend 'core 'if core-if)

(global-extend 'core 'syntax
  (let ()
    (define gen-syntax
      (lambda (src e r maps ellipsis?)
        (if (id? e)
            (let ((label (id-var-name e empty-wrap)))
              (let ((b (lookup label r)))
                (if (eq? (binding-type b) 'syntax)
                    (let-values (((var maps)
                                  (let ((var.lev (binding-value b)))
                                    (gen-ref src (car var.lev) (cdr var.lev) maps))))
                      (values `(ref ,var) maps))
                    (if (ellipsis? e)
                        (syntax-error src "misplaced ellipsis in syntax form")
                        (values `(quote ,e) maps)))))
            (syntax-case e ()
              ((dots e)
               (ellipsis? (syntax dots))
               (gen-syntax src (syntax e) r maps (lambda (x) #f)))
              ((x dots . y)
               ; this could be about a dozen lines of code, except that we
               ; choose to handle (syntax (x ... ...)) forms
               (ellipsis? (syntax dots))
               (let f ((y (syntax y))
                       (k (lambda (maps)
                            (let-values (((x maps)
                                          (gen-syntax src (syntax x) r
                                            (cons '() maps) ellipsis?)))
                              (if (null? (car maps))
                                  (syntax-error src
                                    "extra ellipsis in syntax form")
                                  (values (gen-map x (car maps))
                                          (cdr maps)))))))
                 (syntax-case y ()
                   ((dots . y)
                    (ellipsis? (syntax dots))
                    (f (syntax y)
                       (lambda (maps)
                         (let-values (((x maps) (k (cons '() maps))))
                           (if (null? (car maps))
                               (syntax-error src
                                 "extra ellipsis in syntax form")
                               (values (gen-mappend x (car maps))
                                       (cdr maps)))))))
                   (_ (let-values (((y maps) (gen-syntax src y r maps ellipsis?)))
                        (let-values (((x maps) (k maps)))
                          (values (gen-append x y) maps)))))))
              ((x . y)
               (let-values (((xnew maps) (gen-syntax src (syntax x) r maps ellipsis?)))
                 (let-values (((ynew maps) (gen-syntax src (syntax y) r maps ellipsis?)))
                   (values (gen-cons e (syntax x) (syntax y) xnew ynew)
                           maps))))
              (#(x1 x2 ...)
               (let ((ls (syntax (x1 x2 ...))))
                 (let-values (((lsnew maps) (gen-syntax src ls r maps ellipsis?)))
                   (values (gen-vector e ls lsnew) maps))))
              (_ (values `(quote ,e) maps))))))

    (define gen-ref
      (lambda (src var level maps)
        (if (fx= level 0)
            (values var maps)
            (if (null? maps)
                (syntax-error src "missing ellipsis in syntax form")
                (let-values (((outer-var outer-maps) (gen-ref src var (fx- level 1) (cdr maps))))
                  (let ((b (assq outer-var (car maps))))
                    (if b
                        (values (cdr b) maps)
                        (let ((inner-var (gen-var 'tmp)))
                          (values inner-var
                                  (cons (cons (cons outer-var inner-var)
                                              (car maps))
                                        outer-maps))))))))))

    (define gen-append
      (lambda (x y)
        (if (equal? y '(quote ()))
            x
            `(append ,x ,y))))

    (define gen-mappend
      (lambda (e map-env)
        `(apply (primitive append) ,(gen-map e map-env))))

    (define gen-map
      (lambda (e map-env)
        (let ((formals (map-cdr map-env))
              (actuals (map (lambda (x) `(ref ,(car x))) map-env)))
          (cond
            ((eq? (car e) 'ref)
             ; identity map equivalence:
             ; (map (lambda (x) x) y) == y
             (car actuals))
            ((andmap
                (lambda (x) (and (eq? (car x) 'ref) (memq (cadr x) formals)))
                (cdr e))
             ; eta map equivalence:
             ; (map (lambda (x ...) (f x ...)) y ...) == (map f y ...)
             `(map (primitive ,(car e))
                   ,@(map (let ((r (map cons formals actuals)))
                            (lambda (x) (cdr (assq (cadr x) r))))
                          (cdr e))))
            (else `(map (lambda ,formals ,e) ,@actuals))))))

   ; 12/12/00: semantic change: we now return original syntax object (e)
   ; if no pattern variables were found within, to avoid dropping
   ; source annotations prematurely.  the "syntax returns lists" for
   ; lists in its input guarantee counts only for substructure that
   ; contains pattern variables
    (define gen-cons
      (lambda (e x y xnew ynew)
        (case (car ynew)
          ((quote)
           (if (eq? (car xnew) 'quote)
               (let ((xnew (cadr xnew)) (ynew (cadr ynew)))
                 (if (and (eq? xnew x) (eq? ynew y))
                     `',e
                     `'(,xnew . ,ynew)))
               (if (eq? (cadr ynew) '()) `(list ,xnew) `(cons ,xnew ,ynew))))
          ((list) `(list ,xnew ,@(cdr ynew)))
          (else `(cons ,xnew ,ynew)))))

    (define gen-vector
      (lambda (e ls lsnew)
        (cond
          ((eq? (car lsnew) 'quote)
           (if (eq? (cadr lsnew) ls)
               `',e
               `(quote #(,@(cadr lsnew)))))
          ((eq? (car lsnew) 'list) `(vector ,@(cdr lsnew)))
          (else `(list->vector ,lsnew)))))


    (define regen
      (lambda (x)
        (case (car x)
          ((ref) (build-lexical-reference 'value no-source (cadr x)))
          ((primitive) (build-primref no-source (cadr x)))
          ((quote) (build-data no-source (cadr x)))
          ((lambda) (build-lambda no-source (cadr x) (regen (caddr x))))
          ((map) (let ((ls (map regen (cdr x))))
                   (build-application no-source
                     (if (fx= (length ls) 2)
                         (build-primref no-source 'map)
                        ; really need to do our own checking here
                         (build-primref no-source 2 'map)) ; require error check
                     ls)))
          (else (build-application no-source
                  (build-primref no-source (car x))
                  (map regen (cdr x)))))))

    (lambda (e r w s)
      (let ((e (source-wrap e w s)))
        (syntax-case e ()
          ((_ x)
           (let-values (((e maps) (gen-syntax e (syntax x) r '() ellipsis?)))
             (regen e)))
          (_ (syntax-error e)))))))

(global-extend 'set! 'set! '())

(global-extend 'begin 'begin '())

(global-extend 'module-key 'module '())
(global-extend 'import 'import #f)
(global-extend 'import 'import-only #t)

(global-extend 'define 'define '())

(global-extend 'define-syntax 'define-syntax '())

(global-extend 'eval-when 'eval-when '())

(global-extend 'core 'syntax-case
  (let ()
    (define convert-pattern
      ; accepts pattern & keys
      ; returns syntax-dispatch pattern & ids
      (lambda (pattern keys)
        (define cvt*
          (lambda (p* n ids)
            (if (null? p*)
                (values '() ids)
                (let-values (((y ids) (cvt* (cdr p*) n ids)))
                  (let-values (((x ids) (cvt (car p*) n ids)))
                    (values (cons x y) ids))))))
        (define cvt
          (lambda (p n ids)
            (if (id? p)
                (if (bound-id-member? p keys)
                    (values (vector 'free-id p) ids)
                    (values 'any (cons (cons p n) ids)))
                (syntax-case p ()
                  ((x dots)
                   (ellipsis? (syntax dots))
                   (let-values (((p ids) (cvt (syntax x) (fx+ n 1) ids)))
                     (values (if (eq? p 'any) 'each-any (vector 'each p))
                             ids)))
                  ((x dots y ... . z)
                   (ellipsis? (syntax dots))
                   (let-values (((z ids) (cvt (syntax z) n ids)))
                     (let-values (((y ids) (cvt* (syntax (y ...)) n ids)))
                       (let-values (((x ids) (cvt (syntax x) (fx+ n 1) ids)))
                         (values `#4(each+ ,x ,(reverse y) ,z) ids)))))
                  ((x . y)
                   (let-values (((y ids) (cvt (syntax y) n ids)))
                     (let-values (((x ids) (cvt (syntax x) n ids)))
                       (values (cons x y) ids))))
                  (() (values '() ids))
                  (#(x ...)
                   (let-values (((p ids) (cvt (syntax (x ...)) n ids)))
                     (values (vector 'vector p) ids)))
                  (x (values (vector 'atom (strip p empty-wrap)) ids))))))
        (cvt pattern 0 '())))

    (define build-dispatch-call
      (lambda (pvars exp y r)
        (let ((ids (map-car pvars)) (levels (map-cdr pvars)))
          (let ((labels (gen-labels ids)) (new-vars (map gen-var ids)))
            (build-application no-source
              (build-primref no-source 'apply)
              (list (build-lambda no-source new-vars
                      (chi exp
                         (extend-env*
                             labels
                             (map (lambda (var level)
                                    (make-binding 'syntax `(,var . ,level)))
                                  new-vars
                                  (map-cdr pvars))
                             r)
                           (make-binding-wrap ids labels empty-wrap)))
                    y))))))

    (define gen-clause
      (lambda (x keys clauses r pat fender exp)
        (let-values (((p pvars) (convert-pattern pat keys)))
          (cond
            ((not (distinct-bound-ids? (map-car pvars)))
             (invalid-ids-error (map-car pvars) pat "pattern variable"))
            ((not (andmap (lambda (x) (not (ellipsis? (car x)))) pvars))
             (syntax-error pat
               "misplaced ellipsis in syntax-case pattern"))
            (else
             (let ((y (gen-var 'tmp)))
               ; fat finger binding and references to temp variable y
               (build-application no-source
                 (build-lambda no-source (list y)
                   (let-syntax ((y (identifier-syntax
                                     (build-lexical-reference 'value no-source y))))
                     (build-conditional no-source
                       (syntax-case fender ()
                         (#t y)
                         (_ (build-conditional no-source
                              y
                              (build-dispatch-call pvars fender y r)
                              (build-data no-source #f))))
                       (build-dispatch-call pvars exp y r)
                       (gen-syntax-case x keys clauses r))))
                 (list (if (eq? p 'any)
                           (build-application no-source
                             (build-primref no-source 'list)
                             (list (build-lexical-reference no-source 'value x)))
                           (build-application no-source
                             (build-primref no-source '$syntax-dispatch)
                             (list (build-lexical-reference no-source 'value x)
                                   (build-data no-source p))))))))))))

    (define gen-syntax-case
      (lambda (x keys clauses r)
        (if (null? clauses)
            (build-application no-source
              (build-primref no-source 'syntax-error)
              (list (build-lexical-reference 'value no-source x)))
            (syntax-case (car clauses) ()
              ((pat exp)
               (if (and (id? (syntax pat))
                        (not (bound-id-member? (syntax pat) keys))
                        (not (ellipsis? (syntax pat))))
                   (let ((label (gen-label))
                         (var (gen-var (syntax pat))))
                     (build-application no-source
                       (build-lambda no-source (list var)
                         (chi (syntax exp)
                              (extend-env label (make-binding 'syntax `(,var . 0)) r)
                              (make-binding-wrap (syntax (pat))
                                (list label) empty-wrap)))
                       (list (build-lexical-reference 'value no-source x))))
                   (gen-clause x keys (cdr clauses) r
                     (syntax pat) #t (syntax exp))))
              ((pat fender exp)
               (gen-clause x keys (cdr clauses) r
                 (syntax pat) (syntax fender) (syntax exp)))
              (_ (syntax-error (car clauses) "invalid syntax-case clause"))))))

    (lambda (e r w s)
      (let ((e (source-wrap e w s)))
        (syntax-case e ()
          ((_ val (key ...) m ...)
           (if (andmap (lambda (x) (and (id? x) (not (ellipsis? x))))
                       (syntax (key ...)))
               (let ((x (gen-var 'tmp)))
                 ; fat finger binding and references to temp variable x
                 (build-application s
                   (build-lambda no-source (list x)
                     (gen-syntax-case x
                       (syntax (key ...)) (syntax (m ...))
                       r))
                   (list (chi (syntax val) r empty-wrap))))
               (syntax-error e "invalid literals list in"))))))))

;;; To support eval-when, we maintain two mode sets:
;;;
;;; ctem (compile-time-expression mode)
;;;   determines whether/when to evaluate compile-time expressions such
;;;   as macro definitions, module definitions, and compile-time
;;;   registration of variable definitions
;;;
;;; rtem (run-time-expression mode)
;;;   determines whether/when to evaluate run-time expressions such
;;;   as the actual assignment performed by a variable definition or
;;;   arbitrary top-level expressions

;;; Possible modes in the mode set are:
;;;
;;; L (load): evaluate at load time.  implies V for compile-time
;;;     expressions and R for run-time expressions.
;;;
;;; C (compile): evaluate at compile (file) time
;;;
;;; E (eval): evaluate at evaluation (compile or interpret) time
;;;
;;; V (visit): evaluate at visit time
;;;
;;; R (revisit): evaluate at revisit time

;;; The mode set for the body of an eval-when is determined by
;;; translating each mode in the old mode set based on the situations
;;; present in the eval-when form and combining these into a set,
;;; using the following table.  See also update-mode-set.

;;;      load  compile  visit  revisit  eval
;;;
;;; L     L      C        V       R      -
;;;
;;; C     -      -        -       -      C
;;;
;;; V     V      C        V       -      -
;;;
;;; R     R      C        -       R      -
;;;
;;; E     -      -        -       -      E

;;; When we complete the expansion of a compile or run-time expression,
;;; the current ctem or rtem determines how the expression will be
;;; treated.  See ct-eval/residualize and rt-eval/residualize.

;;; Initial mode sets
;;;
;;; when compiling a file:
;;;
;;;     initial ctem: (L C)
;;;
;;;     initial rtem: (L)
;;;
;;; when not compiling a file:
;;;
;;;     initial ctem: (E)
;;;
;;;     initial rtem: (E)
;;;
;;;
;;; This means that top-level syntactic definitions are evaluated
;;; immediately after they are expanded, and the expanded definitions
;;; are also residualized into the object file if we are compiling
;;; a file.

(set! sc-expand
  (let ((ctem '(L C)) (rtem '(L))
        (user-ribcage
         (let ((ribcage (make-empty-ribcage)))
           (extend-ribcage-subst! ribcage '*top*)
           ribcage)))
    (let ((user-top-wrap
           (make-wrap (wrap-marks top-wrap)
             (cons user-ribcage (wrap-subst top-wrap)))))
      (lambda (x . c/rtem)
        (cond ((and (pair? x) (equal? (car x) noexpand))
               (cadr x))
              ((null? c/rtem)
               (chi-top x null-env user-top-wrap ctem rtem user-ribcage))
              (else
                (chi-top x null-env user-top-wrap (car c/rtem) (cadr c/rtem)
                         user-ribcage)))))))

(set! identifier?
  (lambda (x)
    (nonsymbol-id? x)))

(set! datum->syntax-object
  (lambda (id datum)
    (arg-check nonsymbol-id? id 'datum->syntax-object)
    (make-syntax-object datum (syntax-object-wrap id))))

(set! syntax-object->datum
  ; accepts any object, since syntax objects may consist partially
  ; or entirely of unwrapped, nonsymbolic data
  (lambda (x)
    (strip x empty-wrap)))

(set! generate-temporaries
  (lambda (ls)
    (arg-check list? ls 'generate-temporaries)
    (map (lambda (x) (wrap (gensym-hook) top-wrap)) ls)))

(set! free-identifier=?
   (lambda (x y)
      (arg-check nonsymbol-id? x 'free-identifier=?)
      (arg-check nonsymbol-id? y 'free-identifier=?)
      (free-id=? x y)))

(set! bound-identifier=?
   (lambda (x y)
      (arg-check nonsymbol-id? x 'bound-identifier=?)
      (arg-check nonsymbol-id? y 'bound-identifier=?)
      (bound-id=? x y)))

(set! literal-identifier=?
  (lambda (x y)
    (arg-check nonsymbol-id? x 'literal-identifier=?)
    (arg-check nonsymbol-id? y 'literal-identifier=?)
    (literal-id=? x y)))

(set! syntax-error
  (lambda (object . messages)
    (for-each (lambda (x) (arg-check string? x 'syntax-error)) messages)
    (let ((message (if (null? messages)
                       "invalid syntax"
                       (apply string-append messages))))
      (error-hook #f message (strip object empty-wrap)))))

;;; syntax-dispatch expects an expression and a pattern.  If the expression
;;; matches the pattern a list of the matching expressions for each
;;; "any" is returned.  Otherwise, #f is returned.  (This use of #f will
;;; not work on r4rs implementations that violate the ieee requirement
;;; that #f and () be distinct.)

;;; The expression is matched with the pattern as follows:

;;; p in pattern:                        matches:
;;;   ()                                 empty list
;;;   any                                anything
;;;   (p1 . p2)                          pair (list)
;;;   #(free-id <key>)                   <key> with literal-identifier=?
;;;   each-any                           any proper list
;;;   #(each p)                          (p*)
;;;   #(each+ p1 (p2_1 ...p2_n) p3)      (p1* (p2_n ... p2_1) . p3)
;;;   #(vector p)                        (list->vector p)
;;;   #(atom <object>)                   <object> with "equal?"

;;; Vector cops out to pair under assumption that vectors are rare.  If
;;; not, should convert to:
;;;   #(vector p)                        #(p*)

(let ()

(define match-each
  (lambda (e p w)
    (cond
      ((annotation? e)
       (match-each (annotation-expression e) p w))
      ((pair? e)
       (let ((first (match (car e) p w '())))
         (and first
              (let ((rest (match-each (cdr e) p w)))
                 (and rest (cons first rest))))))
      ((null? e) '())
      ((syntax-object? e)
       (match-each (syntax-object-expression e)
                   p
                   (join-wraps w (syntax-object-wrap e))))
      (else #f))))

(define match-each+
  (lambda (e x-pat y-pat z-pat w r)
    (let f ((e e) (w w))
      (cond
        ((pair? e)
         (let-values (((xr* y-pat r) (f (cdr e) w)))
           (if r
               (if (null? y-pat)
                   (let ((xr (match (car e) x-pat w '())))
                     (if xr
                         (values (cons xr xr*) y-pat r)
                         (values #f #f #f)))
                   (values '() (cdr y-pat) (match (car e) (car y-pat) w r)))
               (values #f #f #f))))
        ((annotation? e) (f (annotation-expression e) w))
        ((syntax-object? e) (f (syntax-object-expression e)
                               (join-wraps w (syntax-object-wrap e))))
        (else (values '() y-pat (match e z-pat w r)))))))

(define match-each-any
  (lambda (e w)
    (cond
      ((annotation? e)
       (match-each-any (annotation-expression e) w))
      ((pair? e)
       (let ((l (match-each-any (cdr e) w)))
         (and l (cons (wrap (car e) w) l))))
      ((null? e) '())
      ((syntax-object? e)
       (match-each-any (syntax-object-expression e)
                       (join-wraps w (syntax-object-wrap e))))
      (else #f))))

(define match-empty
  (lambda (p r)
    (cond
      ((null? p) r)
      ((eq? p 'any) (cons '() r))
      ((pair? p) (match-empty (car p) (match-empty (cdr p) r)))
      ((eq? p 'each-any) (cons '() r))
      (else
       (case (vector-ref p 0)
         ((each) (match-empty (vector-ref p 1) r))
         ((each+) (match-empty (vector-ref p 1)
                    (match-empty (reverse (vector-ref p 2))
                      (match-empty (vector-ref p 3) r))))
         ((free-id atom) r)
         ((vector) (match-empty (vector-ref p 1) r)))))))

(define combine
  (lambda (r* r)
    (if (null? (car r*))
        r
        (cons (map-car r*) (combine (map-cdr r*) r)))))

(define match*
  (lambda (e p w r)
    (cond
      ((null? p) (and (null? e) r))
      ((pair? p)
       (and (pair? e) (match (car e) (car p) w
                        (match (cdr e) (cdr p) w r))))
      ((eq? p 'each-any)
       (let ((l (match-each-any e w))) (and l (cons l r))))
      (else
       (case (vector-ref p 0)
         ((each)
          (if (null? e)
              (match-empty (vector-ref p 1) r)
              (let ((r* (match-each e (vector-ref p 1) w)))
                (and r* (combine r* r)))))
         ((free-id) (and (id? e) (literal-id=? (wrap e w) (vector-ref p 1)) r))
         ((each+)
          (let-values (((xr* y-pat r)
                        (match-each+ e (vector-ref p 1) (vector-ref p 2)
                          (vector-ref p 3) w r)))
            (and r (null? y-pat)
              (if (null? xr*)
                  (match-empty (vector-ref p 1) r)
                  (combine xr* r)))))
         ((atom) (and (equal? (vector-ref p 1) (strip e w)) r))
         ((vector)
          (and (vector? e)
               (match (vector->list e) (vector-ref p 1) w r))))))))

(define match
  (lambda (e p w r)
    (cond
      ((not r) #f)
      ((eq? p 'any) (cons (wrap e w) r))
      ((syntax-object? e)
       (match*
         (unannotate (syntax-object-expression e))
         p
         (join-wraps w (syntax-object-wrap e))
         r))
      (else (match* (unannotate e) p w r)))))

(set! $syntax-dispatch
  (lambda (e p)
    (cond
      ((eq? p 'any) (list e))
      ((syntax-object? e)
       (match* (unannotate (syntax-object-expression e))
         p (syntax-object-wrap e) '()))
      (else (match* (unannotate e) p empty-wrap '())))))
))


(define-syntax with-syntax
   (lambda (x)
      (syntax-case x ()
         ((_ () e1 e2 ...)
          (syntax (begin e1 e2 ...)))
         ((_ ((out in)) e1 e2 ...)
          (syntax (syntax-case in () (out (begin e1 e2 ...)))))
         ((_ ((out in) ...) e1 e2 ...)
          (syntax (syntax-case (list in ...) ()
                     ((out ...) (begin e1 e2 ...))))))))

(define-syntax syntax-rules
  (lambda (x)
    (syntax-case x ()
      ((_ (k ...) ((keyword . pattern) template) ...)
       (syntax (lambda (x)
                (syntax-case x (k ...)
                  ((dummy . pattern) (syntax template))
                  ...)))))))

(define-syntax or
   (lambda (x)
      (syntax-case x ()
         ((_) (syntax #f))
         ((_ e) (syntax e))
         ((_ e1 e2 e3 ...)
          (syntax (let ((t e1)) (if t t (or e2 e3 ...))))))))

(define-syntax and
   (lambda (x)
      (syntax-case x ()
         ((_ e1 e2 e3 ...) (syntax (if e1 (and e2 e3 ...) #f)))
         ((_ e) (syntax e))
         ((_) (syntax #t)))))

(define-syntax let
   (lambda (x)
      (syntax-case x ()
         ((_ ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (x ...)))
          (syntax ((lambda (x ...) e1 e2 ...) v ...)))
         ((_ f ((x v) ...) e1 e2 ...)
          (andmap identifier? (syntax (f x ...)))
          (syntax ((letrec ((f (lambda (x ...) e1 e2 ...))) f)
                    v ...))))))

(define-syntax let*
  (lambda (x)
    (syntax-case x ()
      ((let* ((x v) ...) e1 e2 ...)
       (andmap identifier? (syntax (x ...)))
       (let f ((bindings (syntax ((x v)  ...))))
         (if (null? bindings)
             (syntax (let () e1 e2 ...))
             (with-syntax ((body (f (cdr bindings)))
                           (binding (car bindings)))
               (syntax (let (binding) body)))))))))

(define-syntax cond
  (lambda (x)
    (syntax-case x ()
      ((_ m1 m2 ...)
       (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
         (if (null? clauses)
             (syntax-case clause (else =>)
               ((else e1 e2 ...) (syntax (if '#t (begin e1 e2 ...))))
               ((e0) (syntax (let ((t e0)) (if t t))))
               ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t)))))
               ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...))))
               (_ (syntax-error x)))
             (with-syntax ((rest (f (car clauses) (cdr clauses))))
               (syntax-case clause (else =>)
                 ((e0) (syntax (let ((t e0)) (if t t rest))))
                 ((e0 => e1) (syntax (let ((t e0)) (if t (e1 t) rest))))
                 ((e0 e1 e2 ...) (syntax (if e0 (begin e1 e2 ...) rest)))
                 (_ (syntax-error x))))))))))

(define-syntax do
   (lambda (orig-x)
      (syntax-case orig-x ()
         ((_ ((var init . step) ...) (e0 e1 ...) c ...)
          (with-syntax (((step ...)
                         (map (lambda (v s)
                                 (syntax-case s ()
                                    (() v)
                                    ((e) (syntax e))
                                    (_ (syntax-error orig-x))))
                              (syntax (var ...))
                              (syntax (step ...)))))
             (syntax-case (syntax (e1 ...)) ()
                (() (syntax (let doloop ((var init) ...)
                               (if e0 #!void
                                 (begin c ... (doloop step ...))))))
                ((e1 e2 ...)
                 (syntax (let doloop ((var init) ...)
                            (if e0
                                (begin e1 e2 ...)
                                (begin c ... (doloop step ...))))))))))))

(define-syntax quasiquote
   (letrec
     ; these are here because syntax-case uses literal-identifier=?,
     ; and we want the more precise free-identifier=?
      ((isquote? (lambda (x)
                   (and (identifier? x)
                        (free-identifier=? x (syntax quote)))))
       (islist? (lambda (x)
                  (and (identifier? x)
                       (free-identifier=? x (syntax list)))))
       (iscons? (lambda (x)
                  (and (identifier? x)
                       (free-identifier=? x (syntax cons)))))
       (quote-nil? (lambda (x)
                    (syntax-case x ()
                      ((quote? ()) (isquote? (syntax quote?)))
                      (_ #f))))
       (quasilist*
        (lambda (x y)
          (let f ((x x))
            (if (null? x)
                y
                (quasicons (car x) (f (cdr x)) #f)))))
       (quasicons
        (lambda (x y p)
          (with-syntax ((x x) (y y) (ps p))
            (syntax-case (syntax y) ()
              ((quote? dy)
               (isquote? (syntax quote?))
               (syntax-case (syntax x) ()
                 ((quote? dx)
                  (isquote? (syntax quote?))
                  ; If the output is equal to the input (ie no quasi transforms)
                  ; then output the input, to preserve pointer equality and
                  ; immutability.
                  (let ([dxdy (syntax (quote (dx . dy)))])
                    (let ([dxdyd (syntax-object->datum dxdy)]
                          [pd `(quote ,(syntax-object->datum p))])
                      (if (equal? pd dxdyd)
                          (syntax (quote ps))
                          dxdy))))
                 (_ (if (null? (syntax dy))
                        (syntax (list x))
                        (syntax (cons x y))))))
              ((listp . stuff)
               (islist? (syntax listp))
               (syntax (list x . stuff)))
              (else (syntax (cons x y)))))))
       (quasiappend
        (lambda (x y)
          (let ((ls (let f ((x x))
                      (if (null? x)
                          (if (quote-nil? y)
                              '()
                              (list y))
                          (if (quote-nil? (car x))
                              (f (cdr x))
                              (cons (car x) (f (cdr x))))))))
            (cond
              ((null? ls) (syntax (quote ())))
              ((null? (cdr ls)) (car ls))
              (else (with-syntax (((p ...) ls))
                      (syntax (append p ...))))))))
       (quasivector
        (lambda (x)
          (with-syntax ((pat-x x))
            (syntax-case (syntax pat-x) ()
              ((quote? (x ...))
               (isquote? (syntax quote?))
               (syntax (quote #(x ...))))
              (_ (let f ((x x) (k (lambda (ls) `(,(syntax vector) ,@ls))))
                   (syntax-case x ()
                     ((quote? (x ...))
                      (isquote? (syntax quote?))
                      (k (syntax ((quote x) ...))))
                     ((listp x ...)
                      (islist? (syntax listp))
                      (k (syntax (x ...))))
                     ((cons? x y)
                      (iscons? (syntax cons?))
                      (f (syntax y) (lambda (ls) (k (cons (syntax x) ls)))))
                     (else
                      (syntax (list->vector pat-x))))))))))
       (vquasi
        (lambda (p lev)
          (syntax-case p ()
            ((p . q)
             (syntax-case #'p (unquote unquote-splicing)
               ((unquote p ...)
                (if (= lev 0)
                    (quasilist* (syntax (p ...)) (vquasi (syntax q) lev))
                    (quasicons (quasicons (syntax (quote unquote))
                                          (quasi (syntax (p ...)) (- lev 1))
                                          #f)
                               (vquasi (syntax q) lev)
			       #f)))
               ((unquote-splicing p ...)
                (if (= lev 0)
                    (quasiappend (syntax (p ...)) (vquasi (syntax q) lev))
                    (quasicons (quasicons (syntax (quote unquote-splicing))
                                          (quasi (syntax (p ...)) (- lev 1))
                                          #f)
                               (vquasi (syntax q) lev)
                               #f)))
               (p (quasicons (quasi (syntax p) lev) 
                             (vquasi (syntax q) lev) 
                             #f))))
            (() (syntax (quote ()))))))
       (quasi
        (lambda (p lev)
           (syntax-case p (unquote unquote-splicing quasiquote)
              ((unquote p)
               (if (= lev 0)
                   (syntax p)
                   (quasicons (syntax (quote unquote))
                              (quasi (syntax (p)) (- lev 1)) #f)))
              (((unquote p ...) . q)
               (if (= lev 0)
                   (quasilist* (syntax (p ...)) (quasi (syntax q) lev))
                   (quasicons (quasicons (syntax (quote unquote))
                                         (quasi (syntax (p ...)) (- lev 1))
                                         #f)
                              (quasi (syntax q) lev) #f)))
              (((unquote-splicing p ...) . q)
               (if (= lev 0)
                   (quasiappend (syntax (p ...)) (quasi (syntax q) lev))
                   (quasicons (quasicons (syntax (quote unquote-splicing))
                                         (quasi (syntax (p ...)) (- lev 1))
                                         #f)
                              (quasi (syntax q) lev) #f)))
              ((quasiquote p)
               (quasicons (syntax (quote quasiquote))
                          (quasi (syntax (p)) (+ lev 1)) 
                          #f))
              ((q . r)
               (quasicons (quasi (syntax q) lev) (quasi (syntax r) lev) p))
              (#(x ...) (quasivector (vquasi (syntax (x ...)) lev)))
              (p (syntax (quote p)))))))
    (lambda (x)
       (syntax-case x ()
          ((_ e) (quasi (syntax e) 0))))))

(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ((p (open-source-input-file fn)))
          (let f ((acc '()))
            (let ((x (read-code p)))
              (if (eof-object? x)
                  (begin (close-input-port p) (reverse acc))
                  (f (cons (datum->syntax-object k x) acc))))))))
    (syntax-case x ()
      ((k filename)
       (let ((fn (syntax-object->datum (syntax filename))))
         (with-syntax (((exp ...) (read-file fn (syntax k))))
           (syntax (begin exp ...))))))))

(define-syntax unquote
  (lambda (x)
    (syntax-case x ()
      ((_ e ...)
       (syntax-error x
         "expression not valid outside of quasiquote")))))

(define-syntax unquote-splicing
  (lambda (x)
    (syntax-case x ()
      ((_ e ...)
       (syntax-error x
         "expression not valid outside of quasiquote")))))

(define-syntax case
  (lambda (x)
    (syntax-case x ()
      ((_ e m1 m2 ...)
       (with-syntax
         ((body (let f ((clause (syntax m1)) (clauses (syntax (m2 ...))))
                  (if (null? clauses)
                      (syntax-case clause (else)
                        ((else e1 e2 ...) (syntax (if '#t (begin e1 e2 ...))))
                        (((k) e1 e2 ...)
                         (syntax (if (eqv? t 'k) (begin e1 e2 ...))))
                        (((k ...) e1 e2 ...)
                         (syntax (if (memv t '(k ...)) (begin e1 e2 ...))))
                        (_ (syntax-error x)))
                      (with-syntax ((rest (f (car clauses) (cdr clauses))))
                        (syntax-case clause (else)
                          (((k) e1 e2 ...)
                           (syntax (if (eqv? t 'k)
                                       (begin e1 e2 ...)
                                       rest)))
                          (((k ...) e1 e2 ...)
                           (syntax (if (memv t '(k ...))
                                       (begin e1 e2 ...)
                                       rest)))
                          (_ (syntax-error x))))))))
         (syntax (let ((t e)) body)))))))

(define-syntax identifier-syntax
  (lambda (x)
    (syntax-case x (set!)
      ((_ e)
       (syntax
         (lambda (x)
           (syntax-case x ()
             (id
              (identifier? (syntax id))
              (syntax e))
             ((_ x (... ...))
              (syntax (e x (... ...))))))))
      ((_ (id exp1) ((set! var val) exp2))
       (and (identifier? (syntax id)) (identifier? (syntax var)))
       (syntax
         (cons 'macro!
           (lambda (x)
             (syntax-case x (set!)
               ((set! var val) (syntax exp2))
               ((id x (... ...)) (syntax (exp1 x (... ...))))
               (id (identifier? (syntax id)) (syntax exp1))))))))))

