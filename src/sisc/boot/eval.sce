(#%program
  ((|%%_7w-V8lgoK_x| . 1) (_make-parameter . 1))
  ()
  (_make-parameter)
  (#%define current-optimizer
    (_make-parameter
      (#%lambda #t
        (|%%_7w-V8lgoK_x|)
        ()
        |%%_7w-V8lgoK_x|))))
(#%program
  ((_with-environment . 1))
  ()
  (_with-environment)
  (#%define with-environment _with-environment))
(#%program
  ((compile . 1)
   (|%%_7wowWvjoK_compile| . 1)
   (|%%_7w2AY2joK_analyze| . 1)
   (|%%_7wID-BioK_optimize| . 1)
   (|%%_7wmH09ioK_expand| . 1)
   (cons . 4)
   (list . 1)
   (|%%_7wMhOjloK_expr| . 1)
   (|%%_7wkS6OgoK_old-compile| . 1)
   (|%%_7wqlQSkoK_expr| . 1)
   (_analyze! . 1)
   (|%%_7w4pSpkoK_expr| . 1)
   (current-optimizer . 1)
   (|%%_7wGO4fhoK_flags| . 1)
   (|%%_7wKsUYjoK_expr| . 1)
   (sc-expand . 1)
   (apply . 1)
   (|%%_7w0L2IhoK_env| . 3)
   (with-environment . 1))
  ((|%%_7wowWvjoK_compile| . 1)
   (|%%_7w2AY2joK_analyze| . 1)
   (|%%_7wID-BioK_optimize| . 1)
   (|%%_7wmH09ioK_expand| . 1))
  (compile
    list
    cons
    _analyze!
    current-optimizer
    sc-expand
    apply
    with-environment)
  (#%define compilation-phases
    ((#%lambda #t
       (|%%_7wkS6OgoK_old-compile|)
       ()
       (#%lambda #t
         (|%%_7wGO4fhoK_flags| |%%_7w0L2IhoK_env|)
         (|%%_7wkS6OgoK_old-compile|)
         (#%letrec #t
           ((|%%_7wmH09ioK_expand|
              (#%lambda #t
                (|%%_7wKsUYjoK_expr|)
                (|%%_7w0L2IhoK_env| |%%_7wGO4fhoK_flags|)
                (with-environment
                  |%%_7w0L2IhoK_env|
                  (#%lambda #t
                    ()
                    (|%%_7wKsUYjoK_expr| |%%_7wGO4fhoK_flags|)
                    (apply sc-expand
                           |%%_7wKsUYjoK_expr|
                           |%%_7wGO4fhoK_flags|)))))
            (|%%_7wID-BioK_optimize|
              (#%lambda #t
                (|%%_7w4pSpkoK_expr|)
                ()
                ((current-optimizer) |%%_7w4pSpkoK_expr|)))
            (|%%_7w2AY2joK_analyze|
              (#%lambda #t
                (|%%_7wqlQSkoK_expr|)
                (|%%_7w0L2IhoK_env|)
                (_analyze!
                  |%%_7wqlQSkoK_expr|
                  |%%_7w0L2IhoK_env|)))
            (|%%_7wowWvjoK_compile|
              (#%lambda #t
                (|%%_7wMhOjloK_expr|)
                (|%%_7w0L2IhoK_env| |%%_7wkS6OgoK_old-compile|)
                (|%%_7wkS6OgoK_old-compile|
                  |%%_7wMhOjloK_expr|
                  |%%_7w0L2IhoK_env|))))
           (|%%_7w0L2IhoK_env|
             |%%_7wGO4fhoK_flags|
             |%%_7wkS6OgoK_old-compile|)
           (list (cons (#%quote expand) |%%_7wmH09ioK_expand|)
                 (cons (#%quote optimize) |%%_7wID-BioK_optimize|)
                 (cons (#%quote analyze) |%%_7w2AY2joK_analyze|)
                 (cons (#%quote compile) |%%_7wowWvjoK_compile|)))))
     compile)))
(#%program
  ((interaction-environment . 1)
   (|%%_7w6eMMloK_env| . 2)
   (|%%_7w83G7noK_expr| . 1)
   (|%%_7waUzuooK_env| . 1)
   (|%%_7wsaKdmoK_flags| . 1)
   (compilation-phases . 1)
   (|%%_7wO6IGmoK_start-phase| . 1)
   (|%%_7wQXB1ooK_select-phases| . 2)
   (map . 1)
   (|%%_7weyncroK_start-phase| . 2)
   (|%%_7wAulFroK_phases| . 3)
   (caar . 1)
   (eq? . 1)
   (cdr . 3)
   (|%%_7wu_DAnoK_compose| . 2)
   (apply . 2)
   (car . 2)
   (|%%_7wyFriqoK_x| . 1)
   (|%%_7wSMvopoK_fn| . 1)
   (|%%_7wcJtRpoK_tail| . 1)
   (|%%_7wUBpLqoK_x| . 1)
   (|%%_7wwQxXooK_fs| . 3)
   (null? . 2))
  ((|%%_7wQXB1ooK_select-phases| . 1)
   (|%%_7wu_DAnoK_compose| . 1))
  (null? apply
         cdr
         car
         caar
         eq?
         map
         interaction-environment
         compilation-phases)
  (#%define compile-with-flags
    (#%lambda #t
      (|%%_7w83G7noK_expr|
        |%%_7wO6IGmoK_start-phase|
        |%%_7wsaKdmoK_flags|
        .
        |%%_7w6eMMloK_env|)
      ()
      (#%letrec #t
        ((|%%_7wu_DAnoK_compose|
           (#%lambda #t
             |%%_7wwQxXooK_fs|
             (|%%_7wu_DAnoK_compose|)
             (#%if (null? |%%_7wwQxXooK_fs|)
               (#%lambda #t
                 (|%%_7wUBpLqoK_x|)
                 ()
                 |%%_7wUBpLqoK_x|)
               ((#%lambda #t
                  (|%%_7wSMvopoK_fn| |%%_7wcJtRpoK_tail|)
                  ()
                  (#%lambda #t
                    (|%%_7wyFriqoK_x|)
                    (|%%_7wcJtRpoK_tail| |%%_7wSMvopoK_fn|)
                    (|%%_7wcJtRpoK_tail|
                      (|%%_7wSMvopoK_fn| |%%_7wyFriqoK_x|))))
                (car |%%_7wwQxXooK_fs|)
                (apply |%%_7wu_DAnoK_compose|
                       (cdr |%%_7wwQxXooK_fs|))))))
         (|%%_7wQXB1ooK_select-phases|
           (#%lambda #t
             (|%%_7weyncroK_start-phase|
               |%%_7wAulFroK_phases|)
             (|%%_7wQXB1ooK_select-phases|)
             (#%if (eq? (caar |%%_7wAulFroK_phases|)
                        |%%_7weyncroK_start-phase|)
               (map cdr |%%_7wAulFroK_phases|)
               (|%%_7wQXB1ooK_select-phases|
                 |%%_7weyncroK_start-phase|
                 (cdr |%%_7wAulFroK_phases|))))))
        (|%%_7w6eMMloK_env|
          |%%_7wsaKdmoK_flags|
          |%%_7wO6IGmoK_start-phase|
          |%%_7w83G7noK_expr|)
        ((#%lambda #t
           (|%%_7waUzuooK_env|)
           (|%%_7wQXB1ooK_select-phases|
             |%%_7wu_DAnoK_compose|
             |%%_7wsaKdmoK_flags|
             |%%_7wO6IGmoK_start-phase|
             |%%_7w83G7noK_expr|)
           ((apply |%%_7wu_DAnoK_compose|
                   (|%%_7wQXB1ooK_select-phases|
                     |%%_7wO6IGmoK_start-phase|
                     (compilation-phases
                       |%%_7wsaKdmoK_flags|
                       |%%_7waUzuooK_env|)))
            |%%_7w83G7noK_expr|))
         (#%if (null? |%%_7w6eMMloK_env|)
           (interaction-environment)
           (car |%%_7w6eMMloK_env|)))))))
(#%program
  ((|%%_7wWqj6soK_env| . 1)
   (|%%_7wgnhzsoK_expr| . 1)
   (compile-with-flags . 1)
   (apply . 1)
   (compile . 1))
  ((compile . 1))
  (compile-with-flags apply)
  (#%set! compile
    (#%lambda #t
      (|%%_7wgnhzsoK_expr| . |%%_7wWqj6soK_env|)
      ()
      (apply compile-with-flags
             |%%_7wgnhzsoK_expr|
             (#%quote expand)
             (#%quote ((l) (l)))
             |%%_7wWqj6soK_env|))))
(#%program
  ((compile-with-flags . 1)
   (apply . 1)
   (with-environment . 1)
   (|%%_7wE89nuoK_compiled-expr| . 2)
   (error . 1)
   (strict-r5rs-compliance . 1)
   (|%%_7wCjf0toK_env| . 4)
   (null? . 2)
   (cadr . 2)
   (|%%_7wicbWtoK_phase| . 3)
   (car . 3)
   (equal? . 2)
   (|%%_7wYfdttoK_x| . 9)
   (pair? . 2))
  ((|%%_7wYfdttoK_x| . 2)
   (|%%_7wicbWtoK_phase| . 2))
  (error null?
         strict-r5rs-compliance
         cadr
         pair?
         car
         equal?
         apply
         compile-with-flags
         with-environment)
  (#%define eval
    (#%lambda #t
      (|%%_7wYfdttoK_x| . |%%_7wCjf0toK_env|)
      ()
      ((#%lambda #t
         (|%%_7wicbWtoK_phase|)
         (|%%_7wCjf0toK_env| |%%_7wYfdttoK_x|)
         (#%begin
           (#%if (#%if (pair? |%%_7wYfdttoK_x|)
                   (equal? (car |%%_7wYfdttoK_x|) "noexpand")
                   #f)
             (#%begin
               (#%set! |%%_7wicbWtoK_phase| (#%quote compile))
               (#%set! |%%_7wYfdttoK_x| (cadr |%%_7wYfdttoK_x|)))
             (#%if (#%if (pair? |%%_7wYfdttoK_x|)
                     (equal? (car |%%_7wYfdttoK_x|) "analyzeonly")
                     #f)
               (#%begin
                 (#%set! |%%_7wicbWtoK_phase| (#%quote analyze))
                 (#%set! |%%_7wYfdttoK_x| (cadr |%%_7wYfdttoK_x|)))
               (#%if (#%if (null? |%%_7wCjf0toK_env|)
                       (strict-r5rs-compliance)
                       #f)
                 (error (#%quote eval)
                        "expected 2 arguments to procedure, got 1.")
                 #!void)))
           ((#%lambda #t
              (|%%_7wE89nuoK_compiled-expr|)
              (|%%_7wCjf0toK_env|)
              (#%if (null? |%%_7wCjf0toK_env|)
                (|%%_7wE89nuoK_compiled-expr|)
                (with-environment
                  (car |%%_7wCjf0toK_env|)
                  |%%_7wE89nuoK_compiled-expr|)))
            (apply compile-with-flags
                   |%%_7wYfdttoK_x|
                   |%%_7wicbWtoK_phase|
                   (#%quote ((e) (e)))
                   |%%_7wCjf0toK_env|))))
       (#%quote expand)))))
