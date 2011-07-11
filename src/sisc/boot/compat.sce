(#%program
  ((|%%_LBVS2hoQS_rest| . 2)
   (map . 2)
   (|%%_LBzW4QnQS_first| . 2)
   (|%%_LBhEW4qQS_rest| . 1)
   (|%%_LBd-6nnQS_andmap| . 2)
   (|%%_LBBL-apQS_xr| . 2)
   (|%%_LBfP0KoQS_x| . 2)
   (cons . 2)
   (apply . 2)
   (|%%_LBXHYDpQS_first| . 2)
   (|%%_LBb9d0mQS_first| . 3)
   (cdr . 3)
   (|%%_LBZwS-qQS_first| . 2)
   (car . 3)
   (|%%_LBDAUxqQS_andmap| . 2)
   (|%%_LBjtQrrQS_x| . 2)
   (|%%_LBx5btmQS_f| . 4)
   (|%%_LBFpOUrQS_first| . 2)
   (|%%_LBRcfzlQS_rest| . 2)
   (null? . 4)
   (|%%_LBT19WmQS_t| . 2))
  ((|%%_LBd-6nnQS_andmap| . 1)
   (|%%_LBDAUxqQS_andmap| . 1))
  (map cons apply cdr car null?)
  (#%define andmap
    (#%lambda #t
      (|%%_LBx5btmQS_f|
        |%%_LBb9d0mQS_first|
        .
        |%%_LBRcfzlQS_rest|)
      ()
      ((#%lambda #t
         (|%%_LBT19WmQS_t|)
         (|%%_LBRcfzlQS_rest|
           |%%_LBb9d0mQS_first|
           |%%_LBx5btmQS_f|)
         (#%if |%%_LBT19WmQS_t|
           |%%_LBT19WmQS_t|
           (#%if (null? |%%_LBRcfzlQS_rest|)
             ((#%letrec #t
                ((|%%_LBDAUxqQS_andmap|
                   (#%lambda #t
                     (|%%_LBZwS-qQS_first|)
                     (|%%_LBDAUxqQS_andmap| |%%_LBx5btmQS_f|)
                     ((#%lambda #t
                        (|%%_LBjtQrrQS_x| |%%_LBFpOUrQS_first|)
                        (|%%_LBDAUxqQS_andmap| |%%_LBx5btmQS_f|)
                        (#%if (null? |%%_LBFpOUrQS_first|)
                          (|%%_LBx5btmQS_f| |%%_LBjtQrrQS_x|)
                          (#%if (|%%_LBx5btmQS_f| |%%_LBjtQrrQS_x|)
                            (|%%_LBDAUxqQS_andmap| |%%_LBFpOUrQS_first|)
                            #f)))
                      (car |%%_LBZwS-qQS_first|)
                      (cdr |%%_LBZwS-qQS_first|)))))
                (|%%_LBx5btmQS_f|)
                |%%_LBDAUxqQS_andmap|)
              |%%_LBb9d0mQS_first|)
             ((#%letrec #t
                ((|%%_LBd-6nnQS_andmap|
                   (#%lambda #t
                     (|%%_LBzW4QnQS_first| |%%_LBVS2hoQS_rest|)
                     (|%%_LBd-6nnQS_andmap| |%%_LBx5btmQS_f|)
                     ((#%lambda #t
                        (|%%_LBfP0KoQS_x|
                          |%%_LBBL-apQS_xr|
                          |%%_LBXHYDpQS_first|
                          |%%_LBhEW4qQS_rest|)
                        (|%%_LBd-6nnQS_andmap| |%%_LBx5btmQS_f|)
                        (#%if (null? |%%_LBXHYDpQS_first|)
                          (apply |%%_LBx5btmQS_f|
                                 (cons |%%_LBfP0KoQS_x| |%%_LBBL-apQS_xr|))
                          (#%if (apply |%%_LBx5btmQS_f|
                                       (cons |%%_LBfP0KoQS_x|
                                             |%%_LBBL-apQS_xr|))
                            (|%%_LBd-6nnQS_andmap|
                              |%%_LBXHYDpQS_first|
                              |%%_LBhEW4qQS_rest|)
                            #f)))
                      (car |%%_LBzW4QnQS_first|)
                      (map car |%%_LBVS2hoQS_rest|)
                      (cdr |%%_LBzW4QnQS_first|)
                      (map cdr |%%_LBVS2hoQS_rest|)))))
                (|%%_LBx5btmQS_f|)
                |%%_LBd-6nnQS_andmap|)
              |%%_LBb9d0mQS_first|
              |%%_LBRcfzlQS_rest|))))
       (null? |%%_LBb9d0mQS_first|)))))
(#%program
  ((gensym . 2)
   (symbol->string . 2)
   (string-append . 1)
   (string->symbol . 1)
   (|%%_LB_lMlsQS_base| . 2))
  ()
  (string->symbol
    symbol->string
    gensym
    string-append)
  (#%define gen-sym
    (#%lambda #t
      (|%%_LB_lMlsQS_base|)
      ()
      (#%if |%%_LB_lMlsQS_base|
        (string->symbol
          (string-append
            (symbol->string (gensym))
            "_"
            (symbol->string |%%_LB_lMlsQS_base|)))
        (gensym)))))
(#%program
  ((car . 1)
   (cdr . 1)
   (|%%_LBliKOsQS_proc| . 2)
   (ormap . 1)
   (|%%_LB1bGItQS_t| . 2)
   (|%%_LBHeIftQS_list1| . 3)
   (null? . 1)
   (not . 1))
  ()
  (ormap cdr car not null?)
  (#%define ormap
    (#%lambda #t
      (|%%_LBliKOsQS_proc| |%%_LBHeIftQS_list1|)
      ()
      (#%if (not (null? |%%_LBHeIftQS_list1|))
        ((#%lambda #t
           (|%%_LB1bGItQS_t|)
           (|%%_LBHeIftQS_list1| |%%_LBliKOsQS_proc|)
           (#%if |%%_LB1bGItQS_t|
             |%%_LB1bGItQS_t|
             (ormap |%%_LBliKOsQS_proc|
                    (cdr |%%_LBHeIftQS_list1|))))
         (|%%_LBliKOsQS_proc| (car |%%_LBHeIftQS_list1|)))
        #f))))
(#%program
  ((cons . 1)
   (cdr . 2)
   (remq . 2)
   (car . 2)
   (|%%_LBn7E9uQS_o| . 3)
   (eq? . 1)
   (|%%_LBJ3CCuQS_lst| . 5)
   (null? . 1))
  ()
  (car eq? cdr remq cons null?)
  (#%define remq
    (#%lambda #t
      (|%%_LBn7E9uQS_o| |%%_LBJ3CCuQS_lst|)
      ()
      (#%if (null? |%%_LBJ3CCuQS_lst|)
        ()
        (#%if (eq? |%%_LBn7E9uQS_o| (car |%%_LBJ3CCuQS_lst|))
          (remq |%%_LBn7E9uQS_o| (cdr |%%_LBJ3CCuQS_lst|))
          (cons (car |%%_LBJ3CCuQS_lst|)
                (remq |%%_LBn7E9uQS_o| (cdr |%%_LBJ3CCuQS_lst|))))))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define $sc-put-cte (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define identifier? (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define sc-expand (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define datum->syntax-object (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define syntax-object->datum (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define generate-temporaries (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define free-identifier=? (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define bound-identifier=? (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define literal-identifier=? (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define syntax-error (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define $syntax-dispatch (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define $make-environment (void)))
(#%program
  ((|%%_LB30A3vQS_args| . 1)
   (error . 1)
   (apply . 1))
  ()
  (error apply)
  (#%define throw
    (#%lambda #t
      |%%_LB30A3vQS_args|
      ()
      (apply error |%%_LB30A3vQS_args|))))
(#%program
  ((newline . 1)
   (|%%_LBpYxwvQS_args| . 1)
   (|%%_LBLUvZvQS_arg| . 1)
   (display . 2)
   (for-each . 1))
  ()
  (newline for-each display)
  (#%define error
    (#%lambda #t
      |%%_LBpYxwvQS_args|
      ()
      (#%begin
        (for-each
          (#%lambda #t
            (|%%_LBLUvZvQS_arg|)
            ()
            (#%begin
              (display |%%_LBLUvZvQS_arg|)
              (display #\space)))
          |%%_LBpYxwvQS_args|)
        (newline)))))
(#%program
  ((_make-native-parameter . 1))
  ()
  (_make-native-parameter)
  (#%define strict-r5rs-compliance
    (_make-native-parameter "strictR5RSCompliance")))
(#%program
  ((pair? . 1)
   (|%%_LB5RtqwQS_v| . 2)
   (symbol? . 1)
   (|%%_LBrNrTwQS_t| . 2)
   (not . 1))
  ()
  (symbol? pair? not)
  (#%define atom?
    (#%lambda #t
      (|%%_LB5RtqwQS_v|)
      ()
      (not ((#%lambda #t
              (|%%_LBrNrTwQS_t|)
              (|%%_LB5RtqwQS_v|)
              (#%if |%%_LBrNrTwQS_t|
                |%%_LBrNrTwQS_t|
                (symbol? |%%_LB5RtqwQS_v|)))
            (pair? |%%_LB5RtqwQS_v|))))))
(#%program
  ()
  ()
  ()
  (#%define make-false
    (#%lambda #t (|%%_LBNJpkxQS_v|) () #f)))
(#%program
  ((load-module . 1)
   (for-each . 1)
   (get-symbolic-environment . 1)
   (getprop . 1)
   (not . 1))
  ()
  (load-module
    for-each
    get-symbolic-environment
    getprop
    not)
  (#%if (not (getprop
               (#%quote lite)
               (get-symbolic-environment (#%quote *sisc*))))
    (for-each
      load-module
      (#%quote
        ("sisc.modules.OptionalPrimitives$Index")))
    #!void))
