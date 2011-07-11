(#%program
  ((_make-native-parameter . 1))
  ()
  (_make-native-parameter)
  (#%define hedged-inlining
    (_make-native-parameter "hedgedInlining")))
(#%program
  ((hedged-inlining . 1))
  ()
  (hedged-inlining)
  (hedged-inlining #f))
(#%program () () () #!void)
(#%program
  ((map-cdr . 1)
   (for-each . 1)
   (map-car . 1)
   (|%%_SqJAUsxsu_proc| . 2)
   (apply . 2)
   (cons . 1)
   (|%%_Sq1IYywsu_lists| . 4)
   (|%%_SqnEW_wsu_ls1| . 2)
   (null? . 1))
  ((|%%_Sq1IYywsu_lists| . 1))
  (cons map-car apply map-cdr for-each null?)
  (#%define for-each
    (#%lambda #t
      (|%%_SqJAUsxsu_proc|
        |%%_SqnEW_wsu_ls1|
        .
        |%%_Sq1IYywsu_lists|)
      ()
      (#%if (null? |%%_SqnEW_wsu_ls1|)
        #!void
        (#%begin
          (#%set! |%%_Sq1IYywsu_lists|
            (cons |%%_SqnEW_wsu_ls1| |%%_Sq1IYywsu_lists|))
          (apply |%%_SqJAUsxsu_proc|
                 (map-car |%%_Sq1IYywsu_lists|))
          (apply for-each
                 |%%_SqJAUsxsu_proc|
                 (map-cdr |%%_Sq1IYywsu_lists|)))))))
(#%program
  ((|%%_Sq3xSVxsu_x| . 1) (eq? . 1))
  ()
  (eq?)
  (#%define eof-object?
    (#%lambda #t
      (|%%_Sq3xSVxsu_x|)
      ()
      (eq? |%%_Sq3xSVxsu_x| #!eof))))
(#%program
  ((|%%_SqptQmysu_x| . 1))
  ()
  ()
  (#%define not
    (#%lambda #t
      (|%%_SqptQmysu_x|)
      ()
      (#%if |%%_SqptQmysu_x| #f #t))))
(#%program
  ((|%%_SqLpOPysu_port| . 1)
   (display . 1)
   (apply . 1))
  ()
  (display apply)
  (#%define newline
    (#%lambda #t
      |%%_SqLpOPysu_port|
      ()
      (apply display #\newline |%%_SqLpOPysu_port|))))
(#%program
  ((|%%_SqriKJzsu_ls| . 1)
   (car . 1)
   (cons . 1)
   (cdr . 1)
   (|%%_Sq5mMgzsu_iter| . 2)
   (|%%_Sq7bGDAsu_acc| . 2)
   (|%%_SqNeIaAsu_ls| . 3)
   (null? . 1))
  ((|%%_Sq5mMgzsu_iter| . 1))
  (cdr car cons null?)
  (#%define reverse
    (#%letrec #t
      ((|%%_Sq5mMgzsu_iter|
         (#%lambda #t
           (|%%_SqNeIaAsu_ls| |%%_Sq7bGDAsu_acc|)
           (|%%_Sq5mMgzsu_iter|)
           (#%if (null? |%%_SqNeIaAsu_ls|)
             |%%_Sq7bGDAsu_acc|
             (|%%_Sq5mMgzsu_iter|
               (cdr |%%_SqNeIaAsu_ls|)
               (cons (car |%%_SqNeIaAsu_ls|) |%%_Sq7bGDAsu_acc|))))))
      ()
      (#%lambda #t
        (|%%_SqriKJzsu_ls|)
        (|%%_Sq5mMgzsu_iter|)
        (|%%_Sq5mMgzsu_iter| |%%_SqriKJzsu_ls| ())))))
(#%program
  ((|%%_SqP3CxBsu_s| . 1)
   (cdr . 1)
   (|%%_SqRUvUCsu_d| . 1)
   (|%%_Sqt7E4Bsu_iter| . 2)
   (set-cdr! . 1)
   (|%%_SqvYxrCsu_r| . 2)
   (|%%_Sq90A-Bsu_s| . 4)
   (null? . 1))
  ((|%%_Sqt7E4Bsu_iter| . 1))
  (set-cdr! cdr null?)
  (#%define reverse!
    (#%letrec #t
      ((|%%_Sqt7E4Bsu_iter|
         (#%lambda #t
           (|%%_Sq90A-Bsu_s| |%%_SqvYxrCsu_r|)
           (|%%_Sqt7E4Bsu_iter|)
           (#%if (null? |%%_Sq90A-Bsu_s|)
             |%%_SqvYxrCsu_r|
             ((#%lambda #t
                (|%%_SqRUvUCsu_d|)
                (|%%_SqvYxrCsu_r|
                  |%%_Sq90A-Bsu_s|
                  |%%_Sqt7E4Bsu_iter|)
                (#%begin
                  (set-cdr! |%%_Sq90A-Bsu_s| |%%_SqvYxrCsu_r|)
                  (|%%_Sqt7E4Bsu_iter|
                    |%%_SqRUvUCsu_d|
                    |%%_Sq90A-Bsu_s|)))
              (cdr |%%_Sq90A-Bsu_s|))))))
      ()
      (#%lambda #t
        (|%%_SqP3CxBsu_s|)
        (|%%_Sqt7E4Bsu_iter|)
        (|%%_Sqt7E4Bsu_iter| |%%_SqP3CxBsu_s| ())))))
(#%program
  ((cdr . 1)
   (map-car . 1)
   (caar . 1)
   (cons . 1)
   (|%%_SqbRtlDsu_ls| . 3)
   (null? . 1))
  ()
  (cons caar cdr map-car null?)
  (#%define map-car
    (#%lambda #t
      (|%%_SqbRtlDsu_ls|)
      ()
      (#%if (null? |%%_SqbRtlDsu_ls|)
        ()
        (cons (caar |%%_SqbRtlDsu_ls|)
              (map-car (cdr |%%_SqbRtlDsu_ls|)))))))
(#%program
  ((cdr . 1)
   (map-cdr . 1)
   (cdar . 1)
   (cons . 1)
   (|%%_SqxNrODsu_ls| . 3)
   (null? . 1))
  ()
  (cons cdar cdr map-cdr null?)
  (#%define map-cdr
    (#%lambda #t
      (|%%_SqxNrODsu_ls|)
      ()
      (#%if (null? |%%_SqxNrODsu_ls|)
        ()
        (cons (cdar |%%_SqxNrODsu_ls|)
              (map-cdr (cdr |%%_SqxNrODsu_ls|)))))))
(#%program
  ((|%%_SqVyjCFsu_list1| . 2)
   (|%%_Sqfvh3Gsu_proc| . 2)
   (|%%_SqzCl9Fsu_lists| . 2)
   (map-car . 1)
   (apply . 1)
   (|%%_Sqj95NIsu_lists| . 2)
   (map-cdr . 1)
   (|%%_SqDg9THsu_proc| . 2)
   (|%%_SqdGnIEsu_loop| . 2)
   (|%%_SqF53eJsu_c| . 2)
   (|%%_SqZc7kIsu_list1| . 3)
   (car . 2)
   (cons . 2)
   (cdr . 2)
   (|%%_SqBrfwGsu_proc| . 2)
   (|%%_SqTJpfEsu_map1| . 2)
   (|%%_SqhkbqHsu_acc| . 2)
   (reverse . 2)
   (|%%_SqXndZGsu_list| . 3)
   (null? . 3))
  ((|%%_SqdGnIEsu_loop| . 1)
   (|%%_SqTJpfEsu_map1| . 1))
  (map-cdr
    apply
    map-car
    cdr
    car
    cons
    reverse
    null?)
  (#%define map
    (#%letrec #t
      ((|%%_SqTJpfEsu_map1|
         (#%lambda #t
           (|%%_SqBrfwGsu_proc|
             |%%_SqXndZGsu_list|
             |%%_SqhkbqHsu_acc|)
           (|%%_SqTJpfEsu_map1|)
           (#%if (null? |%%_SqXndZGsu_list|)
             (reverse |%%_SqhkbqHsu_acc|)
             (|%%_SqTJpfEsu_map1|
               |%%_SqBrfwGsu_proc|
               (cdr |%%_SqXndZGsu_list|)
               (cons (|%%_SqBrfwGsu_proc| (car |%%_SqXndZGsu_list|))
                     |%%_SqhkbqHsu_acc|)))))
       (|%%_SqdGnIEsu_loop|
         (#%lambda #t
           (|%%_SqDg9THsu_proc|
             |%%_SqZc7kIsu_list1|
             |%%_Sqj95NIsu_lists|
             |%%_SqF53eJsu_c|)
           (|%%_SqdGnIEsu_loop|)
           (#%if (null? |%%_SqZc7kIsu_list1|)
             (reverse |%%_SqF53eJsu_c|)
             (|%%_SqdGnIEsu_loop|
               |%%_SqDg9THsu_proc|
               (cdr |%%_SqZc7kIsu_list1|)
               (map-cdr |%%_Sqj95NIsu_lists|)
               (cons (apply |%%_SqDg9THsu_proc|
                            (car |%%_SqZc7kIsu_list1|)
                            (map-car |%%_Sqj95NIsu_lists|))
                     |%%_SqF53eJsu_c|))))))
      ()
      (#%lambda #t
        (|%%_Sqfvh3Gsu_proc|
          |%%_SqVyjCFsu_list1|
          .
          |%%_SqzCl9Fsu_lists|)
        (|%%_SqdGnIEsu_loop| |%%_SqTJpfEsu_map1|)
        (#%if (null? |%%_SqzCl9Fsu_lists|)
          (|%%_SqTJpfEsu_map1|
            |%%_Sqfvh3Gsu_proc|
            |%%_SqVyjCFsu_list1|
            ())
          (|%%_SqdGnIEsu_loop|
            |%%_Sqfvh3Gsu_proc|
            |%%_SqVyjCFsu_list1|
            |%%_SqzCl9Fsu_lists|
            ()))))))
(#%program
  ((|%%_SqHWYAKsu_x| . 1)
   (|%%_Sql--7Ksu_g| . 1)
   (|%%_Sq_11HJsu_f| . 1))
  ()
  ()
  (#%define compose2
    (#%lambda #t
      (|%%_Sq_11HJsu_f| |%%_Sql--7Ksu_g|)
      ()
      (#%lambda #t
        (|%%_SqHWYAKsu_x|)
        (|%%_Sql--7Ksu_g| |%%_Sq_11HJsu_f|)
        (|%%_Sq_11HJsu_f|
          (|%%_Sql--7Ksu_g| |%%_SqHWYAKsu_x|))))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define assq (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define assv (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define assoc (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define memq (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define memv (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define member (void)))
(#%program
  ((|%%_Sqv7wWQsu_list| . 1)
   (|%%_Sq9bytQsu_obj| . 1)
   (member . 1)
   (|%%_SqPeA0Qsu_list| . 1)
   (|%%_SqtiCzPsu_obj| . 1)
   (memv . 1)
   (|%%_Sq7mE6Psu_list| . 1)
   (|%%_SqNpGFOsu_obj| . 1)
   (memq . 1)
   (|%%_SqrtIcOsu_alist| . 1)
   (|%%_Sq5xKLNsu_obj| . 1)
   (equal? . 2)
   (assoc . 1)
   (|%%_SqLAMiNsu_alist| . 1)
   (|%%_SqpEORMsu_obj| . 1)
   (eqv? . 2)
   (assv . 1)
   (|%%_Sq3IQoMsu_alist| . 1)
   (|%%_SqJLSXLsu_obj| . 1)
   (eq? . 2)
   (assq . 1)
   (|%%_SqnPUuLsu_memn| . 4)
   (|%%_SqdRlbTsu_obj| . 2)
   (|%%_SqTUnKSsu_n| . 2)
   (|%%_SqzNjETsu_list| . 4)
   (cdr . 2)
   (|%%_Sq1TW1Lsu_assn| . 4)
   (car . 2)
   (|%%_Sqb0sQRsu_obj| . 2)
   (caar . 1)
   (|%%_SqR3unRsu_n| . 2)
   (|%%_SqxYphSsu_alist| . 4)
   (null? . 2))
  ((member . 1)
   (memv . 1)
   (memq . 1)
   (assoc . 1)
   (assv . 1)
   (assq . 1)
   (|%%_SqnPUuLsu_memn| . 1)
   (|%%_Sq1TW1Lsu_assn| . 1))
  (assq eq?
        assv
        eqv?
        assoc
        equal?
        memq
        memv
        member
        caar
        car
        cdr
        null?)
  (#%letrec #t
    ((|%%_Sq1TW1Lsu_assn|
       (#%lambda #t
         (|%%_SqR3unRsu_n|
           |%%_Sqb0sQRsu_obj|
           |%%_SqxYphSsu_alist|)
         (|%%_Sq1TW1Lsu_assn|)
         (#%if (null? |%%_SqxYphSsu_alist|)
           #f
           (#%if (|%%_SqR3unRsu_n|
                   (caar |%%_SqxYphSsu_alist|)
                   |%%_Sqb0sQRsu_obj|)
             (car |%%_SqxYphSsu_alist|)
             (|%%_Sq1TW1Lsu_assn|
               |%%_SqR3unRsu_n|
               |%%_Sqb0sQRsu_obj|
               (cdr |%%_SqxYphSsu_alist|))))))
     (|%%_SqnPUuLsu_memn|
       (#%lambda #t
         (|%%_SqTUnKSsu_n|
           |%%_SqdRlbTsu_obj|
           |%%_SqzNjETsu_list|)
         (|%%_SqnPUuLsu_memn|)
         (#%if (null? |%%_SqzNjETsu_list|)
           #f
           (#%if (|%%_SqTUnKSsu_n|
                   (car |%%_SqzNjETsu_list|)
                   |%%_SqdRlbTsu_obj|)
             |%%_SqzNjETsu_list|
             (|%%_SqnPUuLsu_memn|
               |%%_SqTUnKSsu_n|
               |%%_SqdRlbTsu_obj|
               (cdr |%%_SqzNjETsu_list|)))))))
    ()
    (#%begin
      (#%set! assq
        (#%lambda #t
          (|%%_SqJLSXLsu_obj| |%%_Sq3IQoMsu_alist|)
          (|%%_Sq1TW1Lsu_assn|)
          (|%%_Sq1TW1Lsu_assn|
            eq?
            |%%_SqJLSXLsu_obj|
            |%%_Sq3IQoMsu_alist|)))
      (#%set! assv
        (#%lambda #t
          (|%%_SqpEORMsu_obj| |%%_SqLAMiNsu_alist|)
          (|%%_Sq1TW1Lsu_assn|)
          (|%%_Sq1TW1Lsu_assn|
            eqv?
            |%%_SqpEORMsu_obj|
            |%%_SqLAMiNsu_alist|)))
      (#%set! assoc
        (#%lambda #t
          (|%%_Sq5xKLNsu_obj| |%%_SqrtIcOsu_alist|)
          (|%%_Sq1TW1Lsu_assn|)
          (|%%_Sq1TW1Lsu_assn|
            equal?
            |%%_Sq5xKLNsu_obj|
            |%%_SqrtIcOsu_alist|)))
      (#%set! memq
        (#%lambda #t
          (|%%_SqNpGFOsu_obj| |%%_Sq7mE6Psu_list|)
          (|%%_SqnPUuLsu_memn|)
          (|%%_SqnPUuLsu_memn|
            eq?
            |%%_SqNpGFOsu_obj|
            |%%_Sq7mE6Psu_list|)))
      (#%set! memv
        (#%lambda #t
          (|%%_SqtiCzPsu_obj| |%%_SqPeA0Qsu_list|)
          (|%%_SqnPUuLsu_memn|)
          (|%%_SqnPUuLsu_memn|
            eqv?
            |%%_SqtiCzPsu_obj|
            |%%_SqPeA0Qsu_list|)))
      (#%set! member
        (#%lambda #t
          (|%%_Sq9bytQsu_obj| |%%_Sqv7wWQsu_list|)
          (|%%_SqnPUuLsu_memn|)
          (|%%_SqnPUuLsu_memn|
            equal?
            |%%_Sq9bytQsu_obj|
            |%%_Sqv7wWQsu_list|))))))
(#%program
  ((cdr . 1) (car . 1) (compose2 . 1))
  ()
  (cdr car compose2)
  (#%define cadr (compose2 car cdr)))
(#%program
  ((car . 1) (cdr . 1) (compose2 . 1))
  ()
  (car cdr compose2)
  (#%define cdar (compose2 cdr car)))
(#%program
  ((cdr . 2) (compose2 . 1))
  ()
  (cdr compose2)
  (#%define cddr (compose2 cdr cdr)))
(#%program
  ((car . 2) (compose2 . 1))
  ()
  (car compose2)
  (#%define caar (compose2 car car)))
(#%program
  ((caar . 1) (car . 1) (compose2 . 1))
  ()
  (caar car compose2)
  (#%define caaar (compose2 car caar)))
(#%program
  ((cadr . 1) (car . 1) (compose2 . 1))
  ()
  (cadr car compose2)
  (#%define caadr (compose2 car cadr)))
(#%program
  ((cdar . 1) (car . 1) (compose2 . 1))
  ()
  (cdar car compose2)
  (#%define cadar (compose2 car cdar)))
(#%program
  ((cddr . 1) (car . 1) (compose2 . 1))
  ()
  (cddr car compose2)
  (#%define caddr (compose2 car cddr)))
(#%program
  ((caar . 1) (cdr . 1) (compose2 . 1))
  ()
  (caar cdr compose2)
  (#%define cdaar (compose2 cdr caar)))
(#%program
  ((cadr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cadr cdr compose2)
  (#%define cdadr (compose2 cdr cadr)))
(#%program
  ((cdar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdar cdr compose2)
  (#%define cddar (compose2 cdr cdar)))
(#%program
  ((cddr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cddr cdr compose2)
  (#%define cdddr (compose2 cdr cddr)))
(#%program
  ((caaar . 1) (car . 1) (compose2 . 1))
  ()
  (caaar car compose2)
  (#%define caaaar (compose2 car caaar)))
(#%program
  ((caadr . 1) (car . 1) (compose2 . 1))
  ()
  (caadr car compose2)
  (#%define caaadr (compose2 car caadr)))
(#%program
  ((cadar . 1) (car . 1) (compose2 . 1))
  ()
  (cadar car compose2)
  (#%define caadar (compose2 car cadar)))
(#%program
  ((caddr . 1) (car . 1) (compose2 . 1))
  ()
  (caddr car compose2)
  (#%define caaddr (compose2 car caddr)))
(#%program
  ((cdaar . 1) (car . 1) (compose2 . 1))
  ()
  (cdaar car compose2)
  (#%define cadaar (compose2 car cdaar)))
(#%program
  ((cdadr . 1) (car . 1) (compose2 . 1))
  ()
  (cdadr car compose2)
  (#%define cadadr (compose2 car cdadr)))
(#%program
  ((cddar . 1) (car . 1) (compose2 . 1))
  ()
  (cddar car compose2)
  (#%define caddar (compose2 car cddar)))
(#%program
  ((cdddr . 1) (car . 1) (compose2 . 1))
  ()
  (cdddr car compose2)
  (#%define cadddr (compose2 car cdddr)))
(#%program
  ((caaar . 1) (cdr . 1) (compose2 . 1))
  ()
  (caaar cdr compose2)
  (#%define cdaaar (compose2 cdr caaar)))
(#%program
  ((caadr . 1) (cdr . 1) (compose2 . 1))
  ()
  (caadr cdr compose2)
  (#%define cdaadr (compose2 cdr caadr)))
(#%program
  ((cadar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cadar cdr compose2)
  (#%define cdadar (compose2 cdr cadar)))
(#%program
  ((caddr . 1) (cdr . 1) (compose2 . 1))
  ()
  (caddr cdr compose2)
  (#%define cdaddr (compose2 cdr caddr)))
(#%program
  ((cdaar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdaar cdr compose2)
  (#%define cddaar (compose2 cdr cdaar)))
(#%program
  ((cdadr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdadr cdr compose2)
  (#%define cddadr (compose2 cdr cdadr)))
(#%program
  ((cddar . 1) (cdr . 1) (compose2 . 1))
  ()
  (cddar cdr compose2)
  (#%define cdddar (compose2 cdr cddar)))
(#%program
  ((cdddr . 1) (cdr . 1) (compose2 . 1))
  ()
  (cdddr cdr compose2)
  (#%define cddddr (compose2 cdr cdddr)))
(#%program
  ((cdr . 1)
   (append2 . 1)
   (car . 1)
   (cons . 1)
   (|%%_SqfGfyUsu_ls2| . 2)
   (|%%_SqVJh5Usu_ls1| . 3)
   (null? . 1))
  ()
  (cons car cdr append2 null?)
  (#%define append2
    (#%lambda #t
      (|%%_SqVJh5Usu_ls1| |%%_SqfGfyUsu_ls2|)
      ()
      (#%if (null? |%%_SqVJh5Usu_ls1|)
        |%%_SqfGfyUsu_ls2|
        (cons (car |%%_SqVJh5Usu_ls1|)
              (append2
                (cdr |%%_SqVJh5Usu_ls1|)
                |%%_SqfGfyUsu_ls2|))))))
(#%program
  ((append2 . 1))
  ()
  (append2)
  (#%define append append2))
(#%program
  ((|%%_SqXybsVsu_base-case| . 1)
   (|%%_SqDr7mWsu_args| . 3)
   (cdr . 2)
   (car . 2)
   (|%%_SqBCd_Usu_proc| . 1)
   (|%%_Sqhv9VVsu_helper| . 2)
   (|%%_SqZn5PWsu_acc| . 2)
   (|%%_Sqjk3gXsu_argls| . 3)
   (null? . 2))
  ((|%%_Sqhv9VVsu_helper| . 1))
  (null? cdr car)
  (#%define _make-left-pairwise-nary
    (#%lambda #t
      (|%%_SqBCd_Usu_proc| |%%_SqXybsVsu_base-case|)
      ()
      (#%letrec #t
        ((|%%_Sqhv9VVsu_helper|
           (#%lambda #t
             (|%%_SqZn5PWsu_acc| |%%_Sqjk3gXsu_argls|)
             (|%%_Sqhv9VVsu_helper| |%%_SqBCd_Usu_proc|)
             (#%if (null? |%%_Sqjk3gXsu_argls|)
               |%%_SqZn5PWsu_acc|
               (|%%_Sqhv9VVsu_helper|
                 (|%%_SqBCd_Usu_proc|
                   |%%_SqZn5PWsu_acc|
                   (car |%%_Sqjk3gXsu_argls|))
                 (cdr |%%_Sqjk3gXsu_argls|))))))
        (|%%_SqXybsVsu_base-case| |%%_SqBCd_Usu_proc|)
        (#%lambda #t
          |%%_SqDr7mWsu_args|
          (|%%_Sqhv9VVsu_helper| |%%_SqXybsVsu_base-case|)
          (#%if (null? |%%_SqDr7mWsu_args|)
            |%%_SqXybsVsu_base-case|
            (|%%_Sqhv9VVsu_helper|
              (car |%%_SqDr7mWsu_args|)
              (cdr |%%_SqDr7mWsu_args|))))))))
(#%program
  ((length . 1)
   (make-string . 1)
   (|%%_Sq_c_9Ysu_l| . 2)
   (+ . 1)
   (cdr . 1)
   (|%%_SqFg1JXsu_l2s| . 2)
   (car . 1)
   (|%%_Sq12VwZsu_n| . 2)
   (string-set! . 1)
   (|%%_SqH5X3Zsu_s| . 3)
   (|%%_Sql9ZCYsu_l| . 3)
   (null? . 1))
  ((|%%_SqFg1JXsu_l2s| . 1))
  (make-string length car string-set! + cdr null?)
  (#%define list->string
    (#%letrec #t
      ((|%%_SqFg1JXsu_l2s|
         (#%lambda #t
           (|%%_Sql9ZCYsu_l|
             |%%_SqH5X3Zsu_s|
             |%%_Sq12VwZsu_n|)
           (|%%_SqFg1JXsu_l2s|)
           (#%if (null? |%%_Sql9ZCYsu_l|)
             |%%_SqH5X3Zsu_s|
             (#%begin
               (string-set!
                 |%%_SqH5X3Zsu_s|
                 |%%_Sq12VwZsu_n|
                 (car |%%_Sql9ZCYsu_l|))
               (|%%_SqFg1JXsu_l2s|
                 (cdr |%%_Sql9ZCYsu_l|)
                 |%%_SqH5X3Zsu_s|
                 (+ |%%_Sq12VwZsu_n| 1)))))))
      ()
      (#%lambda #t
        (|%%_Sq_c_9Ysu_l|)
        (|%%_SqFg1JXsu_l2s|)
        (|%%_SqFg1JXsu_l2s|
          |%%_Sq_c_9Ysu_l|
          (make-string (length |%%_Sq_c_9Ysu_l|))
          0)))))
(#%program
  ((string-length . 1)
   (|%%_SqJWQq-su_s| . 2)
   (- . 2)
   (string-ref . 1)
   (cons . 1)
   (|%%_Sq3TOT-su_s| . 2)
   (|%%_Sqn-SZZsu_s2l| . 2)
   (|%%_SqpPMk_su_h| . 2)
   (|%%_SqLLKN_su_n| . 3)
   (< . 1))
  ((|%%_Sqn-SZZsu_s2l| . 1))
  (string-length string-ref cons - <)
  (#%define string->list
    (#%letrec #t
      ((|%%_Sqn-SZZsu_s2l|
         (#%lambda #t
           (|%%_Sq3TOT-su_s|
             |%%_SqpPMk_su_h|
             |%%_SqLLKN_su_n|)
           (|%%_Sqn-SZZsu_s2l|)
           (#%if (< |%%_SqLLKN_su_n| 0)
             |%%_SqpPMk_su_h|
             (|%%_Sqn-SZZsu_s2l|
               |%%_Sq3TOT-su_s|
               (cons (string-ref |%%_Sq3TOT-su_s| |%%_SqLLKN_su_n|)
                     |%%_SqpPMk_su_h|)
               (- |%%_SqLLKN_su_n| 1))))))
      ()
      (#%lambda #t
        (|%%_SqJWQq-su_s|)
        (|%%_Sqn-SZZsu_s2l|)
        (|%%_Sqn-SZZsu_s2l|
          |%%_SqJWQq-su_s|
          ()
          (- (string-length |%%_SqJWQq-su_s|) 1))))))
(#%program
  ((|%%_Sq5IIe0tu_elems| . 1) (list->vector . 1))
  ()
  (list->vector)
  (#%define vector
    (#%lambda #t
      |%%_Sq5IIe0tu_elems|
      ()
      (list->vector |%%_Sq5IIe0tu_elems|))))
(#%program
  ((|%%_SqrEGH0tu_elems| . 1) (list->string . 1))
  ()
  (list->string)
  (#%define string
    (#%lambda #t
      |%%_SqrEGH0tu_elems|
      ()
      (list->string |%%_SqrEGH0tu_elems|))))
(#%program
  ((_make-parameter . 1))
  ()
  (_make-parameter)
  (#%define current-url (_make-parameter "file:.")))
(#%program
  ((car . 1)
   (string-length . 1)
   (string-append . 1)
   (|%%_SqttA22tu_l| . 1)
   (- . 1)
   (|%%_Sq7xCB1tu_v| . 4)
   (string-ref . 1)
   (eqv? . 1)
   (current-url . 3)
   (normalize-url . 2)
   (|%%_SqNAE81tu_rest| . 2)
   (null? . 1))
  ()
  (string-length
    string-ref
    -
    eqv?
    string-append
    car
    normalize-url
    current-url
    null?)
  (#%define current-directory
    (#%lambda #t
      |%%_SqNAE81tu_rest|
      ()
      (#%if (null? |%%_SqNAE81tu_rest|)
        (normalize-url (current-url) ".")
        (current-url
          (normalize-url
            (current-url)
            ((#%lambda #t
               (|%%_Sq7xCB1tu_v|)
               ()
               ((#%lambda #t
                  (|%%_SqttA22tu_l|)
                  (|%%_Sq7xCB1tu_v|)
                  (#%if (eqv? (string-ref
                                |%%_Sq7xCB1tu_v|
                                (- |%%_SqttA22tu_l| 1))
                              #\/)
                    |%%_Sq7xCB1tu_v|
                    (string-append |%%_Sq7xCB1tu_v| "/")))
                (string-length |%%_Sq7xCB1tu_v|)))
             (car |%%_SqNAE81tu_rest|))))))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define file-handler (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define add-file-handler (void)))
(#%program
  ((load . 1)
   (|%%_Sqbbqj4tu_extension| . 1)
   (string-downcase . 1)
   (string->symbol . 1)
   (|%%_SqResS3tu__load| . 1)
   (cdr . 1)
   (|%%_Sqx7oM4tu_t| . 2)
   (file-handler . 1)
   (|%%_Sqviup3tu_thunk| . 1)
   (cons . 2)
   (|%%_SqPpyv2tu_*file-handlers*| . 4)
   (|%%_Sq9mwY2tu_extension| . 2)
   (assq . 2)
   (add-file-handler . 1))
  ((file-handler . 1)
   (|%%_SqPpyv2tu_*file-handlers*| . 1)
   (add-file-handler . 1))
  (cdr string->symbol
       string-downcase
       load
       file-handler
       cons
       assq
       add-file-handler)
  ((#%lambda #t
     (|%%_SqPpyv2tu_*file-handlers*|)
     ()
     (#%begin
       (#%set! add-file-handler
         (#%lambda #t
           (|%%_Sq9mwY2tu_extension| |%%_Sqviup3tu_thunk|)
           (|%%_SqPpyv2tu_*file-handlers*|)
           (#%if (assq |%%_Sq9mwY2tu_extension|
                       |%%_SqPpyv2tu_*file-handlers*|)
             #!void
             (#%set! |%%_SqPpyv2tu_*file-handlers*|
               (cons (cons |%%_Sq9mwY2tu_extension|
                           |%%_Sqviup3tu_thunk|)
                     |%%_SqPpyv2tu_*file-handlers*|)))))
       (#%set! file-handler
         ((#%lambda #t
            (|%%_SqResS3tu__load|)
            (|%%_SqPpyv2tu_*file-handlers*|)
            (#%lambda #t
              (|%%_Sqbbqj4tu_extension|)
              (|%%_SqResS3tu__load|
                |%%_SqPpyv2tu_*file-handlers*|)
              ((#%lambda #t
                 (|%%_Sqx7oM4tu_t|)
                 (|%%_SqResS3tu__load|)
                 (#%if |%%_Sqx7oM4tu_t|
                   (cdr |%%_Sqx7oM4tu_t|)
                   |%%_SqResS3tu__load|))
               (assq (string->symbol
                       (string-downcase |%%_Sqbbqj4tu_extension|))
                     |%%_SqPpyv2tu_*file-handlers*|))))
          load))))
   ()))
(#%program
  ((|%%_Sqd0kG5tu_rest| . 1)
   (|%%_SqzYh76tu_file| . 1)
   (current-url . 1)
   (normalize-url . 1)
   (|%%_SqT3md5tu_proc| . 1)
   (apply . 1))
  ()
  (apply current-url normalize-url)
  (#%define make-io-proc
    (#%lambda #t
      (|%%_SqT3md5tu_proc|)
      ()
      (#%lambda #t
        (|%%_SqzYh76tu_file| . |%%_Sqd0kG5tu_rest|)
        (|%%_SqT3md5tu_proc|)
        (apply |%%_SqT3md5tu_proc|
               (normalize-url (current-url) |%%_SqzYh76tu_file|)
               |%%_Sqd0kG5tu_rest|)))))
(#%program
  ((|%%_Sqjv1L9tu_url| . 1)
   (string->list . 1)
   (reverse! . 1)
   (cons . 1)
   (cdr . 1)
   (|%%_SqFr_batu_loop| . 2)
   (|%%_SqlkX5btu_acc| . 2)
   (list->string . 1)
   (car . 2)
   (equal? . 1)
   (|%%_Sq_nZEatu_x| . 4)
   (null? . 1)
   (void . 1)
   (|%%_SqVUfA6tu_file-extension| . 1)
   (|%%_SqZy3i9tu_fe| . 2)
   (file-handler . 1)
   (|%%_SqhG7o8tu_e| . 1)
   (|%%_SqXJ9X7tu_m| . 1)
   (|%%_SqDC5R8tu_fk| . 1)
   (call-with-failure-continuation . 1)
   (with-failure-continuation . 1)
   (|%%_SqfRd17tu_file| . 1)
   (|%%_SqBNbu7tu_previous-url| . 3)
   (normalize-url . 1)
   (current-url . 6)
   (load . 1)
   (open-output-file . 2)
   (open-source-input-file . 2)
   (make-io-proc . 3)
   (open-input-file . 2))
  ((|%%_SqFr_batu_loop| . 1)
   (load . 1)
   (open-output-file . 1)
   (open-source-input-file . 1)
   (open-input-file . 1))
  (open-input-file
    open-input-file
    make-io-proc
    open-source-input-file
    open-source-input-file
    open-output-file
    open-output-file
    load
    normalize-url
    current-url
    file-handler
    call-with-failure-continuation
    with-failure-continuation
    void
    reverse!
    string->list
    null?
    cdr
    cons
    list->string
    equal?
    car)
  ((#%lambda #t
     (|%%_SqVUfA6tu_file-extension|)
     ()
     (#%begin
       (#%set! open-input-file
         (make-io-proc open-input-file))
       (#%set! open-source-input-file
         (make-io-proc open-source-input-file))
       (#%set! open-output-file
         (make-io-proc open-output-file))
       (#%set! load
         (#%lambda #t
           (|%%_SqfRd17tu_file|)
           (|%%_SqVUfA6tu_file-extension|)
           (#%begin
             ((#%lambda #t
                (|%%_SqBNbu7tu_previous-url|)
                (|%%_SqfRd17tu_file|
                  |%%_SqVUfA6tu_file-extension|)
                (#%begin
                  (current-url
                    (normalize-url
                      |%%_SqBNbu7tu_previous-url|
                      |%%_SqfRd17tu_file|))
                  (with-failure-continuation
                    (#%lambda #t
                      (|%%_SqXJ9X7tu_m| |%%_SqhG7o8tu_e|)
                      (|%%_SqBNbu7tu_previous-url|)
                      (#%begin
                        (current-url |%%_SqBNbu7tu_previous-url|)
                        (call-with-failure-continuation
                          (#%lambda #t
                            (|%%_SqDC5R8tu_fk|)
                            (|%%_SqhG7o8tu_e| |%%_SqXJ9X7tu_m|)
                            (|%%_SqDC5R8tu_fk|
                              |%%_SqXJ9X7tu_m|
                              |%%_SqhG7o8tu_e|)))))
                    (#%lambda #t
                      ()
                      (|%%_SqVUfA6tu_file-extension|)
                      ((#%lambda #t
                         (|%%_SqZy3i9tu_fe|)
                         ()
                         ((file-handler
                            (#%if |%%_SqZy3i9tu_fe| |%%_SqZy3i9tu_fe| "scm"))
                          (current-url)))
                       (|%%_SqVUfA6tu_file-extension| (current-url)))))
                  (current-url |%%_SqBNbu7tu_previous-url|)))
              (current-url))
             (void))))))
   (#%lambda #t
     (|%%_Sqjv1L9tu_url|)
     ()
     ((#%letrec #t
        ((|%%_SqFr_batu_loop|
           (#%lambda #t
             (|%%_Sq_nZEatu_x| |%%_SqlkX5btu_acc|)
             (|%%_SqFr_batu_loop|)
             (#%if (null? |%%_Sq_nZEatu_x|)
               #f
               (#%if (equal? (car |%%_Sq_nZEatu_x|) #\.)
                 (list->string |%%_SqlkX5btu_acc|)
                 (|%%_SqFr_batu_loop|
                   (cdr |%%_Sq_nZEatu_x|)
                   (cons (car |%%_Sq_nZEatu_x|) |%%_SqlkX5btu_acc|)))))))
        ()
        |%%_SqFr_batu_loop|)
      (reverse! (string->list |%%_Sqjv1L9tu_url|))
      ()))))
(#%program
  ((|%%_SqHgVybtu_str| . 1)
   (load-native-library . 1)
   (native-library-binding-names . 1)
   (|%%_Sqn9Rsctu_binding-names| . 1)
   (|%%_Sq1dT_btu_nl| . 2)
   (native-library-binding . 1)
   (|%%_SqJ5PVctu_name| . 2)
   (putprop . 1)
   (for-each . 1))
  ()
  (load-native-library
    native-library-binding
    putprop
    for-each
    native-library-binding-names)
  (#%define load-module
    (#%lambda #t
      (|%%_SqHgVybtu_str|)
      ()
      ((#%lambda #t
         (|%%_Sq1dT_btu_nl|)
         ()
         ((#%lambda #t
            (|%%_Sqn9Rsctu_binding-names|)
            (|%%_Sq1dT_btu_nl|)
            (for-each
              (#%lambda #t
                (|%%_SqJ5PVctu_name|)
                (|%%_Sq1dT_btu_nl|)
                (putprop
                  |%%_SqJ5PVctu_name|
                  (native-library-binding
                    |%%_Sq1dT_btu_nl|
                    |%%_SqJ5PVctu_name|)))
              |%%_Sqn9Rsctu_binding-names|))
          (native-library-binding-names |%%_Sq1dT_btu_nl|)))
       (load-native-library |%%_SqHgVybtu_str|)))))
(#%program
  ((append2 . 1) (_make-left-pairwise-nary . 1))
  ()
  (append2 _make-left-pairwise-nary)
  (#%define append
    (_make-left-pairwise-nary append2 ())))
(#%program
  ((|%%_Sq32Nmdtu_x| . 2)
   (null? . 2)
   (|%%_Sq5TGJetu_lag| . 1)
   (cdr . 3)
   (|%%_Sqp-KPdtu_lp| . 2)
   (|%%_Sq7IA4gtu_lag| . 2)
   (|%%_SqNLCDftu_x| . 2)
   (eq? . 1)
   (|%%_SqrPEaftu_x| . 3)
   (|%%_SqLWIgetu_x| . 3)
   (pair? . 2))
  ((|%%_Sqp-KPdtu_lp| . 1))
  (pair? cdr eq? null?)
  (#%define proper-list?
    (#%lambda #t
      (|%%_Sq32Nmdtu_x|)
      ()
      ((#%letrec #t
         ((|%%_Sqp-KPdtu_lp|
            (#%lambda #t
              (|%%_SqLWIgetu_x| |%%_Sq5TGJetu_lag|)
              (|%%_Sqp-KPdtu_lp|)
              (#%if (pair? |%%_SqLWIgetu_x|)
                ((#%lambda #t
                   (|%%_SqrPEaftu_x|)
                   (|%%_Sq5TGJetu_lag| |%%_Sqp-KPdtu_lp|)
                   (#%if (pair? |%%_SqrPEaftu_x|)
                     ((#%lambda #t
                        (|%%_SqNLCDftu_x| |%%_Sq7IA4gtu_lag|)
                        (|%%_Sqp-KPdtu_lp|)
                        (#%if (eq? |%%_SqNLCDftu_x| |%%_Sq7IA4gtu_lag|)
                          #f
                          (|%%_Sqp-KPdtu_lp|
                            |%%_SqNLCDftu_x|
                            |%%_Sq7IA4gtu_lag|)))
                      (cdr |%%_SqrPEaftu_x|)
                      (cdr |%%_Sq5TGJetu_lag|))
                     (null? |%%_SqrPEaftu_x|)))
                 (cdr |%%_SqLWIgetu_x|))
                (null? |%%_SqLWIgetu_x|)))))
         ()
         |%%_Sqp-KPdtu_lp|)
       |%%_Sq32Nmdtu_x|
       |%%_Sq32Nmdtu_x|))))
(#%program
  ((proper-list? . 1))
  ()
  (proper-list?)
  (#%define list? proper-list?))
(#%program
  ((|%%_SqtEyxgtu_general-expt| . 1)
   (|%%_Sq9xurhtu_integer-expt| . 1)
   (denominator . 1)
   (numerator . 1)
   (|%%_SqPAw-gtu_rational-expt| . 1)
   (integer? . 2)
   (rational? . 1)
   (|%%_SqvtsUhtu_base| . 9)
   (|%%_SqRpqlitu_exponent| . 8)
   (|%%_SqDN3kntu_squaring| . 3)
   (odd? . 1)
   (quotient . 1)
   (|%%_SqBY9Zltu_loop| . 2)
   (|%%_SqhR5Tmtu_result| . 3)
   (|%%_SqXU7qmtu_rest| . 3)
   (zero? . 3)
   (abs . 2)
   (ashl . 2)
   (|%%_Sqf0cwltu_exponent| . 7)
   (negative? . 3)
   (= . 1)
   (|%%_SqV3e3ltu_base| . 4)
   (exact? . 5)
   (|%%_Sqdbi9ktu_base-denominator| . 1)
   (|%%_Sqz7gCktu_exponent| . 2)
   (|%%_SqTekIjtu_base-numerator| . 1)
   (expt . 2)
   (/ . 3)
   (|%%_SqbmoOitu_base| . 1)
   (log . 1)
   (|%%_Sqximfjtu_exponent| . 1)
   (* . 3)
   (exp . 1))
  ((|%%_SqBY9Zltu_loop| . 1)
   (|%%_Sq9xurhtu_integer-expt| . 1)
   (|%%_SqPAw-gtu_rational-expt| . 1)
   (|%%_SqtEyxgtu_general-expt| . 1))
  (numerator
    denominator
    integer?
    rational?
    quotient
    odd?
    zero?
    negative?
    ashl
    abs
    exact?
    =
    expt
    /
    *
    log
    exp)
  (#%define expt
    (#%letrec #t
      ((|%%_SqtEyxgtu_general-expt|
         (#%lambda #t
           (|%%_SqbmoOitu_base| |%%_Sqximfjtu_exponent|)
           ()
           (exp (* |%%_Sqximfjtu_exponent|
                   (log |%%_SqbmoOitu_base|)))))
       (|%%_SqPAw-gtu_rational-expt|
         (#%lambda #t
           (|%%_SqTekIjtu_base-numerator|
             |%%_Sqdbi9ktu_base-denominator|
             |%%_Sqz7gCktu_exponent|)
           ()
           (/ (expt |%%_SqTekIjtu_base-numerator|
                    |%%_Sqz7gCktu_exponent|)
              (expt |%%_Sqdbi9ktu_base-denominator|
                    |%%_Sqz7gCktu_exponent|))))
       (|%%_Sq9xurhtu_integer-expt|
         (#%lambda #t
           (|%%_SqV3e3ltu_base| |%%_Sqf0cwltu_exponent|)
           ()
           (#%if (#%if (exact? |%%_SqV3e3ltu_base|)
                   (= |%%_SqV3e3ltu_base| 2)
                   #f)
             (#%if (negative? |%%_Sqf0cwltu_exponent|)
               (/ (ashl 1 (abs |%%_Sqf0cwltu_exponent|)))
               (ashl 1 |%%_Sqf0cwltu_exponent|))
             ((#%letrec #t
                ((|%%_SqBY9Zltu_loop|
                   (#%lambda #t
                     (|%%_SqXU7qmtu_rest|
                       |%%_SqhR5Tmtu_result|
                       |%%_SqDN3kntu_squaring|)
                     (|%%_SqBY9Zltu_loop|)
                     (#%if (zero? |%%_SqXU7qmtu_rest|)
                       |%%_SqhR5Tmtu_result|
                       (|%%_SqBY9Zltu_loop|
                         (quotient |%%_SqXU7qmtu_rest| 2)
                         (#%if (odd? |%%_SqXU7qmtu_rest|)
                           (* |%%_SqhR5Tmtu_result| |%%_SqDN3kntu_squaring|)
                           |%%_SqhR5Tmtu_result|)
                         (* |%%_SqDN3kntu_squaring|
                            |%%_SqDN3kntu_squaring|))))))
                ()
                |%%_SqBY9Zltu_loop|)
              (#%if (negative? |%%_Sqf0cwltu_exponent|)
                (abs |%%_Sqf0cwltu_exponent|)
                |%%_Sqf0cwltu_exponent|)
              1
              (#%if (negative? |%%_Sqf0cwltu_exponent|)
                (/ |%%_SqV3e3ltu_base|)
                |%%_SqV3e3ltu_base|))))))
      ()
      (#%lambda #t
        (|%%_SqvtsUhtu_base| |%%_SqRpqlitu_exponent|)
        (|%%_Sq9xurhtu_integer-expt|
          |%%_SqPAw-gtu_rational-expt|
          |%%_SqtEyxgtu_general-expt|)
        (#%if (zero? |%%_SqRpqlitu_exponent|)
          (#%if (exact? |%%_SqRpqlitu_exponent|) 1 1.0)
          (#%if (zero? |%%_SqvtsUhtu_base|)
            (#%if (exact? |%%_SqRpqlitu_exponent|)
              |%%_SqvtsUhtu_base|
              0.0)
            (#%if (#%if (exact? |%%_SqvtsUhtu_base|)
                    (#%if (rational? |%%_SqvtsUhtu_base|)
                      (#%if (integer? |%%_SqvtsUhtu_base|) #f #t)
                      #f)
                    #f)
              (|%%_SqPAw-gtu_rational-expt|
                (numerator |%%_SqvtsUhtu_base|)
                (denominator |%%_SqvtsUhtu_base|)
                |%%_SqRpqlitu_exponent|)
              (#%if (#%if (exact? |%%_SqRpqlitu_exponent|)
                      (integer? |%%_SqRpqlitu_exponent|)
                      #f)
                (|%%_Sq9xurhtu_integer-expt|
                  |%%_SqvtsUhtu_base|
                  |%%_SqRpqlitu_exponent|)
                (|%%_SqtEyxgtu_general-expt|
                  |%%_SqvtsUhtu_base|
                  |%%_SqRpqlitu_exponent|)))))))))
(#%program
  ((- . 1)
   (|%%_Sq_yX7ptu_tmp| . 2)
   (/ . 2)
   (modpow . 2)
   (|%%_SqlvVAptu_tmp| . 2)
   (* . 3)
   (even? . 1)
   (|%%_SqFCZGotu_n| . 6)
   (|%%_SqZJ1Nntu_x| . 4)
   (modulo . 4)
   (|%%_SqjG_dotu_y| . 4)
   (= . 1))
  ()
  (even? modpow / * - modulo =)
  (#%define modpow
    (#%lambda #t
      (|%%_SqZJ1Nntu_x|
        |%%_SqjG_dotu_y|
        |%%_SqFCZGotu_n|)
      ()
      (#%if (= |%%_SqjG_dotu_y| 1)
        (modulo |%%_SqZJ1Nntu_x| |%%_SqFCZGotu_n|)
        (#%if (even? |%%_SqjG_dotu_y|)
          ((#%lambda #t
             (|%%_SqlvVAptu_tmp|)
             (|%%_SqFCZGotu_n|)
             (modulo
               (* |%%_SqlvVAptu_tmp| |%%_SqlvVAptu_tmp|)
               |%%_SqFCZGotu_n|))
           (modpow
             |%%_SqZJ1Nntu_x|
             (/ |%%_SqjG_dotu_y| 2)
             |%%_SqFCZGotu_n|))
          ((#%lambda #t
             (|%%_Sq_yX7ptu_tmp|)
             (|%%_SqFCZGotu_n| |%%_SqZJ1Nntu_x|)
             (modulo
               (* |%%_SqZJ1Nntu_x|
                  (modulo
                    (* |%%_Sq_yX7ptu_tmp| |%%_Sq_yX7ptu_tmp|)
                    |%%_SqFCZGotu_n|))
               |%%_SqFCZGotu_n|))
           (modpow
             |%%_SqZJ1Nntu_x|
             (/ (- |%%_SqjG_dotu_y| 1) 2)
             |%%_SqFCZGotu_n|)))))))
(#%program
  ((round . 1)
   (= . 1)
   (real? . 1)
   (|%%_SqHrT1qtu_n| . 4)
   (_integer? . 1))
  ()
  (real? round = _integer?)
  (#%define integer?
    (#%lambda #t
      (|%%_SqHrT1qtu_n|)
      ()
      (#%if (_integer? |%%_SqHrT1qtu_n|)
        #t
        (#%if (real? |%%_SqHrT1qtu_n|)
          (= (round |%%_SqHrT1qtu_n|) |%%_SqHrT1qtu_n|)
          #f)))))
(#%program
  ((complex? . 1)
   (|%%_Sq1oRuqtu_oldcomp?| . 1)
   (|%%_SqnkPXqtu_n| . 2)
   (number? . 1))
  ()
  (complex? number?)
  (#%define real?
    ((#%lambda #t
       (|%%_Sq1oRuqtu_oldcomp?|)
       ()
       (#%lambda #t
         (|%%_SqnkPXqtu_n|)
         (|%%_Sq1oRuqtu_oldcomp?|)
         (#%if (number? |%%_SqnkPXqtu_n|)
           (#%if (|%%_Sq1oRuqtu_oldcomp?| |%%_SqnkPXqtu_n|)
             #f
             #t)
           #f)))
     complex?)))
(#%program
  ((real? . 1))
  ()
  (real?)
  (#%define rational? real?))
(#%program
  ((number? . 1))
  ()
  (number?)
  (#%define complex? number?))
(#%program
  ((imag-part . 1)
   (real-part . 1)
   (|%%_Sqp9Jistu_b| . 2)
   (|%%_Sq3dLRrtu_a| . 2)
   (* . 2)
   (+ . 1)
   (sqrt . 1)
   (- . 1)
   (< . 1)
   (|%%_SqJgNortu_num| . 6)
   (real? . 1))
  ()
  (+ * sqrt real-part imag-part < - real?)
  (#%define abs
    (#%lambda #t
      (|%%_SqJgNortu_num|)
      ()
      (#%if (real? |%%_SqJgNortu_num|)
        (#%if (< |%%_SqJgNortu_num| 0)
          (- |%%_SqJgNortu_num|)
          |%%_SqJgNortu_num|)
        ((#%lambda #t
           (|%%_Sq3dLRrtu_a| |%%_Sqp9Jistu_b|)
           ()
           (sqrt (+ (* |%%_Sq3dLRrtu_a| |%%_Sq3dLRrtu_a|)
                    (* |%%_Sqp9Jistu_b| |%%_Sqp9Jistu_b|))))
         (real-part |%%_SqJgNortu_num|)
         (imag-part |%%_SqJgNortu_num|))))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define min (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define max (void)))
(#%program
  ((> . 1)
   (|%%_Sq7Tyzutu_x1| . 3)
   (|%%_SqNWA6utu_args| . 2)
   (max . 1)
   (< . 1)
   (|%%_Sqr-CFttu_x1| . 3)
   (|%%_Sq52Fcttu_args| . 2)
   (min . 1)
   (inexact? . 3)
   (cdr . 2)
   (|%%_SqL5HLstu__min_max| . 4)
   (car . 3)
   (|%%_SqtPw0vtu_proc| . 3)
   (exact->inexact . 1)
   (|%%_SqPLutvtu_mv| . 5)
   (exact? . 1)
   (|%%_SqvEqnwtu_inexact| . 3)
   (|%%_Sq9IsWvtu_args| . 6)
   (null? . 3))
  ((max . 1)
   (min . 1)
   (|%%_SqL5HLstu__min_max| . 1))
  (min <
       max
       >
       car
       inexact?
       cdr
       exact?
       exact->inexact
       null?)
  (#%letrec #t
    ((|%%_SqL5HLstu__min_max|
       (#%lambda #t
         (|%%_SqtPw0vtu_proc|
           |%%_SqPLutvtu_mv|
           |%%_Sq9IsWvtu_args|
           |%%_SqvEqnwtu_inexact|)
         (|%%_SqL5HLstu__min_max|)
         (#%if (null? |%%_Sq9IsWvtu_args|)
           (#%if (#%if |%%_SqvEqnwtu_inexact|
                   (exact? |%%_SqPLutvtu_mv|)
                   #f)
             (exact->inexact |%%_SqPLutvtu_mv|)
             |%%_SqPLutvtu_mv|)
           (#%if (|%%_SqtPw0vtu_proc|
                   (car |%%_Sq9IsWvtu_args|)
                   |%%_SqPLutvtu_mv|)
             (|%%_SqL5HLstu__min_max|
               |%%_SqtPw0vtu_proc|
               (car |%%_Sq9IsWvtu_args|)
               (cdr |%%_Sq9IsWvtu_args|)
               (#%if |%%_SqvEqnwtu_inexact|
                 #t
                 (inexact? (car |%%_Sq9IsWvtu_args|))))
             (|%%_SqL5HLstu__min_max|
               |%%_SqtPw0vtu_proc|
               |%%_SqPLutvtu_mv|
               (cdr |%%_Sq9IsWvtu_args|)
               |%%_SqvEqnwtu_inexact|))))))
    ()
    (#%begin
      (#%set! min
        (#%lambda #t
          (|%%_Sqr-CFttu_x1| . |%%_Sq52Fcttu_args|)
          (|%%_SqL5HLstu__min_max|)
          (#%if (null? |%%_Sq52Fcttu_args|)
            |%%_Sqr-CFttu_x1|
            (|%%_SqL5HLstu__min_max|
              <
              |%%_Sqr-CFttu_x1|
              |%%_Sq52Fcttu_args|
              (inexact? |%%_Sqr-CFttu_x1|)))))
      (#%set! max
        (#%lambda #t
          (|%%_Sq7Tyzutu_x1| . |%%_SqNWA6utu_args|)
          (|%%_SqL5HLstu__min_max|)
          (#%if (null? |%%_SqNWA6utu_args|)
            |%%_Sq7Tyzutu_x1|
            (|%%_SqL5HLstu__min_max|
              >
              |%%_Sq7Tyzutu_x1|
              |%%_SqNWA6utu_args|
              (inexact? |%%_Sq7Tyzutu_x1|))))))))
(#%program
  ((|%%_SqRAoQwtu_n| . 1) (< . 1))
  ()
  (<)
  (#%define negative?
    (#%lambda #t
      (|%%_SqRAoQwtu_n|)
      ()
      (< |%%_SqRAoQwtu_n| 0))))
(#%program
  ((|%%_Sqbxmhxtu_n| . 1) (> . 1))
  ()
  (>)
  (#%define positive?
    (#%lambda #t
      (|%%_Sqbxmhxtu_n|)
      ()
      (> |%%_Sqbxmhxtu_n| 0))))
(#%program
  ((|%%_SqxtkKxtu_n| . 1) (modulo . 1) (= . 1))
  ()
  (modulo =)
  (#%define even?
    (#%lambda #t
      (|%%_SqxtkKxtu_n|)
      ()
      (= 0 (modulo |%%_SqxtkKxtu_n| 2)))))
(#%program
  ((|%%_SqTpibytu_n| . 1) (even? . 1))
  ()
  (even?)
  (#%define odd?
    (#%lambda #t
      (|%%_SqTpibytu_n|)
      ()
      (#%if (even? |%%_SqTpibytu_n|) #f #t))))
(#%program
  ((|%%_SqdmgEytu_n| . 1) (= . 1))
  ()
  (=)
  (#%define zero?
    (#%lambda #t
      (|%%_SqdmgEytu_n|)
      ()
      (= |%%_SqdmgEytu_n| 0))))
(#%program
  ((|%%_Sqzie5ztu_n| . 1) (+ . 1))
  ()
  (+)
  (#%define add1
    (#%lambda #t
      (|%%_Sqzie5ztu_n|)
      ()
      (+ |%%_Sqzie5ztu_n| 1))))
(#%program
  ((|%%_SqVecyztu_n| . 1) (- . 1))
  ()
  (-)
  (#%define sub1
    (#%lambda #t
      (|%%_SqVecyztu_n|)
      ()
      (- |%%_SqVecyztu_n| 1))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define >= (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define <= (void)))
(#%program
  ((|%%_SqJrLTFtu_y| . 1)
   (|%%_SqnvNqFtu_x| . 1)
   (|%%_SqlGT3Etu_args| . 1)
   (|%%_SqHCRwEtu_loop| . 2)
   (cadr . 1)
   (car . 1)
   (|%%_SqjRZICtu_comparator| . 1)
   (|%%_SqFNX9Dtu_chainer| . 1)
   (cdr . 2)
   (|%%_Sq_JVCDtu_endstate| . 2)
   (|%%_Sq1zPZEtu_x| . 5)
   (null? . 2)
   (|%%_SqZU_fCtu_b| . 2)
   (|%%_SqDY1PBtu_a| . 2)
   (> . 1)
   (>= . 1)
   (|%%_SqB78sAtu__and2| . 2)
   (= . 2)
   (|%%_Sqh04mBtu_b| . 2)
   (|%%_SqX36VAtu_a| . 2)
   (< . 1)
   (|%%_Sqfba_ztu__comp_help| . 2)
   (<= . 1))
  ((|%%_SqHCRwEtu_loop| . 1) (>= . 1) (<= . 1))
  (null? cadr car cdr <= = < >= >)
  ((#%lambda #t
     (|%%_Sqfba_ztu__comp_help| |%%_SqB78sAtu__and2|)
     ()
     (#%begin
       (#%set! <=
         (|%%_Sqfba_ztu__comp_help|
           (#%lambda #t
             (|%%_SqX36VAtu_a| |%%_Sqh04mBtu_b|)
             ()
             (#%if (< |%%_SqX36VAtu_a| |%%_Sqh04mBtu_b|)
               #t
               (= |%%_SqX36VAtu_a| |%%_Sqh04mBtu_b|)))
           |%%_SqB78sAtu__and2|
           #t))
       (#%set! >=
         (|%%_Sqfba_ztu__comp_help|
           (#%lambda #t
             (|%%_SqDY1PBtu_a| |%%_SqZU_fCtu_b|)
             ()
             (#%if (> |%%_SqDY1PBtu_a| |%%_SqZU_fCtu_b|)
               #t
               (= |%%_SqDY1PBtu_a| |%%_SqZU_fCtu_b|)))
           |%%_SqB78sAtu__and2|
           #t))))
   (#%lambda #t
     (|%%_SqjRZICtu_comparator|
       |%%_SqFNX9Dtu_chainer|
       |%%_Sq_JVCDtu_endstate|)
     ()
     (#%lambda #t
       |%%_SqlGT3Etu_args|
       (|%%_Sq_JVCDtu_endstate|
         |%%_SqFNX9Dtu_chainer|
         |%%_SqjRZICtu_comparator|)
       ((#%letrec #t
          ((|%%_SqHCRwEtu_loop|
             (#%lambda #t
               (|%%_Sq1zPZEtu_x|)
               (|%%_SqHCRwEtu_loop|
                 |%%_Sq_JVCDtu_endstate|
                 |%%_SqFNX9Dtu_chainer|
                 |%%_SqjRZICtu_comparator|)
               (#%if (null? |%%_Sq1zPZEtu_x|)
                 |%%_Sq_JVCDtu_endstate|
                 (#%if (null? (cdr |%%_Sq1zPZEtu_x|))
                   |%%_Sq_JVCDtu_endstate|
                   (|%%_SqFNX9Dtu_chainer|
                     (|%%_SqjRZICtu_comparator|
                       (car |%%_Sq1zPZEtu_x|)
                       (cadr |%%_Sq1zPZEtu_x|))
                     (|%%_SqHCRwEtu_loop| (cdr |%%_Sq1zPZEtu_x|))))))))
          (|%%_Sq_JVCDtu_endstate|
            |%%_SqFNX9Dtu_chainer|
            |%%_SqjRZICtu_comparator|)
          |%%_SqHCRwEtu_loop|)
        |%%_SqlGT3Etu_args|)))
   (#%lambda #t
     (|%%_SqnvNqFtu_x| |%%_SqJrLTFtu_y|)
     ()
     (#%if |%%_SqnvNqFtu_x| |%%_SqJrLTFtu_y| #f))))
(#%program
  ((|%%_SqLgFeHtu_chainer| . 1)
   (apply . 1)
   (|%%_SqpkHNGtu_comparator| . 1)
   (cadr . 2)
   (car . 2)
   (= . 1)
   (cdr . 2)
   (|%%_Sq5dDHHtu_args| . 7)
   (null? . 2)
   (< . 1)
   (<= . 2)
   (> . 1)
   (|%%_Sq3oJkGtu__?=| . 2)
   (>= . 2))
  ((<= . 1) (>= . 1))
  (< <= <= > >= >= null? = car cadr apply cdr)
  ((#%lambda #t
     (|%%_Sq3oJkGtu__?=|)
     ()
     (#%begin
       (#%set! >= (|%%_Sq3oJkGtu__?=| > >=))
       (#%set! <= (|%%_Sq3oJkGtu__?=| < <=))))
   (#%lambda #t
     (|%%_SqpkHNGtu_comparator|
       |%%_SqLgFeHtu_chainer|)
     ()
     (#%lambda #t
       |%%_Sq5dDHHtu_args|
       (|%%_SqLgFeHtu_chainer|
         |%%_SqpkHNGtu_comparator|)
       (#%if (null? |%%_Sq5dDHHtu_args|)
         #t
         (#%if (null? (cdr |%%_Sq5dDHHtu_args|))
           #t
           (#%if (#%if (= (car |%%_Sq5dDHHtu_args|)
                          (cadr |%%_Sq5dDHHtu_args|))
                   #t
                   (|%%_SqpkHNGtu_comparator|
                     (car |%%_Sq5dDHHtu_args|)
                     (cadr |%%_Sq5dDHHtu_args|)))
             (apply |%%_SqLgFeHtu_chainer|
                    (cdr |%%_Sq5dDHHtu_args|))
             #f)))))))
(#%program
  ((gcd . 1)
   (apply . 1)
   (_gcd . 1)
   (car . 2)
   (cdr . 2)
   (|%%_SqN5zBItu_args| . 5)
   (null? . 2))
  ()
  (cdr car apply gcd _gcd null?)
  (#%define gcd
    (#%lambda #t
      |%%_SqN5zBItu_args|
      ()
      (#%if (null? |%%_SqN5zBItu_args|)
        0
        (#%if (null? (cdr |%%_SqN5zBItu_args|))
          (car |%%_SqN5zBItu_args|)
          (_gcd (car |%%_SqN5zBItu_args|)
                (apply gcd (cdr |%%_SqN5zBItu_args|))))))))
(#%program
  ((lcm . 1)
   (apply . 1)
   (_lcm . 1)
   (car . 2)
   (cdr . 2)
   (|%%_Sq72x2Jtu_args| . 5)
   (null? . 2))
  ()
  (cdr car apply lcm _lcm null?)
  (#%define lcm
    (#%lambda #t
      |%%_Sq72x2Jtu_args|
      ()
      (#%if (null? |%%_Sq72x2Jtu_args|)
        1
        (#%if (null? (cdr |%%_Sq72x2Jtu_args|))
          (car |%%_Sq72x2Jtu_args|)
          (_lcm (car |%%_Sq72x2Jtu_args|)
                (apply lcm (cdr |%%_Sq72x2Jtu_args|))))))))
(#%program
  ((|%%_Sqt-uvJtu_x| . 1)
   (remainder . 1)
   (+ . 1)
   (|%%_Sq9TqpKtu_r| . 3)
   (positive? . 1)
   (|%%_SqPWsYJtu_y| . 3)
   (negative? . 2))
  ()
  (remainder positive? negative? +)
  (#%define modulo
    (#%lambda #t
      (|%%_Sqt-uvJtu_x| |%%_SqPWsYJtu_y|)
      ()
      ((#%lambda #t
         (|%%_Sq9TqpKtu_r|)
         (|%%_SqPWsYJtu_y|)
         (#%if ((#%if (negative? |%%_SqPWsYJtu_y|)
                  positive?
                  negative?)
                |%%_Sq9TqpKtu_r|)
           (+ |%%_Sq9TqpKtu_r| |%%_SqPWsYJtu_y|)
           |%%_Sq9TqpKtu_r|))
       (remainder |%%_Sqt-uvJtu_x| |%%_SqPWsYJtu_y|)))))
(#%program
  ((- . 1)
   (char->integer . 4)
   (|%%_SqxEidMtu_c| . 2)
   (|%%_SqbIkMLtu_lc-offset| . 1)
   (+ . 1)
   (integer->char . 1)
   (|%%_SqRLmjLtu_z| . 1)
   (<= . 1)
   (|%%_SqvPoSKtu_a| . 2)
   (|%%_SqTAgGMtu_cv| . 3)
   (>= . 1))
  ()
  (integer->char + >= <= char->integer -)
  (#%define char-downcase
    ((#%lambda #t
       (|%%_SqvPoSKtu_a|)
       ()
       ((#%lambda #t
          (|%%_SqRLmjLtu_z|)
          (|%%_SqvPoSKtu_a|)
          ((#%lambda #t
             (|%%_SqbIkMLtu_lc-offset|)
             (|%%_SqRLmjLtu_z| |%%_SqvPoSKtu_a|)
             (#%lambda #t
               (|%%_SqxEidMtu_c|)
               (|%%_SqbIkMLtu_lc-offset|
                 |%%_SqRLmjLtu_z|
                 |%%_SqvPoSKtu_a|)
               ((#%lambda #t
                  (|%%_SqTAgGMtu_cv|)
                  (|%%_SqxEidMtu_c|
                    |%%_SqbIkMLtu_lc-offset|
                    |%%_SqRLmjLtu_z|
                    |%%_SqvPoSKtu_a|)
                  (#%if (#%if (>= |%%_SqTAgGMtu_cv| |%%_SqvPoSKtu_a|)
                          (<= |%%_SqTAgGMtu_cv| |%%_SqRLmjLtu_z|)
                          #f)
                    (integer->char
                      (+ |%%_SqTAgGMtu_cv| |%%_SqbIkMLtu_lc-offset|))
                    |%%_SqxEidMtu_c|))
                (char->integer |%%_SqxEidMtu_c|))))
           (- (char->integer #\a) |%%_SqvPoSKtu_a|)))
        (char->integer #\Z)))
     (char->integer #\A))))
(#%program
  ((char->integer . 4)
   (|%%_Sqfm8uOtu_c| . 2)
   (|%%_SqVpa1Otu_uc-offset| . 1)
   (- . 2)
   (integer->char . 1)
   (|%%_SqztcANtu_z| . 1)
   (<= . 1)
   (|%%_Sqdxe7Ntu_a| . 2)
   (|%%_SqBi6XOtu_cv| . 3)
   (>= . 1))
  ()
  (integer->char - >= <= char->integer)
  (#%define char-upcase
    ((#%lambda #t
       (|%%_Sqdxe7Ntu_a|)
       ()
       ((#%lambda #t
          (|%%_SqztcANtu_z|)
          (|%%_Sqdxe7Ntu_a|)
          ((#%lambda #t
             (|%%_SqVpa1Otu_uc-offset|)
             (|%%_SqztcANtu_z| |%%_Sqdxe7Ntu_a|)
             (#%lambda #t
               (|%%_Sqfm8uOtu_c|)
               (|%%_SqVpa1Otu_uc-offset|
                 |%%_SqztcANtu_z|
                 |%%_Sqdxe7Ntu_a|)
               ((#%lambda #t
                  (|%%_SqBi6XOtu_cv|)
                  (|%%_Sqfm8uOtu_c|
                    |%%_SqVpa1Otu_uc-offset|
                    |%%_SqztcANtu_z|
                    |%%_Sqdxe7Ntu_a|)
                  (#%if (#%if (>= |%%_SqBi6XOtu_cv| |%%_Sqdxe7Ntu_a|)
                          (<= |%%_SqBi6XOtu_cv| |%%_SqztcANtu_z|)
                          #f)
                    (integer->char
                      (- |%%_SqBi6XOtu_cv| |%%_SqVpa1Otu_uc-offset|))
                    |%%_Sqfm8uOtu_c|))
                (char->integer |%%_Sqfm8uOtu_c|))))
           (- |%%_Sqdxe7Ntu_a| (char->integer #\A))))
        (char->integer #\z)))
     (char->integer #\a))))
(#%program
  ((|%%_SqXe4oPtu_args| . 1)
   (map . 1)
   (|%%_Sqhb2RPtu_c2| . 1)
   (|%%_SqD70iQtu_c1| . 1)
   (char->integer . 3)
   (> . 1)
   (apply . 1))
  ()
  (map char->integer > apply)
  (#%define char>?
    (#%lambda #t
      (|%%_SqD70iQtu_c1|
        |%%_Sqhb2RPtu_c2|
        .
        |%%_SqXe4oPtu_args|)
      ()
      (apply >
             (char->integer |%%_SqD70iQtu_c1|)
             (char->integer |%%_Sqhb2RPtu_c2|)
             (map char->integer |%%_SqXe4oPtu_args|)))))
(#%program
  ((|%%_SqZ3-KQtu_args| . 1)
   (map . 1)
   (|%%_Sqj0YbRtu_c2| . 1)
   (|%%_SqFYVERtu_c1| . 1)
   (char->integer . 3)
   (< . 1)
   (apply . 1))
  ()
  (map char->integer < apply)
  (#%define char<?
    (#%lambda #t
      (|%%_SqFYVERtu_c1|
        |%%_Sqj0YbRtu_c2|
        .
        |%%_SqZ3-KQtu_args|)
      ()
      (apply <
             (char->integer |%%_SqFYVERtu_c1|)
             (char->integer |%%_Sqj0YbRtu_c2|)
             (map char->integer |%%_SqZ3-KQtu_args|)))))
(#%program
  ((char=? . 1)
   (|%%_SqlRRyStu_c2| . 2)
   (|%%_Sq_UT5Stu_c1| . 2)
   (char>? . 1))
  ()
  (char=? char>?)
  (#%define char>=?
    (#%lambda #t
      (|%%_Sq_UT5Stu_c1| |%%_SqlRRyStu_c2|)
      ()
      (#%if (char>? |%%_Sq_UT5Stu_c1| |%%_SqlRRyStu_c2|)
        #t
        (char=? |%%_Sq_UT5Stu_c1| |%%_SqlRRyStu_c2|)))))
(#%program
  ((char=? . 1)
   (|%%_Sq1KNsTtu_c2| . 2)
   (|%%_SqHNP_Stu_c1| . 2)
   (char<? . 1))
  ()
  (char=? char<?)
  (#%define char<=?
    (#%lambda #t
      (|%%_SqHNP_Stu_c1| |%%_Sq1KNsTtu_c2|)
      ()
      (#%if (char<? |%%_SqHNP_Stu_c1| |%%_Sq1KNsTtu_c2|)
        #t
        (char=? |%%_SqHNP_Stu_c1| |%%_Sq1KNsTtu_c2|)))))
(#%program
  ((|%%_SqnGLVTtu_args| . 1)
   (map . 1)
   (|%%_SqJCJmUtu_c2| . 1)
   (|%%_Sq3zHPUtu_c1| . 1)
   (char-downcase . 3)
   (char>? . 1)
   (apply . 1))
  ()
  (map char-downcase char>? apply)
  (#%define char-ci>?
    (#%lambda #t
      (|%%_Sq3zHPUtu_c1|
        |%%_SqJCJmUtu_c2|
        .
        |%%_SqnGLVTtu_args|)
      ()
      (apply char>?
             (char-downcase |%%_Sq3zHPUtu_c1|)
             (char-downcase |%%_SqJCJmUtu_c2|)
             (map char-downcase |%%_SqnGLVTtu_args|)))))
(#%program
  ((|%%_SqpvFgVtu_args| . 1)
   (map . 1)
   (|%%_SqLrDJVtu_c2| . 1)
   (|%%_Sq5oBaWtu_c1| . 1)
   (char-downcase . 3)
   (char<? . 1)
   (apply . 1))
  ()
  (map char-downcase char<? apply)
  (#%define char-ci<?
    (#%lambda #t
      (|%%_Sq5oBaWtu_c1|
        |%%_SqLrDJVtu_c2|
        .
        |%%_SqpvFgVtu_args|)
      ()
      (apply char<?
             (char-downcase |%%_Sq5oBaWtu_c1|)
             (char-downcase |%%_SqLrDJVtu_c2|)
             (map char-downcase |%%_SqpvFgVtu_args|)))))
(#%program
  ((|%%_SqrkzDWtu_args| . 1)
   (map . 1)
   (|%%_SqNgx4Xtu_c2| . 1)
   (|%%_Sq7dvxXtu_c1| . 1)
   (char-downcase . 3)
   (char=? . 1)
   (apply . 1))
  ()
  (map char-downcase char=? apply)
  (#%define char-ci=?
    (#%lambda #t
      (|%%_Sq7dvxXtu_c1|
        |%%_SqNgx4Xtu_c2|
        .
        |%%_SqrkzDWtu_args|)
      ()
      (apply char=?
             (char-downcase |%%_Sq7dvxXtu_c1|)
             (char-downcase |%%_SqNgx4Xtu_c2|)
             (map char-downcase |%%_SqrkzDWtu_args|)))))
(#%program
  ((char-ci=? . 1)
   (|%%_SqP5rrYtu_c2| . 2)
   (|%%_Sqt9t-Xtu_c1| . 2)
   (char-ci>? . 1))
  ()
  (char-ci=? char-ci>?)
  (#%define char-ci>=?
    (#%lambda #t
      (|%%_Sqt9t-Xtu_c1| |%%_SqP5rrYtu_c2|)
      ()
      (#%if (char-ci>? |%%_Sqt9t-Xtu_c1| |%%_SqP5rrYtu_c2|)
        #t
        (char-ci=? |%%_Sqt9t-Xtu_c1| |%%_SqP5rrYtu_c2|)))))
(#%program
  ((char-ci=? . 1)
   (|%%_Sqv-mlZtu_c2| . 2)
   (|%%_Sq92pUYtu_c1| . 2)
   (char-ci<? . 1))
  ()
  (char-ci=? char-ci<?)
  (#%define char-ci<=?
    (#%lambda #t
      (|%%_Sq92pUYtu_c1| |%%_Sqv-mlZtu_c2|)
      ()
      (#%if (char-ci<? |%%_Sq92pUYtu_c1| |%%_Sqv-mlZtu_c2|)
        #t
        (char-ci=? |%%_Sq92pUYtu_c1| |%%_Sqv-mlZtu_c2|)))))
(#%program
  ((|%%_SqRWkOZtu_c| . 2) (char<? . 2))
  ()
  (char<?)
  (#%define char-alphabetic?
    (#%lambda #t
      (|%%_SqRWkOZtu_c|)
      ()
      (#%if (char<? #\@ |%%_SqRWkOZtu_c| #\[)
        #t
        (char<? #\` |%%_SqRWkOZtu_c| #\{)))))
(#%program
  ((|%%_SqbTif-tu_c| . 1) (char<? . 1))
  ()
  (char<?)
  (#%define char-numeric?
    (#%lambda #t
      (|%%_SqbTif-tu_c|)
      ()
      (char<? #\/ |%%_SqbTif-tu_c| #\:))))
(#%program
  ((|%%_SqxPgI-tu_c| . 4) (char=? . 4))
  ()
  (char=?)
  (#%define char-whitespace?
    (#%lambda #t
      (|%%_SqxPgI-tu_c|)
      ()
      (#%if (char=? |%%_SqxPgI-tu_c| #\space)
        #t
        (#%if (char=? |%%_SqxPgI-tu_c| #\tab)
          #t
          (#%if (char=? |%%_SqxPgI-tu_c| #\newline)
            #t
            (char=? |%%_SqxPgI-tu_c| #\return)))))))
(#%program
  ((|%%_SqTLe9_tu_c| . 1) (char<? . 1))
  ()
  (char<?)
  (#%define char-upper-case?
    (#%lambda #t
      (|%%_SqTLe9_tu_c|)
      ()
      (char<? #\@ |%%_SqTLe9_tu_c| #\[))))
(#%program
  ((|%%_SqdIcC_tu_c| . 1) (char<? . 1))
  ()
  (char<?)
  (#%define char-lower-case?
    (#%lambda #t
      (|%%_SqdIcC_tu_c|)
      ()
      (char<? #\` |%%_SqdIcC_tu_c| #\{))))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define string-downcase (void)))
(#%program
  ((void . 1))
  ()
  (void)
  (#%define string-upcase (void)))
(#%program
  ((char-upcase . 1)
   (|%%_SqXp2T1uu_newstr| . 1)
   (|%%_SqBt4q1uu_str| . 3)
   (string-upcase . 1)
   (make-string . 2)
   (string-length . 4)
   (char-downcase . 1)
   (|%%_Sqfx6Z0uu_newstr| . 1)
   (|%%_SqVA8w0uu_str| . 3)
   (string-downcase . 1)
   (+ . 1)
   (|%%_SqzEa30uu_string-map| . 3)
   (|%%_Sqhm0k2uu_strsrc| . 2)
   (string-ref . 1)
   (|%%_SqZeYd3uu_proc| . 2)
   (|%%_SqDi-M2uu_strdst| . 3)
   (string-set! . 1)
   (|%%_SqF7U74uu_l| . 2)
   (|%%_SqjbWG3uu_n| . 4)
   (< . 1))
  ((string-upcase . 1)
   (string-downcase . 1)
   (|%%_SqzEa30uu_string-map| . 1))
  (string-downcase
    string-length
    char-downcase
    make-string
    string-upcase
    char-upcase
    string-ref
    string-set!
    +
    <)
  (#%letrec #t
    ((|%%_SqzEa30uu_string-map|
       (#%lambda #t
         (|%%_Sqhm0k2uu_strsrc|
           |%%_SqDi-M2uu_strdst|
           |%%_SqZeYd3uu_proc|
           |%%_SqjbWG3uu_n|
           |%%_SqF7U74uu_l|)
         (|%%_SqzEa30uu_string-map|)
         (#%if (< |%%_SqjbWG3uu_n| |%%_SqF7U74uu_l|)
           (#%begin
             (string-set!
               |%%_SqDi-M2uu_strdst|
               |%%_SqjbWG3uu_n|
               (|%%_SqZeYd3uu_proc|
                 (string-ref
                   |%%_Sqhm0k2uu_strsrc|
                   |%%_SqjbWG3uu_n|)))
             (|%%_SqzEa30uu_string-map|
               |%%_Sqhm0k2uu_strsrc|
               |%%_SqDi-M2uu_strdst|
               |%%_SqZeYd3uu_proc|
               (+ |%%_SqjbWG3uu_n| 1)
               |%%_SqF7U74uu_l|))
           |%%_SqDi-M2uu_strdst|))))
    ()
    (#%begin
      (#%set! string-downcase
        (#%lambda #t
          (|%%_SqVA8w0uu_str|)
          (|%%_SqzEa30uu_string-map|)
          ((#%lambda #t
             (|%%_Sqfx6Z0uu_newstr|)
             (|%%_SqVA8w0uu_str| |%%_SqzEa30uu_string-map|)
             (|%%_SqzEa30uu_string-map|
               |%%_SqVA8w0uu_str|
               |%%_Sqfx6Z0uu_newstr|
               char-downcase
               0
               (string-length |%%_SqVA8w0uu_str|)))
           (make-string (string-length |%%_SqVA8w0uu_str|)))))
      (#%set! string-upcase
        (#%lambda #t
          (|%%_SqBt4q1uu_str|)
          (|%%_SqzEa30uu_string-map|)
          ((#%lambda #t
             (|%%_SqXp2T1uu_newstr|)
             (|%%_SqBt4q1uu_str| |%%_SqzEa30uu_string-map|)
             (|%%_SqzEa30uu_string-map|
               |%%_SqBt4q1uu_str|
               |%%_SqXp2T1uu_newstr|
               char-upcase
               0
               (string-length |%%_SqBt4q1uu_str|)))
           (make-string (string-length |%%_SqBt4q1uu_str|))))))))
(#%program
  ((|%%_SqHYNu5uu_s2| . 1)
   (|%%_Sql0Q15uu_s1| . 1)
   (string->list . 2)
   (car . 2)
   (cdr . 2)
   (|%%_Sq_3SA4uu_s<?| . 2)
   (char>? . 1)
   (|%%_Sq3KFi7uu_c2| . 2)
   (|%%_SqJNHR6uu_c1| . 2)
   (char<? . 1)
   (|%%_SqnRJo6uu_s2| . 4)
   (|%%_Sq1VLX5uu_s1| . 3)
   (null? . 3))
  ((|%%_Sq_3SA4uu_s<?| . 1))
  (string->list car char<? cdr char>? null?)
  (#%define string<?
    (#%letrec #t
      ((|%%_Sq_3SA4uu_s<?|
         (#%lambda #t
           (|%%_Sq1VLX5uu_s1| |%%_SqnRJo6uu_s2|)
           (|%%_Sq_3SA4uu_s<?|)
           (#%if (null? |%%_Sq1VLX5uu_s1|)
             (#%if (null? |%%_SqnRJo6uu_s2|) #f #t)
             (#%if (null? |%%_SqnRJo6uu_s2|)
               #f
               ((#%lambda #t
                  (|%%_SqJNHR6uu_c1| |%%_Sq3KFi7uu_c2|)
                  (|%%_SqnRJo6uu_s2|
                    |%%_Sq1VLX5uu_s1|
                    |%%_Sq_3SA4uu_s<?|)
                  (#%if (char<? |%%_SqJNHR6uu_c1| |%%_Sq3KFi7uu_c2|)
                    #t
                    (#%if (char>? |%%_SqJNHR6uu_c1| |%%_Sq3KFi7uu_c2|)
                      #f
                      (|%%_Sq_3SA4uu_s<?|
                        (cdr |%%_Sq1VLX5uu_s1|)
                        (cdr |%%_SqnRJo6uu_s2|)))))
                (car |%%_Sq1VLX5uu_s1|)
                (car |%%_SqnRJo6uu_s2|)))))))
      ()
      (#%lambda #t
        (|%%_Sql0Q15uu_s1| |%%_SqHYNu5uu_s2|)
        (|%%_Sq_3SA4uu_s<?|)
        (|%%_Sq_3SA4uu_s<?|
          (string->list |%%_Sql0Q15uu_s1|)
          (string->list |%%_SqHYNu5uu_s2|))))))
(#%program
  ((|%%_Sq5zzF8uu_s2| . 1)
   (|%%_SqLCBc8uu_s1| . 1)
   (string->list . 2)
   (car . 2)
   (cdr . 2)
   (|%%_SqpGDL7uu_s>?| . 2)
   (char<? . 1)
   (|%%_Sqtkrtauu_c2| . 2)
   (|%%_Sq7ot0auu_c1| . 2)
   (char>? . 1)
   (|%%_Sqrvx69uu_s1| . 4)
   (|%%_SqNrvz9uu_s2| . 3)
   (null? . 3))
  ((|%%_SqpGDL7uu_s>?| . 1))
  (string->list car char>? cdr char<? null?)
  (#%define string>?
    (#%letrec #t
      ((|%%_SqpGDL7uu_s>?|
         (#%lambda #t
           (|%%_Sqrvx69uu_s1| |%%_SqNrvz9uu_s2|)
           (|%%_SqpGDL7uu_s>?|)
           (#%if (null? |%%_SqNrvz9uu_s2|)
             (#%if (null? |%%_Sqrvx69uu_s1|) #f #t)
             (#%if (null? |%%_Sqrvx69uu_s1|)
               #f
               ((#%lambda #t
                  (|%%_Sq7ot0auu_c1| |%%_Sqtkrtauu_c2|)
                  (|%%_SqNrvz9uu_s2|
                    |%%_Sqrvx69uu_s1|
                    |%%_SqpGDL7uu_s>?|)
                  (#%if (char>? |%%_Sq7ot0auu_c1| |%%_Sqtkrtauu_c2|)
                    #t
                    (#%if (char<? |%%_Sq7ot0auu_c1| |%%_Sqtkrtauu_c2|)
                      #f
                      (|%%_SqpGDL7uu_s>?|
                        (cdr |%%_Sqrvx69uu_s1|)
                        (cdr |%%_SqNrvz9uu_s2|)))))
                (car |%%_Sqrvx69uu_s1|)
                (car |%%_SqNrvz9uu_s2|)))))))
      ()
      (#%lambda #t
        (|%%_SqLCBc8uu_s1| |%%_Sq5zzF8uu_s2|)
        (|%%_SqpGDL7uu_s>?|)
        (|%%_SqpGDL7uu_s>?|
          (string->list |%%_SqLCBc8uu_s1|)
          (string->list |%%_Sq5zzF8uu_s2|))))))
(#%program
  ((string=? . 1)
   (|%%_Sq9dnnbuu_s2| . 2)
   (|%%_SqPgpWauu_s1| . 2)
   (string<? . 1))
  ()
  (string=? string<?)
  (#%define string<=?
    (#%lambda #t
      (|%%_SqPgpWauu_s1| |%%_Sq9dnnbuu_s2|)
      ()
      (#%if (string<? |%%_SqPgpWauu_s1| |%%_Sq9dnnbuu_s2|)
        #t
        (string=? |%%_SqPgpWauu_s1| |%%_Sq9dnnbuu_s2|)))))
(#%program
  ((string=? . 1)
   (|%%_SqR5jhcuu_s2| . 2)
   (|%%_Sqv9lQbuu_s1| . 2)
   (string>? . 1))
  ()
  (string=? string>?)
  (#%define string>=?
    (#%lambda #t
      (|%%_Sqv9lQbuu_s1| |%%_SqR5jhcuu_s2|)
      ()
      (#%if (string>? |%%_Sqv9lQbuu_s1| |%%_SqR5jhcuu_s2|)
        #t
        (string=? |%%_Sqv9lQbuu_s1| |%%_SqR5jhcuu_s2|)))))
(#%program
  ((|%%_Sqx-ebduu_s2| . 1)
   (|%%_Sqb2hKcuu_s1| . 1)
   (string-downcase . 2)
   (string=? . 1))
  ()
  (string-downcase string=?)
  (#%define string-ci=?
    (#%lambda #t
      (|%%_Sqb2hKcuu_s1| |%%_Sqx-ebduu_s2|)
      ()
      (string=?
        (string-downcase |%%_Sqb2hKcuu_s1|)
        (string-downcase |%%_Sqx-ebduu_s2|)))))
(#%program
  ((|%%_SqdTa5euu_s2| . 1)
   (|%%_SqTWcEduu_s1| . 1)
   (string-downcase . 2)
   (string<? . 1))
  ()
  (string-downcase string<?)
  (#%define string-ci<?
    (#%lambda #t
      (|%%_SqTWcEduu_s1| |%%_SqdTa5euu_s2|)
      ()
      (string<?
        (string-downcase |%%_SqTWcEduu_s1|)
        (string-downcase |%%_SqdTa5euu_s2|)))))
(#%program
  ((|%%_SqVL6_euu_s2| . 1)
   (|%%_SqzP8yeuu_s1| . 1)
   (string-downcase . 2)
   (string>? . 1))
  ()
  (string-downcase string>?)
  (#%define string-ci>?
    (#%lambda #t
      (|%%_SqzP8yeuu_s1| |%%_SqVL6_euu_s2|)
      ()
      (string>?
        (string-downcase |%%_SqzP8yeuu_s1|)
        (string-downcase |%%_SqVL6_euu_s2|)))))
(#%program
  ((|%%_SqBE2Vfuu_s2| . 1)
   (|%%_SqfI4sfuu_s1| . 1)
   (string-downcase . 2)
   (string>=? . 1))
  ()
  (string-downcase string>=?)
  (#%define string-ci>=?
    (#%lambda #t
      (|%%_SqfI4sfuu_s1| |%%_SqBE2Vfuu_s2|)
      ()
      (string>=?
        (string-downcase |%%_SqfI4sfuu_s1|)
        (string-downcase |%%_SqBE2Vfuu_s2|)))))
(#%program
  ((|%%_Sqhx-Oguu_s2| . 1)
   (|%%_SqXA0mguu_s1| . 1)
   (string-downcase . 2)
   (string<=? . 1))
  ()
  (string-downcase string<=?)
  (#%define string-ci<=?
    (#%lambda #t
      (|%%_SqXA0mguu_s1| |%%_Sqhx-Oguu_s2|)
      ()
      (string<=?
        (string-downcase |%%_SqXA0mguu_s1|)
        (string-downcase |%%_Sqhx-Oguu_s2|)))))
(#%program
  ((- . 1)
   (make-string . 1)
   (|%%_SqFiSCiuu_end| . 2)
   (|%%_SqjmU9iuu_start| . 2)
   (|%%_Sq_eQ3juu_newstr| . 2)
   (|%%_SqZpWIhuu_str| . 1)
   (+ . 2)
   (|%%_SqDtYfhuu_fill-string| . 2)
   (|%%_SqlbOwjuu_sstr| . 2)
   (string-ref . 1)
   (|%%_Sq14Kqkuu_n| . 2)
   (|%%_SqH7MZjuu_dstr| . 2)
   (string-set! . 1)
   (|%%_SqJYFkluu_e| . 2)
   (|%%_Sqn0ITkuu_s| . 3)
   (< . 1))
  ((|%%_SqDtYfhuu_fill-string| . 1))
  (make-string - string-ref string-set! + <)
  (#%define substring
    (#%letrec #t
      ((|%%_SqDtYfhuu_fill-string|
         (#%lambda #t
           (|%%_SqlbOwjuu_sstr|
             |%%_SqH7MZjuu_dstr|
             |%%_Sq14Kqkuu_n|
             |%%_Sqn0ITkuu_s|
             |%%_SqJYFkluu_e|)
           (|%%_SqDtYfhuu_fill-string|)
           (#%if (< |%%_Sqn0ITkuu_s| |%%_SqJYFkluu_e|)
             (#%begin
               (string-set!
                 |%%_SqH7MZjuu_dstr|
                 |%%_Sq14Kqkuu_n|
                 (string-ref |%%_SqlbOwjuu_sstr| |%%_Sqn0ITkuu_s|))
               (|%%_SqDtYfhuu_fill-string|
                 |%%_SqlbOwjuu_sstr|
                 |%%_SqH7MZjuu_dstr|
                 (+ |%%_Sq14Kqkuu_n| 1)
                 (+ |%%_Sqn0ITkuu_s| 1)
                 |%%_SqJYFkluu_e|))
             #!void))))
      ()
      (#%lambda #t
        (|%%_SqZpWIhuu_str|
          |%%_SqjmU9iuu_start|
          |%%_SqFiSCiuu_end|)
        (|%%_SqDtYfhuu_fill-string|)
        ((#%lambda #t
           (|%%_Sq_eQ3juu_newstr|)
           (|%%_SqFiSCiuu_end|
             |%%_SqjmU9iuu_start|
             |%%_SqZpWIhuu_str|
             |%%_SqDtYfhuu_fill-string|)
           (#%begin
             (|%%_SqDtYfhuu_fill-string|
               |%%_SqZpWIhuu_str|
               |%%_Sq_eQ3juu_newstr|
               0
               |%%_SqjmU9iuu_start|
               |%%_SqFiSCiuu_end|)
             |%%_Sq_eQ3juu_newstr|))
         (make-string
           (- |%%_SqFiSCiuu_end| |%%_SqjmU9iuu_start|)))))))
(#%program
  ((- . 1)
   (cdr . 1)
   (list-ref . 1)
   (|%%_Sq3VDNluu_list| . 2)
   (car . 1)
   (|%%_SqpRBemuu_n| . 2)
   (zero? . 1))
  ()
  (list-ref cdr - car zero?)
  (#%define list-ref
    (#%lambda #t
      (|%%_Sq3VDNluu_list| |%%_SqpRBemuu_n|)
      ()
      (#%if (zero? |%%_SqpRBemuu_n|)
        (car |%%_Sq3VDNluu_list|)
        (list-ref
          (cdr |%%_Sq3VDNluu_list|)
          (- |%%_SqpRBemuu_n| 1))))))
(#%program
  ((|%%_SqLNzHmuu_args| . 1)
   (|%%_Sq5Kx8nuu_k| . 1)
   (apply . 1)
   (call-with-current-continuation . 1))
  ()
  (apply call-with-current-continuation)
  (#%define values
    (#%lambda #t
      |%%_SqLNzHmuu_args|
      ()
      (call-with-current-continuation
        (#%lambda #t
          (|%%_Sq5Kx8nuu_k|)
          (|%%_SqLNzHmuu_args|)
          (apply |%%_Sq5Kx8nuu_k| |%%_SqLNzHmuu_args|))))))
