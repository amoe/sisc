;;; Display an array of any dimension. The format is such as attempts
;;; to display the dimensions and indices, too - more informative than
;;; pretty, perhaps. The playful name should suggest that this is for
;;; playing around with the system, not for a public library.

;;; Jussi Piitulainen, October 2001.

(define (play arr)
  (let ((low (- (array-rank arr) 1))
        (shp (array-shape arr)))
    (define (play ks dim)
      (let* ((b (array-ref shp dim 0))
             (e (array-ref shp dim 1)))
        (write-prefix (reverse ks) b (- low dim))
        (if (= dim low)
            (let ((vec (share-array
                        arr
                        (shape b e)
                        (lambda (k)
                          (apply values (reverse (cons k ks)))))))
              (do ((k b (+ k 1)))
                ((= k e))
                (write-char #\space)
                (write (array-ref vec k)))
              (newline))
            (do ((k b (+ k 1)))
              ((= k e))
              (play (cons k ks) (+ dim 1))))))
    (define (write-prefix ks k cs)
      (for-each (lambda (k) (write k) (write-char #\:)) ks)
      (write k) (write-char #\.) (write-char #\.)
      (do ((cs cs (- cs 1))) ((= cs 0)) (write-char #\:))
      (if (> cs 0) (newline)))
    (if (zero? (array-rank arr))
        (begin
          (write (array-ref arr))
          (newline))
        (play '() 0))))
