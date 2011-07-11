(import s2j)
(import type-system)
(import s2j-reflection)

;;constructor
(define-java-class <java.lang.object>)
(define-java-class <java.lang.string-buffer>)
(define sb (java-new <java.lang.string-buffer> (->jstring "foo")))

;;method invocation
(define-generic-java-method jappend append)
(jappend sb (->jstring "foo"))
(jappend sb sb)

;;static method invocation
(define-java-class <java.lang.string>)
(define-generic-java-method value-of)
(value-of (java-null <java.lang.string>) (->jint 1234))

;array creation and access
(define a (java-array-new <jint> '#(2 2 2)))
(java-array-ref a '#(1 1 1))
(java-array-set! a '#(1 1 1) (->jint 1))
(java-array-ref a '(1 1 1))
(map ->number (->list (->jarray (map ->jint (iota 10)) <jint>)))

;;access to instance fields
;;the only reason we use sisc's symbol class for testing is that the
;;JDK has no classes with public instance fields and I didn't want to
;;create a class just for testing.
(define-java-class <sisc.data.symbol>)
(define-generic-java-field-accessor :symval)
(define-generic-java-field-modifier :symval!)
(define s (java-new <sisc.data.symbol> (->jstring "foo")))
(:symval s)
(:symval! s (->jstring "bar"))
(:symval s)

;;access to static fields
(define-java-class <sisc.data.scheme-string>)
(define-generic-java-field-accessor :compact-representation)
(define-generic-java-field-modifier :compact-representation!)
(:compact-representation (java-null <sisc.data.scheme-string>))
(:compact-representation! (java-null <sisc.data.scheme-string>)
                          (->jboolean #t))
(:compact-representation (java-null <sisc.data.scheme-string>))

;;equality
(define-java-class <java.util.date>)
(define-generic-java-method get-time)
(define d (java-new <java.util.date>))
(define now (get-time d))
(define d1 (java-new <java.util.date> now))
(define d2 (java-new <java.util.date> now))
(eq? d1 d2) ;=> #f
(eqv? d1 d2) ;=> #f
(equal? d1 d2) ;=> #t
(eq? (get-time d) (get-time d)) ;=> #f
(eqv? (get-time d) (get-time d)) ;=> #t
(equal? (get-time d) (get-time d)) ;=> #t

;;use of null
(value-of (java-null <java.lang.string>) jnull) ;=> #<java java.lang.String null>
(define aa (java-array-new <java.lang.string> '#(2 2 2)))
(java-array-ref aa '#(0 0 0)) ;=> #<jnull java.lang.String>
(type-of (java-null <java.lang.string>)) ;=> #<java java.lang.Class java.lang.String>

;;equality on null
(define jstring-null (java-null <java.lang.string>))
(eq? jstring-null jnull) ;=> #f
(eqv? jstring-null jnull) ;=> #f
(equal? jstring-null jnull) ;=> #t

;;widening conversion of primitive types is reflected in the
;;inheritance hierarchy
(java-class-precedence-list <jbyte>)
;;=>
#|(#<java java.lang.Class byte>
   #<java java.lang.Class short>
   #<java java.lang.Class int>
   #<java java.lang.Class long>
   #<java java.lang.Class float>
   #<java java.lang.Class double>) |#
(java-class-precedence-list <jchar>)
;;=>
#|(#<java java.lang.Class char>
   #<java java.lang.Class int>
   #<java java.lang.Class long>
   #<java java.lang.Class float>
   #<java java.lang.Class double>)|#

;;primitive types do not inherit from the java.lang types
(define-java-class <java.lang.integer>)
(instance-of? (->jint 1) <jint>) ;=> #t
(instance-of? (->jint 1) <java.lang.integer>) ;=> #f

;;widening conversion of array classes is reflected in the inheritance
;;hierarchy
(define-java-class <java.util.array-list**>)
(java-class-precedence-list <java.util.array-list**>)
;;=>
#|(#<java java.lang.Class java.util.ArrayList[][]>
   #<java java.lang.Class java.util.AbstractList[][]>
   #<java java.lang.Class java.util.AbstractCollection[][]>
   #<java java.lang.Class java.util.List[][]>
   #<java java.lang.Class java.util.RandomAccess[][]>
   #<java java.lang.Class java.lang.Cloneable[][]>
   #<java java.lang.Class java.io.Serializable[][]>
   #<java java.lang.Class java.lang.Object[][]>
   #<java java.lang.Class java.util.Collection[][]>
   #<java java.lang.Class java.lang.Object[]>
   #<java java.lang.Class java.io.Serializable[]>
   #<java java.lang.Class java.lang.Cloneable[]>
   #<java java.lang.Class java.lang.Object>
   #<java java.lang.Class java.io.Serializable>
   #<java java.lang.Class java.lang.Cloneable>)|#

;;wrapping and unwrapping of Scheme objects
(define a (java-array-new <java.lang.object> '#(1)))
(java-array-set! a '#(0) (java-wrap 'foo))
(java-unwrap (java-array-ref a '#(0))) ;=> 'foo

;;implementing Java interfaces in Scheme
(define-java-classes
  <java.util.comparator>
  <java.util.arrays>)
(define-java-proxy (comparator fn)
  (<java.util.comparator>)
  (define (.compare p obj1 obj2)
    (let ([x (java-unwrap obj1)]
          [y (java-unwrap obj2)])
      (->jint (cond [(fn x y) -1]
                    [(fn y x) +1]
                    [else 0])))))
(define-generic-java-method sort)
(define-java-class <java.lang.object>)
(define (list-sort fn l)
  (let ([a (->jarray (map java-wrap l) <java.lang.object>)])
    (sort (java-null <java.util.arrays>) a (comparator fn))
    (map java-unwrap (->list a))))
(list-sort < '(3 4 2 1))
(list-sort string<? '("foo" "bar" "baz"))

;;exception handling
(define-generic-java-method char-at)
(with/fc print-error
  (lambda () (char-at (->jstring "foo") (->jint 3))))

;;throwing exceptions from within proxy
(define-java-classes
  <java.util.iterator>
  <java.lang.unsupported-operation-exception>
  <java.util.no-such-element-exception>)
(define-java-proxy (list-iterator l)
  (<java.util.iterator>)
  (define (has-next i)
    (->jboolean (not (null? l))))
  (define (next i)
    (if (null? l)
        (error (java-new <java.util.no-such-element-exception>))
        (let ([res (car l)])
          (begin (set! l (cdr l))
                 (java-wrap res)))))
  (define (remove i)
    (error (java-new <java.lang.unsupported-operation-exception>)))
  (define (to-string i) (->jstring 'list-iterator)))
(define-generic-java-methods
  has-next
  next
  remove)
(define i (list-iterator '(1 2 3)))
(has-next i)
(next i)
(next i)
(next i)

;;overriding of toString
(display i) ;;-> #<java $Proxy1 list-iterator>

;;catching an exception thrown by a proxy.
(with/fc print-error
  (lambda () (remove i)))

;;nice handling of scheme exceptions thrown by a proxy
(define-java-class <java.lang.runnable>)
(define-generic-java-method run)
(define-java-proxy (runnable thunk)
  (<java.lang.runnable>)
  (define run
    (lambda ()
      (thunk)
      (void))))
(run (runnable (lambda () 1)))
;;the above should report something like
;;Error in java/invoke-method: Scheme invocation target exception
;;---------------------------
;;console:16:1: <from call to run>
;;===========================
;;Caused by Error: expected 0 argument(s) to #<procedure>, got 1.
(define-java-proxy (runnable thunk)
  (<java.lang.runnable>)
  (define run
    (lambda (x)
      (thunk)
      (void))))
(run (runnable (lambda () (/ 1 0))))
;;the above should report something like
;;Error in java/invoke-method: Scheme invocation target exception
;;---------------------------
;;console:25:1: <from call to run>
;;===========================
;;Caused by Error in /: <java.lang.ArithmeticException>: division by zero.
;;---------------------------
;;console:25:27: <from call to />
;;console:22:7: <indeterminate call>
(define-java-proxy (runnable thunk)
  (<java.lang.runnable>))
(run (runnable (lambda () 1)))
;;the above should throw a *java* UnsupportedOperationException.

;garbage collection
(let loop ([count 100])
  (if (> count 0)
      (begin
        (java-array-new <jdouble> 10000)
        (java-new <java.lang.string-buffer> (->jint 10000))
        (loop (- count 1)))))

;swing
(define-java-classes
  (<j-frame>            |javax.swing.JFrame|)
  (<j-button>           |javax.swing.JButton|)
  (<j-label>            |javax.swing.JLabel|)
  (<j-panel>            |javax.swing.JPanel|)
  (<border-factory>     |javax.swing.BorderFactory|)
  (<border-layout>      |java.awt.BorderLayout|)
  (<grid-layout>        |java.awt.GridLayout|)
  (<key-event>          |java.awt.event.KeyEvent|)
  <java.awt.event.action-listener>)
(define-generic-java-methods
  get-content-pane
  set-text
  set-mnemonic
  set-label-for
  set-layout
  set-border
  pack
  add
  show
  add-action-listener
  create-empty-border)
(define-generic-java-field-accessors
  (:CENTER |CENTER|)
  (:VK_I |VK_I|))
(define (set-label-text label clicks)
  (set-text label (->jstring (string-append
                              "Number of button clicks: "
                              (number->string clicks)))))
(define-java-proxy (click-counter label clicks)
  (<java.awt.event.action-listener>)
  (define (action-performed p e)
    (set! clicks (+ clicks 1))
    (set-label-text label clicks)))
(define (create-components)
  (let ([label  (java-new <j-label>)]
        [button (java-new <j-button> (->jstring
                                      "I am a Swing button!"))]
        [pane   (java-new <j-panel>)])
    (set-label-text label 0)
    (set-mnemonic button (:VK_I (java-null <key-event>)))
    (add-action-listener button (click-counter label 0)) 
    (set-label-for label button)
    (set-border pane (create-empty-border (java-null <border-factory>)
                                          (->jint 30)
                                          (->jint 30)
                                          (->jint 10)
                                          (->jint 30)))
    (set-layout pane (java-new <grid-layout> (->jint 0) (->jint 1)))
    (add pane button)
    (add pane label)
    pane))
(let ([frame (java-new <j-frame> (->jstring "SwingApplication"))])
  (add (get-content-pane frame)
       (create-components)
       (:CENTER (java-null <border-layout>)))
  (pack frame)
  (show frame))

;;threading
(define-java-class <java.lang.object>)
(define mtx (java-new <java.lang.object>))
(define v 0)
(define (inc-v)
  (java-synchronized mtx (lambda () (set! v (+ v 1)) v)))
(define (dec-v)
  (java-synchronized mtx (lambda () (set! v (- v 1)) v)))
(import threading)
(begin (parallel inc-v dec-v inc-v inc-v dec-v dec-v) v) ;=> 0

;;check exception handling in reflection api
(define (find pred l)
  (and (not (null? l))
       (if (pred (car l))
           (car l)
           (find pred (cdr l)))))
(java/wrap) ;;arg size exception
(java/unwrap 1) ;;type error
(java/class '|foo|) ;;class not found
(java/array-class (java/null) 1) ;;illegal null
(java/array-class <jvoid> 1) ;;illegal void
(java/array-class <jint> -1) ;;illegal dimensions
(java/array-new (java/null) '(1 1 1)) ;;illegal null
(java/array-new <jvoid> '(1 1 1)) ;;illegal void
(java/array-new <jint> '(1 -1 1)) ;;illegal dimensions
(define <jnumber> (java/class '|java.lang.Number|))
(java/invoke-constructor (car (java/constructors <jnumber>))
                         (list)) ;;constructor on abstract class
(define <jaccessible-object>
  (java/class '|java.lang.reflect.AccessibleObject|))
(java/invoke-constructor (car (java/constructors
                               <jaccessible-object>))
                         (list)) ;;access to protected constructor
(define <jurl> (java/class '|java.net.URL|))
(define <jstring> (java/class '|java.lang.String|))
(define make-url-from-string
  (find (lambda (c) (equal? (java/parameter-types c)
                            (list <jstring>)))
        (java/constructors <jurl>)))
(java/invoke-constructor make-url-from-string
                         (list)) ;;illegal arguments
(java/invoke-constructor make-url-from-string
                         (list (->jint 1))) ;;illegal arguments
(java/invoke-constructor make-url-from-string
                         (list (->jstring "foo://bar"))) ;;malformed url
(define <jreader> (java/class '|java.io.Reader|))
(define reader-lock
  (find (lambda (f) (eq? (java/name f) 'lock))
        (java/fields <jreader>)))
(define <jstring-reader> (java/class '|java.io.StringReader|))
(define str
  (java/invoke-constructor
   (car (java/constructors <jstring-reader>))
   (list (->jstring "foo"))))
(java/field-ref reader-lock str) ;;illegal access
(define <jstream-tokenizer> (java/class '|java.io.StreamTokenizer|))
(define st
  (java/invoke-constructor
   (find (lambda (c) (equal? (java/parameter-types c)
                             (list <jreader>)))
         (java/constructors <jstream-tokenizer>))
   (list str)))
(define tokenizer-sval
  (find (lambda (f) (eq? (java/name f) 'sval))
        (java/fields <jstream-tokenizer>)))
(java/field-ref tokenizer-sval (java/null)) ;;illegal null
(java/field-ref tokenizer-sval str) ;;illegal arg
(java/field-set! tokenizer-sval
                 (java/null)
                 (->jstring "foo")) ;;illegal null
(java/field-set! tokenizer-sval
                 str
                 (->jstring "foo")) ;;illegal arg
(java/field-set! tokenizer-sval
                 st
                 (->jint 1)) ;;illegal arg
(java/field-set! reader-lock str (->jstring "foo")) ;;illegal access
(define url-set
  (find (lambda (m) (and (eq? (java/name m) 'set)
                         (equal? (java/parameter-types m)
                                 (list <jstring> <jstring> <jint>
                                       <jstring> <jstring>))))
        (java/methods <jurl>)))
(define url (make-url-from-string
             (->jstring "http://sisc.sourceforge.net")))
(java/invoke-method url-set
                    url
                    (list (->jstring "http")
                          (->jstring "sisc.sourceforge.net")
                          (->jint 80)
                          (->jstring "foo")
                          (->jstring "bar"))) ;;illegal access
(define stream-mark
  (find (lambda (m) (eq? (java/name m) 'mark))
        (java/methods <jreader>)))
(java/invoke-method stream-mark
                    (java/null)
                    (list (->jint 1))) ;;illegal null
(java/invoke-method stream-mark
                    url
                    (list (->jint 1))) ;;illegal args
(java/invoke-method stream-mark
                    str
                    (list)) ;;illegal args
(java/invoke-method stream-mark
                    str
                    (list (->jstring "foo"))) ;;illegal args
(java/invoke-method stream-mark
                    str
                    (list (->jint -1))) ;;IllegalArgumentException
(java/proxy-class <jurl>) ;;illegal interfaces
(java/proxy-class (java/null)) ;;illegal null
(java/array-ref (java/null) 1) ;;illegal null
(java/array-ref (->jint 1) 1) ;;not an array
(define ar (java/array-new <jint> '(1)))
(java/array-ref ar 2) ;;out of bounds
(java/array-set! (java/null) 1 (->jint 1)) ;;illegal null
(java/array-set! (->jint 1) 1 (->jint 1)) ;;not an array
(java/array-set! ar 0 (->jstring "foo")) ;;type incompatible
(java/array-set! ar 2 (->jint 1)) ;;out of bounds

;;micro benchmark for method invocation

(define-generic-java-method trim-to-size)
(define-java-class <java.util.Vector>)
(let ([v (java-new <java.util.Vector> (->jint 0))])
  (time (let loop ([i 10000])
          (or (zero? i)
              (begin
                (trim-to-size v)
                (loop (- i 1)))))))
