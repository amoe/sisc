;; records - mostly taken from SRFI-9

; Definition of DEFINE-RECORD-TYPE

(define-syntax define-record-type-helper
  (syntax-rules ()
    [(define-record-type-helper type
       guid
       (constructor constructor-tag ...)
       predicate
       ()
       (field-tag ...)
       ((accessor-field-tag accessor) ...)
       ((modifier-field-tag modifier) ...))
     (define-values (type constructor predicate accessor ... modifier ...)
       (let ([type (make-record-type 'type '(field-tag ...) guid)])
         (values type
                 (record-constructor type '(constructor-tag ...))
                 (record-predicate type)
                 (record-accessor type 'accessor-field-tag)
                 ...
                 (record-modifier type 'modifier-field-tag)
                 ...)))]
    [(define-record-type-helper type
       guid
       constructor
       predicate
       ((field-tag accessor modifier) . fields)
       field-tags
       accessors
       modifiers)
     (define-record-type-helper type
       guid
       constructor
       predicate
       fields
       (field-tag . field-tags)
       ((field-tag accessor) . accessors)
       ((field-tag modifier) . modifiers))]
    [(define-record-type-helper type
       guid
       constructor
       predicate
       ((field-tag accessor) . fields)
       field-tags
       accessors
       modifiers)
     (define-record-type-helper type
       guid
       constructor
       predicate
       fields
       (field-tag . field-tags)
       ((field-tag accessor) . accessors)
       modifiers)]))

(define-simple-syntax (define-record-type type
                        constructor
                        predicate
                        field
                        ...)
  (define-record-type-helper type
    #f
    constructor
    predicate
    (field ...)
    ()
    ()
    ()))

(define-simple-syntax (define-nongenerative-record-type type guid
                        constructor
                        predicate
                        field
                        ...)
  (define-record-type-helper type
    'guid
    constructor
    predicate
    (field ...)
    ()
    ()
    ()))

; We define the following procedures:
; 
; (make-record-type <type-name <field-names> [<guid>])  -> <record-type>
; (record-constructor <record-type<field-names>) -> <constructor>
; (record-predicate <record-type>)               -> <predicate>
; (record-accessor <record-type <field-name>)    -> <accessor>
; (record-modifier <record-type <field-name>)    -> <modifier>
;   where
; (<constructor> <initial-value> ...)         -> <record>
; (<predicate> <value>)                       -> <boolean>
; (<accessor> <record>)                       -> <value>
; (<modifier> <record> <value>)         -> <unspecific>

;----------------
; Record types are themselves records, so we first define the type for
; them.  Except for problems with circularities, this could be defined as:
;  (define-record-type :record-type
;    (make-record-type name field-tags)
;    record-type?
;    (name record-type-name)
;    (field-tags record-type-field-tags))
; As it is, we need to define everything by hand.

; Now that :record-type exists we can define a procedure for making more
; record types.

(define :record-type (void))

(define (make-record-type name field-tags . guid)
  (define (maybe-intern value)
    (if (and (not (null? guid)) (car guid))
        (type-safe-intern record-type? (car guid) value)
        value))
  (let ([res (maybe-intern (make-record :record-type 2))])
    (record-set! res 0 name)
    (record-set! res 1 field-tags)
    res))

(define (record-type? thing)
  (and (record? thing) (eq? (record-type thing) :record-type)))

; Accessors for record types.

(define (record-type-name record-type)
  (record-ref record-type 0))

(define (record-type-field-tags record-type)
  (record-ref record-type 1))

;----------------
; A utility for getting the offset of a field within a record.

(define (field-index type tag)
  (let loop ((i 0) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error "record type ~a has no field ~a" (record-type-name type) tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))

;----------------
; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
; procedures used by the macro expansion of DEFINE-RECORD-TYPE.

(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (map (lambda (tag) (field-index type tag)) tags)))
    (lambda args
      (if (= (length args) arg-count)
          (let ((new (make-record type size)))
            (for-each (lambda (arg i) (record-set! new i arg))
                      args
                      indexes)
            new)
          (error "wrong number of arguments to constructor for record type ~a; expected ~a args, got ~a" (record-type-name type) arg-count args)))))

(define (record-predicate type)
  (lambda (thing)
    (and (record? thing) (eq? (record-type thing) type))))

(define (record-accessor type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (eq? (record-type thing) type)
          (record-ref thing index)
          (error "accessor for ~a in record type ~a applied to ~a" tag (record-type-name type) thing)))))

(define (record-modifier type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (eq? (record-type thing) type)
          (record-set! thing index value)
          (error "modifier for ~a in record type ~a applied to ~a" tag (record-type-name type) thing)))))

;; define-struct ala MzScheme
(define-syntax define-struct-helper
  (lambda (x)
    (syntax-case x ()
      [(_ "helper" name #f constructor-spec predicate field-spec ...)
       (syntax
        (define-record-type name
          constructor-spec
          predicate
          field-spec
          ...))]
      [(_ "helper" name guid constructor-spec predicate field-spec ...)
       (syntax
        (define-nongenerative-record-type name guid
          constructor-spec
          predicate
          field-spec
          ...))]
      [(_ name guid (field ...))
       (let* ((name-syntax (syntax name))
              (name-string (symbol->string (syntax-object->datum
                                            name-syntax))))
         (with-syntax ((make-name (wrap-symbol "make-" name-syntax ""))
                       (name? (wrap-symbol "" name-syntax "?"))
                       ((field-getter ...)
                        (map (lambda (f)
                               (wrap-symbol
                                (string-append name-string "-")
                                (datum->syntax-object name-syntax f)
                                ""))
                             (syntax-object->datum (syntax (field ...)))))
                       ((field-setter ...)
                        (map (lambda (f)
                               (wrap-symbol
                                (string-append "set-" name-string "-")
                                (datum->syntax-object name-syntax f)
                                "!"))
                             (syntax-object->datum (syntax (field ...))))))
           (syntax
            (define-struct-helper "helper"
              name guid (make-name field ...)
              name?
              (field field-getter field-setter) ...))))])))

(define-simple-syntax (define-struct name fields)
  (define-struct-helper name #f fields))

(define-simple-syntax (define-nongenerative-struct name guid fields)
  (define-struct-helper name guid fields))

(define <record> (make-type '|sisc.modules.record.Record|))

(define (record-type-of-hook next o)
  (if (record? o)
      (record-type o)
      (next o)))

(define (record-type<=-hook next x y)
  (cond [(record-type? x)
         (if (record-type? y)
             (eq? x y)
             (type<= <record> y))]
        [(record-type? y) #f]
        [else (next x y)]))

(set! :record-type (type-safe-intern record? 'sisc.record.record-type
                                     (make-record :record-type 2)))
(record-type! :record-type :record-type)
(record-set! :record-type 0 ':record-type)
(record-set! :record-type 1 '(name field-tags))

;; 
;; The contents of this file are subject to the Mozilla Public
;; License Version 1.1 (the "License"); you may not use this file
;; except in compliance with the License. You may obtain a copy of
;; the License at http://www.mozilla.org/MPL/
;; 
;; Software distributed under the License is distributed on an "AS
;; IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
;; implied. See the License for the specific language governing
;; rights and limitations under the License.
;; 
;; The Original Code is the Second Interpreter of Scheme Code (SISC).
;; 
;; The Initial Developer of the Original Code is Scott G. Miller.
;; Portions created by Scott G. Miller are Copyright (C) 2000-2007
;; Scott G. Miller.  All Rights Reserved.
;; 
;; Contributor(s):
;; Matthias Radestock 
;; 
;; Alternatively, the contents of this file may be used under the
;; terms of the GNU General Public License Version 2 or later (the
;; "GPL"), in which case the provisions of the GPL are applicable 
;; instead of those above.  If you wish to allow use of your 
;; version of this file only under the terms of the GPL and not to
;; allow others to use your version of this file under the MPL,
;; indicate your decision by deleting the provisions above and
;; replace them with the notice and other provisions required by
;; the GPL.  If you do not delete the provisions above, a recipient
;; may use your version of this file under either the MPL or the
;; GPL.
;;