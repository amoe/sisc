(define-nongenerative-record-type <slot> sisc.oo.slot-type
  (make-slot name class accessor modifier accessor-method modifier-method)
  slot?
  (name                 slot-name)
  (class                slot-class)
  (accessor             slot-accessor)
  (modifier             slot-modifier)
  (accessor-method      slot-accessor-method)
  (modifier-method      slot-modifier-method))

(define (make-slot-helper name class idx)
  (define (object-type-error obj)
    (error "wrong object type, expected ~s, got ~s"
           class (type-of obj)))
  (make-slot name
             class
             (lambda (obj) (if (instance-of? obj class)
                               (record-ref obj idx)
                               (object-type-error obj)))
             (lambda (obj val) (if (instance-of? obj class)
                                   (record-set! obj idx val)
                                   (object-type-error obj)))
             (make-method
              (lambda (next obj) (record-ref obj idx))
              (list class)
              #f)
             (make-method
              (lambda (next obj val) (record-set! obj idx val))
              (list class <value>)
              #f)))

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