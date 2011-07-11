; SRFI-14 Support Functions
;
; This file contains all non-portable definitions.  Implementors
; should adjust these defintions to match your capabilities.

(define HASH-BOUNDS #x7fffffff)

; If your Scheme system doesn't support the full range of unicode characters,
; set MAX-CODEPOINT here to the largest supported codepoint.  

; Example, for 16-bit character support
(define MAX-CODEPOINT #xffff)

; The end of the Unicode 4.0 Range
;(define MAX-CODEPOINT #x10FFFD)

; %char->code-point is responsible for converting
; a character into a numeric Unicode code-point.
; The default assumes that char->integer from R5RS
; does this.
(define %char->code-point char->integer)

; %code-point->char is responsible for converting
; a Unicode codepoint into a Scheme character.
; It should return a character, unless
; the code-point is outside the range of representable
; character range, in which case it may either return #f,
; if you desire silent failure, or raise an error.
; It assumes currently that integer->char does this,
; adjust if necessary.
;
(define (%code-point->char c)
  (if (<= c MAX-CODEPOINT)
      (integer->char c)
      
      ; (error "code-point outside of representable character range: " c)
      #f))

(define %latin1->char integer->char)
(define %char->latin1 char->integer)