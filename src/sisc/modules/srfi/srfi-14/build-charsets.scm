(with-output-to-file "charsets.scm"
  (lambda ()
    (for-each (lambda (name charset)
                (display "(define ") (display name) (newline)
                (display "  (make-char-set #x") (display (number->string (char-set:s charset) 16)) (display "))") (newline)
                (newline))
              '(char-set:lower-case  char-set:upper-case  char-set:title-case
                char-set:letter      char-set:digit       char-set:letter+digit
                char-set:graphic     char-set:printing    char-set:whitespace
                char-set:iso-control char-set:punctuation char-set:symbol
                char-set:hex-digit   char-set:blank       char-set:ascii
                char-set:empty       char-set:full)

              (list
                char-set:lower-case  char-set:upper-case  char-set:title-case
                char-set:letter      char-set:digit       char-set:letter+digit
                char-set:graphic     char-set:printing    char-set:whitespace
                char-set:iso-control char-set:punctuation char-set:symbol
                char-set:hex-digit   char-set:blank       char-set:ascii
                char-set:empty       char-set:full))))
              

