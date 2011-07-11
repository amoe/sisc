(display benchmark-results)
(display (list 'total (apply + (map cadr benchmark-results)) 'ms))
(newline)

