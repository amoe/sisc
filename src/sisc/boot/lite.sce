; If loaded, instructs init.pp not to load SNative
(#%program
  ((putprop . 1) (get-symbolic-environment . 1))
  ()
  (putprop get-symbolic-environment)
  (putprop 'LITE (get-symbolic-environment '*sisc*) '#t))
