(require-library 'sisc/libs/srfi/srfi-1)
(require-library 'sisc/libs/srfi/srfi-19)

(module srfi-18 
    (current-thread thread? make-thread
     thread-name thread-specific thread-specific-set!
     thread-start! thread-yield! thread-sleep! thread-terminate!
     thread-join!
     mutex? make-mutex mutex-name mutex-specific mutex-specific-set!
     mutex-state mutex-lock! mutex-unlock!
     condition-variable? make-condition-variable condition-variable-name
     condition-variable-specific condition-variable-specific-set!
     condition-variable-signal! condition-variable-broadcast!
     current-time time? time->seconds seconds->time
     current-exception-handler with-exception-handler raise
     join-timeout-exception? abandoned-mutex-exception?
     terminated-thread-exception? uncaught-exception?
     uncaught-exception-reason)
  (import threading-native)
  (import record)
  (import* srfi-19
           current-time
           time-utc
           time?
           make-time
           time-second
           time-nanosecond)
  (import* srfi-1 delete!)
  (include "../../modules/srfi/srfi-18.scm")
  (add-feature 'srfi-18))
