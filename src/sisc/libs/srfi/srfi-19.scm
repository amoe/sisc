(require-library 'sisc/libs/srfi/srfi-6)
(require-library 'sisc/libs/srfi/srfi-8)
(require-library 'sisc/libs/srfi/optional-args)

(module srfi-19
    (time-tai
     time-utc time-monotonic time-thread time-process
     time-duration time-gc
     make-time time?
     time-type time-second time-nanosecond
     set-time-type! set-time-second! set-time-nanosecond!
     copy-time current-time time-resolution
     time=? time>? time<? time>=? time<=?
     time-difference time-difference!
     add-duration add-duration!
     subtract-duration subtract-duration!
     time-utc->time-tai time-utc->time-tai!
     time-utc->time-monotonic time-utc->time-monotonic!
     time-tai->time-utc time-tai->time-utc!
     time-tai->time-monotonic time-tai->time-monotonic!
     time-monotonic->time-utc time-monotonic->time-utc!
     time-monotonic->time-tai time-monotonic->time-tai!
     make-date date?
     date-nanosecond date-second date-minute date-hour
     date-day date-month date-year date-zone-offset
     ;;set-date-nanosecond! set-date-second! set-date-minute! set-date-hour!
     ;;set-date-day! set-date-month! set-date-year! set-date-zone-offset!
     time-utc->date time-tai->date time-monotonic->date
     date->time-utc date->time-tai date->time-monotonic
     date-year-day date-week-day date-week-number
     current-date current-julian-day current-modified-julian-day
     date->julian-day date->modified-julian-day
     time-utc->julian-day time-utc->modified-julian-day
     time-tai->julian-day time-tai->modified-julian-day
     time-monotonic->julian-day time-monotonic->modified-julian-day
     julian-day->time-utc julian-day->time-tai julian-day->time-monotonic
     julian-day->date modified-julian-day->date
     modified-julian-day->time-utc
     modified-julian-day->time-tai
     modified-julian-day->time-monotonic
     date->string string->date)
  (import srfi-6)
  (import srfi-8)
  (import record)
  (import* optional-args :optional)
  (include "../../modules/srfi/srfi-19/srfi-19.scm")
  (add-feature 'srfi-19))
