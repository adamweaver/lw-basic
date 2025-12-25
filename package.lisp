(defpackage :basic
  (:use :cl :utils)
  (:export "BASIC-PARSE-ERROR" "PARSE"
           "KEYMAP" "KEYMAP-P" "MAKE-KEYMAP" "KEYMAP-COUNT" "KEYMAP-MAP" "DEEP-COPY-KEYMAP" "KEYMAP-DELETE"))

(defpackage :forth
  (:use :cl :utils)
  (:export "WITH-DICT" "WITH-VARIABLES" "COMPILE-FORTH" "RUN" "DEFINE-FORTH-FUNCTION"))
