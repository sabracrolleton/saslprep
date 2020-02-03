;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :saslprep
  (:use :common-lisp)
  (:export #:normalize
           #:saslprep
           #:get-mapping
           #:get-illegal-char-list
           #:get-canonical-combining-class-map
           #:parse-hex-string-to-char
           #:parse-hex-string-to-string
           #:parse-hex-list-to-string))
