;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defpackage :saslprep-system
  (:use :common-lisp :asdf))
(in-package :saslprep-system)

;; Change this to enable/disable unicode manually (mind that it won't
;; work unless your implementation supports it).
(defparameter *unicode*
  #+(or sb-unicode unicode ics openmcl-unicode-strings) t
  #-(or sb-unicode unicode ics openmcl-unicode-strings) nil)
(defparameter *string-file* (if *unicode* "strings-utf-8" "strings-ascii"))

(defsystem "saslprep"
  :description "Common lisp implementation of RFC 3454 (Stringprep) and 4013 (SASLprep)"
  :author "Takeru Ohta, Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "MIT"
  :depends-on ("split-sequence" "cl-ppcre" "uiop")
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "utilities" :depends-on ("package"))
                         (:file "saslprep" :depends-on ("package" "utilities")))))
  :in-order-to ((test-op (test-op "t/tests"))))

(defsystem "saslprep/tests"
  :depends-on ("saslprep" "fiveam")
  :components
  ((:module "t"
            :components ((:file "test-package")
                         (:file "tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :saslprep-tests '#:prompt-connection)
             (uiop:symbol-call :fiveam '#:run! :saslprep)))
