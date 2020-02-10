;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

(defpackage :saslprep-system
  (:use :common-lisp :asdf))
(in-package :saslprep-system)

(defparameter *string-file* "strings-utf-8")

(defsystem "saslprep"
  :description "Common lisp implementation of stringprep functions as referenced by RFC 3454 (Stringprep) and 4013 (SASLprep)"
  :author "Sabra Crolleton <sabra.crolleton@gmail.com>"
  :license "MIT"
  :depends-on ("split-sequence" "cl-ppcre" "uiop" "uax-15")
  :components
  ((:module "src"
            :components ((:file "package")
                         (:file "utilities" :depends-on ("package"))
                         (:file "precomputed-tables" :depends-on ("package" "utilities"))
                         (:file "saslprep-backend" :depends-on ("package" "utilities" "precomputed-tables"))
                         (:file "saslprep" :depends-on ("package" "utilities" "saslprep-backend")))))
  :in-order-to ((test-op (test-op "t/tests"))))

(defsystem "saslprep/tests"
  :depends-on ("saslprep" "fiveam" "uiop" "cl-ppcre" "split-sequence")
  :components
  ((:module "t"
            :components ((:file "test-package")
                         (:file "tests"))))
  :perform (test-op (o c)
             (uiop:symbol-call :fiveam '#:run! :saslprep)))
