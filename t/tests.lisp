;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: SASLPREP-TESTS; -*-
(in-package :saslprep-tests)

(def-suite :saslprep
    :description "Test suite for saslprep")
(in-suite :saslprep)

(defun multibyte-char-as-string-to-array (str)
  "Takes a string in form of '0044 0307' (in hex) and returns an array of char"
  (let* ((split-str (split-sequence:split-sequence #\SPACE str))
         (char-arry (make-array (length split-str))))
    (loop for x in split-str counting x into y do
         (setf (aref char-arry (- y 1)) (saslprep:parse-hex-string-to-char x)))
    char-arry))
