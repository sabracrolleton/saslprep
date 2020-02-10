;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: SASLPREP-TESTS; -*-
(in-package :saslprep-tests)

(fiveam:def-suite :saslprep
    :description "Test suite for saslprep")
(fiveam:in-suite :saslprep)

(defun parse-hex-string-to-string (str)
  "Takes a string which may be one or more hex numbers e.g. '0044 0307', builds an array of characters, coerces to string and returns the string. Mostly used for testing."
  (let* ((split-str (split-sequence:split-sequence #\Space str :remove-empty-subseqs t))
         (arry (make-array (length split-str))))
    (loop for x in split-str counting x into y do
         (setf (aref arry (- y 1)) (parse-hex-string-to-char x)))
    (coerce arry 'string)))

(defun parse-hex-string-to-int (str)
  "Parse a string which is a single character in hex to a decimal."
  (parse-integer str :radix 16))

(defun parse-hex-string-to-char (str)
  "Parse a hex string which is a single character into a character using code-char."
  (code-char (parse-hex-string-to-int str)))

(defun int-to-hex-string (int)
  (write-to-string int :base 16))

(defun parse-hex-list-to-string (lst)
  "Takes a list of numbers and returns a string of characters"
  (let ((arry (make-array (length lst))))
    (loop for x in lst counting x into y do
         (setf (aref arry (- y 1)) (code-char x)))
    (coerce arry 'string)))

(defun str-to-arry (str)
  (let ((arry (make-array (length str))))
    (loop for x across arry counting x into y do
         (setf (aref arry (- y 1)) (schar str (- y 1))))))


(defun hs-to-cs (str)
  "Syntactic sugar"
  (parse-hex-string-to-string str))

(defparameter *test-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "t") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'saslprep nil))))

(defun read-test-data (fname)
  (with-open-file (in (uiop:merge-pathnames* *test-directory* fname))
    (loop for line = (read-line in nil nil)
       while line
       collect (cl-ppcre:split ";" line))))
