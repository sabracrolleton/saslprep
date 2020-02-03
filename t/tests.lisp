;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: SASLPREP-TESTS; -*-
(in-package :saslprep-tests)

(fiveam:def-suite :saslprep
    :description "Test suite for saslprep")
(fiveam:in-suite :saslprep)

(defun hs-to-cs (str)
  "Syntactic sugar"
  (saslprep::parse-hex-string-to-string str))

(defparameter *test-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "t") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'saslprep nil))))

(defun read-test-data (fname)
  (with-open-file (in (uiop:merge-pathnames* *test-directory* fname))
    (loop for line = (read-line in nil nil)
       while line
       collect (cl-ppcre:split ";" line))))

(test part0-nfkc
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part1-nfkc
  (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part2-nfkc
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part3-nfkc
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (first x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (second x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (third x)) :nfkc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfkc)))))

(test part0-nfkd
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part1-nfkd
  (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part2-nfkd
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part3-nfkd
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (first x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (second x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (third x)) :nfkd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfkd)))))

(test part0-nfc
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part1-nfc
  (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part2-nfc
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part3-nfc
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (first x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (second x)) :nfc)))
       (is (equal (hs-to-cs (second x)) (normalize (hs-to-cs (third x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fourth x)) :nfc)))
       (is (equal (hs-to-cs (fourth x)) (normalize (hs-to-cs (fifth x)) :nfc)))))

(test part0-nfd
  (loop for x in (read-test-data "test-part0.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(test part1-nfd
      (loop for x in (read-test-data "test-part1.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(test part2-nfd
  (loop for x in (read-test-data "test-part2.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))

(test part3-nfd
  (loop for x in (read-test-data "test-part3.txt") do
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (first x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (second x)) :nfd)))
       (is (equal (hs-to-cs (third x)) (normalize (hs-to-cs (third x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fourth x)) :nfd)))
       (is (equal (hs-to-cs (fifth x)) (normalize (hs-to-cs (fifth x)) :nfd)))))
