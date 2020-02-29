;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: SASLPREP-TESTS; -*-
(in-package :saslprep-tests)

(fiveam:def-suite :saslprep
    :description "Test suite for saslprep")

(fiveam:in-suite :saslprep)

(defparameter *test-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "t") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'saslprep nil))))

(test chars-mapped-to-nothing
  (let ((arry (make-array 3 :initial-contents "ABC")))
    (loop for x in (get-chars-mapped-to-nothing) do
         (let ((str ""))
           (setf (aref arry 1) x)
           (setf str (coerce arry 'string))
           (is (equal "AC" (string-mapped-to-nothing str)))))))

(test chars-mapped-to-space
  (let ((arry (make-array 3 :initial-contents "ABC")))
    (loop for x in (get-chars-mapped-to-space) do
         (let ((str ""))
           (setf (aref arry 1) x)
           (setf str (coerce arry 'string))
           (is (equal "A C" (string-mapped-to-space str)))))))

(test ascii-control-chars
  (loop for x in (get-ascii-control-chars) do
       (is (ascii-control-char-p x))))

(test non-ascii-control-chars
  (loop for x in (get-non-ascii-control-chars) do
       (is (non-ascii-control-char-p x))))

(test non-ascii-space-chars
  (loop for x in (get-non-ascii-space-chars) do
       (is (non-ascii-space-char-p x))))

(test private-use-char
  (loop for x in (get-private-use-chars) do
       (is (private-use-char-p x))))

(test non-char-code-point
  (loop for x in (get-non-char-code-points) do
       (is (non-char-code-point-p x))))

(test surrogate-code-point
  (loop for x in (get-surrogate-code-points) do
       (is (surrogate-code-point-p x))))

(test inappropriate-for-plain-text
  (loop for x in (get-inappropriate-for-plain-text-chars) do
       (is (inappropriate-for-plain-text-char-p x))))

(test inappropriate-for-canonical-representation-char
  (loop for x in (get-inappropriate-for-canonical-representation-chars) do
       (is (inappropriate-for-canonical-representation-char-p x))))

(test change-display-property-char
  (loop for x in (get-change-display-property-chars) do
       (is (change-display-property-char-p x))))

(test tagging-chars
  (loop for x in (get-tagging-chars) do
       (is (tagging-char-p x))))

(test char-with-bidirectional-property-R-or-AL
  (loop for x in (get-bidirectional-property-R-or-AL-chars) do
       (is (char-with-bidirectional-property-r-or-al-p x))))

(test char-with-bidirectional-property-L
  (loop for x in (get-bidirectional-property-L-chars) do
       (is (char-with-bidirectional-property-l-p x))))

(defun set-arry ()
  (let ((arry (make-array 5)))
    (setf (aref arry 0) #\A)
    (setf (aref arry 1) #\B)
    (setf (aref arry 2) #\C)
    (setf (aref arry 3) #\D)
    (setf (aref arry 4) #\E)
    arry))

(test saslprep-normalize
  (let ((arry (set-arry)))
    (loop for x in (get-chars-mapped-to-nothing) do
         (setf (aref arry 3) x)
         (is (equal (saslprep-normalize (coerce arry 'string)) "ABCE")))
    (setf arry (set-arry))
    (loop for x in (get-chars-mapped-to-space) do
         (setf (aref arry 3) x)
         (if (equal (char-code x) 8203) ; NEED TO DEAL WITH ZERO_WIDTH_SPACE
             (is (equal (saslprep-normalize (coerce arry 'string)) "ABCE"))
             (is (equal (saslprep-normalize (coerce arry 'string)) "ABC E"))))
    (setf arry (set-arry))
    (loop for x in (get-ascii-control-chars) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-non-ascii-control
  "Note that normalization removes the zero width and word joiner control characters, so they no longer exist to signal an error"
  (let ((arry (set-arry)))
    (loop for x in (set-difference (get-non-ascii-control-chars) (get-chars-mapped-to-nothing)) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-private-use
  (let ((arry (set-arry)))
    (loop for x in (get-private-use-chars) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-non-char-code-point
  (let ((arry (set-arry)))
    (loop for x in (get-non-char-code-points) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-surrogate-code-points
  (let ((arry (set-arry)))
    (loop for x in (get-surrogate-code-points) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-inappropriate-for-plain-text-chars
  (let ((arry (set-arry)))
    (loop for x in (get-inappropriate-for-plain-text-chars) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-inappropriate-for-canonical-representation-chars
  (let ((arry (set-arry)))
    (loop for x in (get-inappropriate-for-canonical-representation-chars) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-change-display-property-chars
  "note that normalization removes the #\COMBINING_ACUTE_TONE_MARK and #\COMBINING_GRAVE_TONE_MARK"
  (let ((arry (set-arry)))
    (loop for x in (get-change-display-property-chars) do
         (setf (aref arry 3) x)
         (unless (member x '(#\COMBINING_ACUTE_TONE_MARK #\COMBINING_GRAVE_TONE_MARK))
           (signals error (saslprep-normalize (coerce arry 'string)))))))

(test saslprep-normalize-tagging-chars
  (let ((arry (set-arry)))
    (loop for x in (get-tagging-chars) do
         (setf (aref arry 3) x)
         (signals error (saslprep-normalize (coerce arry 'string))))))

(test saslprep-normalize-bidirectional-errors
  (let ((arry (set-arry)))
         (setf (aref arry 1) (elt (get-bidirectional-property-R-or-AL-chars) (random (length (get-bidirectional-property-R-or-AL-chars)))))
         (setf (aref arry 3) (elt (get-bidirectional-property-L-chars) (random (length (get-bidirectional-property-L-chars)))))
         (signals error (saslprep-normalize (coerce arry 'string)))))
