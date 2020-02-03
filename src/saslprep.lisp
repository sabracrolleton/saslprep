;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-

(in-package :saslprep)


;;;;;;;;;;;;;;;;;;;;;;;
;;;; external functions
(defun normalize (str normalization-form)
  "Base external function which calls the appropriate normalization for the normalization form."
  (ecase normalization-form
    (:nfd  (nfd str))
    (:nfkd (nfkd str))
    (:nfc  (nfc str))
    (:nfkc (nfkc str))))

(defun normalize-char (chr normalization-form)
  "Runs normalize on a single character input. You must provide the normalization form (:nfd, :nfkd, :nfc, or :nfkc)"
  (ecase normalization-form
    (:nfd  (nfd (format nil "~a" chr)))
    (:nfkd (nfkd (format nil "~a" chr)))
    (:nfc  (nfc (format nil "~a" chr)))
    (:nfkc (nfkc (format nil "~a" chr)))))

(defun get-mapping (normalization-form &aux (mapping '()))
  "Note no mapping for :nfkc"
  (dolist (map (ecase normalization-form
                 (:nfd  (list *canonical-decomp-map*))
                 (:nfkd (list *canonical-decomp-map* *compatible-decomp-map*))
                 (:nfc  (list *canonical-comp-map*))))
    (maphash
     (lambda (from to)
       (flet ((to-str (x)
                (if (listp x) (coerce x 'string) (string x))))
             (case normalization-form
               (:nfd  (push (list (to-str from) (decompose (to-str to) :canonical)) mapping))
               (:nfkd (push (list (to-str from) (decompose (to-str to) :compatible)) mapping))
               (:nfc  (when (string= (compose (to-str from)) (to-str to))
                        (push (list (decompose (to-str from) :canonical) (to-str to)) mapping))))))
     map))

  ;; hangul
  (loop for code from #xAC00 below (+ #xAC00 11172)
        for char = (string (code-char code))
    do
    (case normalization-form
      ((:nfd :nfkd) (push (list char (decompose char :canonical)) mapping))
      ((:nfc)       (push (list (decompose char :canonical) char) mapping))))

  (nreverse mapping))

(defparameter *derived-normalization-props-data* (make-hash-table :test #'equal))

(defparameter *derived-normalization-props-data-file*
  (uiop:merge-pathnames* *data-directory* "DerivedNormalizationProps.txt"))

(defun get-canonical-combining-class-map ()
  *canonical-combining-class*)

(let ((nfd-illegal-list '())
      (nfkd-illegal-list '())
      (nfc-illegal-list '())
      (nfkc-illegal-list '()))

  (flet ((parse-line (maybe-key line)
           (let* ((fst (string-trim " " (car (cl-ppcre:split ";" line))))
                  (range (mapcar #'parse-hex-string-to-int (cl-ppcre:split "\\.\\." fst)))
                  (maybe? (and maybe-key (not (null (search maybe-key line))))))
             (loop for code from (first range) to (or (second range) (first range))
                   for char = (code-char code)
               collect (list char maybe?)))))
    (with-open-file (in *derived-normalization-props-data-file*)
      (loop for line = (read-line in nil nil)
            while line
        do
        (cond ((or (search "NFKC_QC; N" line) (search "NFKC_QC; M" line))
               (nconcf nfkc-illegal-list (parse-line "NFKC_QC; M" line)))
              ((or (search "NFC_QC; N" line) (search "NFC_QC; M" line))
               (nconcf nfc-illegal-list (parse-line "NFC_QC; M" line)))
              ((search "NFKD_QC; N" line)
               (nconcf nfkd-illegal-list (parse-line nil line)))
              ((search "NFD_QC; N" line)
               (nconcf nfd-illegal-list (parse-line nil line)))))))

  (defun get-illegal-char-list (normalization-form)
    (ecase normalization-form
      (:nfd  nfd-illegal-list)
      (:nfkd nfkd-illegal-list)
      (:nfc  nfc-illegal-list)
      (:nfkc nfkc-illegal-list))))

(defun string-mapped-to-nothing (str)
  (let ((s1 (coerce str 'simple-vector))
        (lst nil)
        (skip nil))
    (loop for x across s1 counting x into y do
         (cond ((char-mapped-to-nothing-p x)
                (setf skip t))
               ((characterp x)
                (push x lst))
               (t (return nil))))
    (setf lst (nreverse lst))
    (format nil "~{~A~}" lst)))

(defun string-mapped-to-space (str)
  (let ((s1 (coerce str 'simple-vector)))
    (loop for x across s1 counting x into y do
         (when (char-mapped-to-space-p x)
           (setf (aref s1 (- y 1)) #\Space)))
    (coerce s1 'string)))

(defun saslprep-normalize (str &optional (form :nfkc))
  "Scans string. If any character should be mapped to nothing, it eliminates that character. If any character is not printable ascii,
it returns nil. If every character remaining after eliminations is printable ascii, it returns the printable-ascii string. "
  (when (string-printable-ascii-p str)
    (return-from saslprep-normalize str))
  (setf str (string-mapped-to-nothing str))
  (setf str (string-mapped-to-space str))
  (normalize str form))
