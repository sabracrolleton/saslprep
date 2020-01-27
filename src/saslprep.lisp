;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-

(in-package :saslprep)

(defmacro nconcf (list1 list2) `(setf ,list1 (nconc ,list1 ,list2)))

(defun parse-hex-string-to-decimal (str)
  "Parse a string which is in hex to a decimal."
  (parse-integer str :radix 16))

(defun parse-hex-string-to-char (str)
  "Parse a hex string into a character using code-char."
  (code-char (parse-hex-string-to-decimal str)))


(defparameter *data-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "data") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'saslprep nil))))

(defparameter *composition-exclusions-data-location*
  (uiop:merge-pathnames* *data-directory* "CompositionExclusions.txt"))

(defparameter *derived-normalization-props-data-location*
  (uiop:merge-pathnames* *data-directory* "DerivedNormalizationProps.txt"))

(defparameter *unicode-data-location*
  (uiop:merge-pathnames* *data-directory* "UnicodeData.txt"))

(defparameter *normalization-test-data-location*
  (uiop:merge-pathnames* *data-directory* "NormalizationTest.txt"))

(defvar *unicode-data*
  (with-open-file (in *unicode-data-location*)
    (loop for line = (read-line in nil nil)
       while line
       collect (cl-ppcre:split ";" line))))

(let ((canonical-decomp-map (make-hash-table))
      (compatible-decomp-map (make-hash-table))
      (canonical-combining-class (make-hash-table)))
  (loop for (1st _ __ 4th ___ 6th) in *unicode-data*
        for char = (parse-hex-string-to-char 1st)
        for ccc  = (parse-integer 4th)
        for decomp-chars =
        (let ((tmp (cl-ppcre:split " " 6th)))
          (when tmp
            (if (char= #\< (char (first tmp) 0))
                (cons :compatible (mapcar #'parse-hex-string-to-char (cdr tmp))) ; swap decomposition
              (cons :canonical (mapcar #'parse-hex-string-to-char tmp)))))       ; formal decomposition
    do
    (when (plusp ccc)
      (setf (gethash char canonical-combining-class) ccc))

    (when decomp-chars
      (if (eq (car decomp-chars) :canonical)
          (setf (gethash char canonical-decomp-map) (cdr decomp-chars))   ; formal decomposition
        (setf (gethash char compatible-decomp-map) (cdr decomp-chars))))) ; swap decomposition

  (defvar *canonical-decomp-map* canonical-decomp-map)
  (defvar *compatible-decomp-map* compatible-decomp-map)
  (defvar *canonical-combining-class* canonical-combining-class))


(defparameter *composition-exclusions-data* (make-hash-table))
(with-open-file (in *composition-exclusions-data-location*)
  (loop for line = (read-line in nil nil)
     while line
       when (and (plusp (length line))
                  (char/= (char line 0) #\#))
         do (setf (gethash (parse-hex-string-to-char (subseq line 0 (position #\Space line)))
                           *composition-exclusions-data*)
                  t)))

(let ((canonical-comp-map (make-hash-table :test #'equal)))
  (maphash
   (lambda (src-char decomped-chars)
     (when (and (= 2 (length decomped-chars))
                (not (gethash src-char *composition-exclusions-data*)))
       (setf (gethash (coerce decomped-chars 'list)
                      canonical-comp-map)
             src-char)))
   *canonical-decomp-map*)
  (defparameter *canonical-comp-map* canonical-comp-map))

(defun get-canonical-combining-class (ch)
  (gethash ch *canonical-combining-class* 0))

(defun decompose-char (char &optional (type :canonical))
  (let ((decomped-chars (or (gethash char *canonical-decomp-map*)
                            (and (eq type :compatible)
                                 (gethash char *compatible-decomp-map*)))))
    (if decomped-chars
        (mapcan (lambda (c) (decompose-char c type)) decomped-chars)
      (list char))))

(let* ((s-base #xAC00)
       (l-base #x1100)
       (v-base #x1161)
       (t-base #x11A7)
       (l-count 19)
       (v-count 21)
       (t-count 28)
       (n-count (* v-count t-count))
       (s-count (* l-count n-count)))
  ;; split
  (defun decompose-hangul-char (ch &aux (cd (char-code ch)))
    (let ((s-index (- cd s-base)))
      (unless (<= 0 s-index (1- s-count))
        (return-from decompose-hangul-char (list ch)))

      (let ((lc (+ l-base (floor s-index n-count)))
            (vc (+ v-base (floor (mod s-index n-count) t-count)))
            (tc (+ t-base (mod s-index t-count))))
        (if (/= tc t-base)
            (list (code-char lc) (code-char vc) (code-char tc))
          (list (code-char lc) (code-char vc))))))

  ;; synthesis
  (defun compose-hangul (str &aux (len (length str)))
    (if (zerop len)
        str
      (let* ((last (char str 0))
             (new-chars (list last)))
        (loop for i from 1 below len
              for ch = (char str i)
              for l-index = (- (char-code last) l-base)
              for s-index = (- (char-code last) s-base)
          DO
          (tagbody
           ;; 1. check to see if two current characters are L and V
           (when (<= 0 l-index (1- l-count))
             (let ((v-index (- (char-code ch) v-base)))
               (when (<= 0 v-index (1- v-count))
                 ;; make syllable of form LV
                 (setf last
                       (code-char (+ s-base (* (+ (* l-index v-count) v-index) t-count))))
		 (setf (car new-chars) last) ; reset last
                 (go :end))))                ; discard ch

           ;; 2. check to see if two current characters are LV and T
           (when (and (<= 0 s-index (1- s-count))
                      (zerop (mod s-index t-count)))
             (let ((t-index (- (char-code ch) t-base)))
               (when (< 0 t-index t-count)
                 ;; make syllable of form LVT
                 (setf last (code-char (+ (char-code last) t-index)))
                 (setf (car new-chars) last) ; reset last
                 (go :end))))                ; discard ch

           ;; if neigher case was true, just add the character
           (push (setf last ch) new-chars)
           :end))
        (coerce (nreverse new-chars) 'string)))))

(defun decompose (s type)
  (loop for c across s
    append
      (mapcan #'decompose-hangul-char (decompose-char c type))
      into new-s
    finally
      (return (coerce new-s 'string))))

(defun canonical-ordering (decomposed-string &aux (s decomposed-string))
  (let ((starter-indices
         (loop for i from 1 below (length s)
               for ccc = (get-canonical-combining-class (aref s i))
               when (zerop ccc)
           collect i)))
    (loop for (beg end) on (cons 0 starter-indices) do
      (setf #1=(subseq s beg end)
            (stable-sort #1# #'< :key #'get-canonical-combining-class))))
  s)

(defun compose (decomposed-string)
  (let* ((s decomposed-string)
         (to-cs (coerce s 'simple-vector)))
    (loop for i from 1 below (length s)
          for ch-right  = (char s i)      ; right character
          for ccc-right = (get-canonical-combining-class ch-right)
      do
      (loop for j from (1- i) downto 0
            for ch-left  = (aref to-cs j) ; left character
            for ccc-left = (and ch-left (get-canonical-combining-class ch-left))
            WHEN ch-left
        do
        (when (zerop ccc-left)
          ;; ch-left + ch-right  if there is a composite character ch-left replace
          (let ((comped-char (gethash (list ch-left ch-right) *canonical-comp-map*)))
            (when comped-char
              (setf (aref to-cs j) comped-char
                    (aref to-cs i) nil)))
          (return))

        (unless (< ccc-left ccc-right)
          (return))))
    (compose-hangul (coerce (remove nil to-cs) 'string))))

;;;;;;;;;;;;;;;;;;;;;;
;;;; NFD/NFKD/NFC/NFKC
(defun nfd (s)
  (canonical-ordering (decompose s :canonical)))

(defun nfkd (s)
  (canonical-ordering (decompose s :compatible)))

(defun nfc (s)
  (compose (nfd s)))

(defun nfkc (s)
  (compose (nfkd s)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; external functions
(defun normalize (str normalization-form)
  (ecase normalization-form
    (:nfd  (nfd str))
    (:nfkd (nfkd str))
    (:nfc  (nfc str))
    (:nfkc (nfkc str))))

(defun get-mapping (normalization-form &aux (mapping '()))
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

(defun get-canonical-combining-class-map ()
  *canonical-combining-class*)

(let ((nfd-illegal-list '())
      (nfkd-illegal-list '())
      (nfc-illegal-list '())
      (nfkc-illegal-list '()))

  (flet ((parse-line (maybe-key line)
           (let* ((fst (string-trim " " (car (cl-ppcre:split ";" line))))
                  (range (mapcar #'parse-hex-string-to-decimal (cl-ppcre:split "\\.\\." fst)))
                  (maybe? (and maybe-key (not (null (search maybe-key line))))))
             (loop for code from (first range) to (or (second range) (first range))
                   for char = (code-char code)
               collect (list char maybe?)))))
    (with-open-file (in *derived-normalization-props-data-location*)
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
