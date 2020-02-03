;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-

(in-package :saslprep)

(defun char-mapped-to-nothing-p (ch &optional (hsh *mapped-to-nothing*))
  "Returns t if the character should be mapped to nothing"
  (gethash ch hsh))

(defun char-mapped-to-space-p (chr)
  "If character is mapped to space, then return t, else nil"
  (if (member chr (get-chars-mapped-to-space) )
      t
      nil))

(defun non-character-code-point-with-hash-p (chr)
  (cond ((characterp chr) (setf chr (char-code chr)))
        ((stringp chr) (setf chr (parse-integer chr :radix 16)))
        (t nil))
  (if (>= chr #xFDD0)
      nil
      (gethash chr *non-character-code-points*)))

(defun non-character-code-point-p (chr)
  "Returns t if the parameter is a non-character code points as defined in StringPrep C.4."
  (cond ((characterp chr) (setf chr (char-code chr)))
        ((stringp chr) (setf chr (parse-integer chr :radix 16)))
        (t nil))
  (if (and (>= chr #xFDD0)
           (or
            (and (>= chr #xFDD0) (<= chr #xFDEF))
            (and (>= chr #xFFFE) (<= chr #xFFFF))
            (and (>= chr #x1FFFE) (<= chr #x1FFFF))
            (and (>= chr #x2FFFE) (<= chr #x2FFFF))
            (and (>= chr #x3FFFE) (<= chr #x3FFFF))
            (and (>= chr #x4FFFE) (<= chr #x4FFFF))
            (and (>= chr #x5FFFE) (<= chr #x5FFFF))
            (and (>= chr #x6FFFE) (<= chr #x6FFFF))
            (and (>= chr #x7FFFE) (<= chr #x7FFFF))
            (and (>= chr #x8FFFE) (<= chr #x8FFFF))
            (and (>= chr #x9FFFE) (<= chr #x9FFFF))
            (and (>= chr #xAFFFE) (<= chr #xAFFFF))
            (and (>= chr #xBFFFE) (<= chr #xBFFFF))
            (and (>= chr #xCFFFE) (<= chr #xCFFFF))
            (and (>= chr #xDFFFE) (<= chr #xDFFFF))
            (and (>= chr #xEFFFE) (<= chr #xEFFFF))
            (and (>= chr #xFFFFE) (<= chr #xFFFFF))
            (and (>= chr #x10FFFE) (<= chr #x10FFFF))))
      t
      nil))

(defun surrogate-code-points-p (chr)
  "Returns t if chr is a surrogate-code-point as defined by StringPrep C.6. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345 or an integer expressed
   in hex, e.g. #xE001"
  (cond ((characterp chr) (setf chr (char-code chr)))
        ((stringp chr) (setf chr (parse-integer chr :radix 16)))
        (t nil))
  (if (and (>= chr #xD800)
           (<= chr #xDFFF))
      t
      nil))

(defun private-use-character-p (chr)
  "Returns t if chr is a code-point within the private range E000-F8FF; [PRIVATE USE, PLANE 0]
   F0000-FFFFD; [PRIVATE USE, PLANE 15]
   100000-10FFFD; [PRIVATE USE, PLANE 16]. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345 or an integer expressed
   in hex, e.g. #xE001"
  (cond ((characterp chr) (setf chr (char-code chr)))
        ((stringp chr) (setf chr (parse-integer chr :radix 16)))
        (t nil))
  (cond ((< chr #xE000) nil)
        ((or (and (>= chr #xE000)
                 (<= chr #xF8FF))
            (and (>= chr #xF0000)
                 (<= chr #xFFFFD))
            (and (>= chr #x100000)
                 (<= chr #x10FFFD)))
         t)))

(defun inappropriate-for-canonical-representation-character-p (chr)
  "Returns t if chr is a code-point considered by StringPrep C. as inappropriate for canonical representation characters. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345, a hex string  or an integer expressed in hex, e.g. #xE001"
  (cond ((and (characterp chr)
              (member chr '(#\U+2FF0 #\U+2FF1 #\U+2FF2 #\U+2FF3 #\U+2FF4 #\U+2FF5 #\U+2FF6 #\U+2FF7 #\U+2FF8 #\U+2FF9 #\U+2FFA #\U+2FFB)))
         t)
        ((and (integerp chr)
              (member chr '(12272 12273 12274 12275 12276 12277 12278 12279 12280 12281 12282 12283)))
         t)
        ((and (stringp chr)
              (member chr '("2FF0" "2FF1" "2FF2" "2FF3" "2FF4" "2FF5" "2FF6" "2FF7" "2FF8" "2FF9" "2FFA" "2FFB")))
         t)
        (t nil)))

(defun change-display-property-character-p (chr)
  "Returns t if chr is a code-point considered by StringPrep C.8 as a change of display properties. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345, a hex string  or an integer expressed in hex, e.g. #xE001"
  (cond ((and (characterp chr)
              (member chr '(#\Combining_Grave_Tone_Mark #\Combining_Acute_Tone_Mark #\U+200E #\U+200F #\U+202A #\U+202B #\U+202C #\U+202D #\U+202E #\U+206A #\U+206B #\U+206C #\U+206D #\U+206E #\U+206F)))
         t)
        ((and (integerp chr)
              (member chr '(#x0340 #x0341 #x200E #x200F #x202A #x202B #x202C #x202D #x202E #x206A #x206B #x206C #x206D #x206E #x206F)))
         t)
        ((and (stringp chr)
              (member chr '("0340" "0341" "200E" "200F" "202A" "202B" "202C" "202D" "202E" "206A" "206B" "206C" "206D" "206E" "206F")))
         t)
        (t nil)))

(defun char-printable-ascii-p (ch)
  "Returns t if the char is printable ascii."
  (gethash ch *printable-ascii*))

(defun string-printable-ascii-p (str)
  "Returns t if every character in the string is printable ascii."
  (every #'char-printable-ascii-p str))

(defun code-point-printable-ascii-p (int)
  "Returns t if the int is a printable ascii code-point."
  (and (>= int 32)
       (<= 126)))
