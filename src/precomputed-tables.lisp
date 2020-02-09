;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-
(in-package :saslprep)

(defparameter *data-directory* (uiop:merge-pathnames*
                                (make-pathname :directory (list :relative "data") :name nil :type nil)
                                (asdf:system-source-directory (asdf:find-system 'saslprep nil))))

;; http://www.unicode.org/L2/L1999/UnicodeData.html


(defvar *unicode-data*
  (with-open-file (in (uiop:merge-pathnames* *data-directory* "UnicodeData.txt"))
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
(with-open-file (in (uiop:merge-pathnames* *data-directory* "CompositionExclusions.txt"))
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

(defun get-chars-mapped-to-nothing (&optional (form :char))
  "Returns a list of characters commonly mapped to nothing per RFC 3454. Default returns a list of charactershex strings. This can be changed
by passing in an optional parameter of either :hex-string (returning a list of strings of the hexadecimal representation of the code-point),
 :hex (an integer represented in hex) or :code-points (which will return a list of decimals representing the code-points)."
  (cond ((eq form :char)
         (loop for x in '(#x00AD #x1806 #x200B #x2060 #xFEFF #x034F #x180B #x180C #x180D #x200C #x200D #xFE00 #xFE01 #xFE02 #xFE03 #xFE04 #xFE05 #xFE06 #xFE07 #xFE08 #xFE09 #xFE0A #xFE0B #xFE0C #xFE0D #xFE0E #xFE0F) collect (code-char x)))
        ((eq form :hex-string)
         '("00AD" "1806" "200B" "2060" "FEFF" "034F" "180B" "180C" "180D" "200C" "200D" "FE00" "FE01" "FE02" "FE03" "FE04" "FE05" "FE06" "FE07" "FE08" "FE09" "FE0A" "FE0B" "FE0C" "FE0D" "FE0E" "FE0F"))
        ((eq form :hex)
         '(#x00AD #x1806 #x200B #x2060 #xFEFF #x034F #x180B #x180C #x180D #x200C #x200D #xFE00 #xFE01 #xFE02 #xFE03 #xFE04 #xFE05 #xFE06 #xFE07 #xFE08 #xFE09 #xFE0A #xFE0B #xFE0C #xFE0D #xFE0E #xFE0F))
        ((eq form :code-points)
         '(173 6150 8203 8288 65279 847 6155 6156 6157 8204 8205 65024 65025 65026 65027 65028 65029 65030 65031 65032 65033 65034 65035 65036 65037 65038 65039))
        (t '())))

(defparameter *mapped-to-nothing* (make-hash-table :size 27))

(loop for x in (get-chars-mapped-to-nothing :char)  do (setf (gethash x *mapped-to-nothing*) t))

(defun get-chars-mapped-to-space (&optional (form :char))
  "Returns a list of characters mapped to space 0020 per RFC 3454. Default returns a list of character hex strings. This can be changed
by passing in an optional parameter of either :hex-string (returning a list of strings of the hexadecimal representation of the code-point),
 :hex (an integer represented in hex) or :code-points (which will return a list of decimals representing the code-points)."
  (cond ((eq form :char)
         (loop for x in '(#x00A0 #x1680 #x2000 #x2001 #x2002 #x2003 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x200B #x202F #x205F #x3000)
              collect (code-char x)))
        ((eq form :hex-string)
         '("00A0" "1680" "2000" "2001" "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "200A" "200B" "202F" "205F" "3000"))
        ((eq form :hex)
         '(#x00A0 #x1680 #x2000 #x2001 #x2002 #x2003 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x200B #x202F #x205F #x3000))
        ((eq form :code-points)
         '(160 5760 8192 8193 8194 8195 8196 8197 8198 8199 8200 8201 8202 8203 8239 8287 12288))
        (t '())))

(defun get-ascii-control-chars (&optional (form :char))
  "Returns a list of ascii control characters. Default returns a list of charactershex strings. This can be changed
by passing in an optional parameter of either :hex-string (returning a list of strings of the hexadecimal representation of the code-point),
 :hex (an integer represented in hex) or :code-points (which will return a list of decimals representing the code-points)."
  (cond ((eq form :char)
         (loop for x in '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8 #x9 #xA #xB #xC #xD #xE #xF #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F #x7F)
              collect (code-char x)))
        ((eq form :hex-string)
         '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "1A" "1B" "1C" "1D" "1E" "1F" "7F"))
        ((eq form :hex)
         '(#x0 #x1 #x2 #x3 #x4 #x5 #x6 #x7 #x8 #x9 #xA #xB #xC #xD #xE #xF #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1A #x1B #x1C #x1D #x1E #x1F #x7F))
        ((eq form :code-points)
         '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 127))
        (t '())))

(defun get-non-ascii-control-chars (&optional (form :char))
  "Returns a list of non-ascii control characters. Default returns a list of charactershex strings. This can be changed
by passing in an optional parameter of either :hex-string (returning a list of strings of the hexadecimal representation of the code-point),
 :hex (an integer represented in hex) or :code-points (which will return a list of decimals representing the code-points)."
  (cond ((eq form :char)
         (loop for x in '(#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x8B #x8C #x8D #x8E #x8F #x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D #x9E #x9F #x06DD #x070F #x180E #x200C #x200D #x2028 #x2029 #x2060 #x2061 #x2062 #x2063 #x206A #x206B #x206C #x206D #x206E #x206F #xFEFF #xFFF9 #xFFFA #xFFFB #xFFFC #x1D173 #x1D174 #x1D175 #x1D176 #x1D177 #x1D178 #x1D179 #x1D17A)
              collect (code-char x)))
        ((eq form :hex-string)
         '("80" "81" "82" "83" "84" "85" "86" "87" "88" "89" "8A" "8B" "8C" "8D" "8E" "8F" "90" "91" "92" "93" "94" "95" "96" "97" "98" "99" "9A" "9B" "9C" "9D" "9E" "9F" "06DD" "070F" "180E" "200C" "200D" "2028" "2029" "2060" "2061" "2062" "2063" "206A" "206B" "206C" "206D" "206E" "206F" "FEFF" "FFF9" "FFFA" "FFFB" "FFFC" "1D173" "1D174""1D175" "1D176""1D177" "1D178""1D179" "1D17A"))
        ((eq form :hex)
         '(#x80 #x81 #x82 #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x8B #x8C #x8D #x8E #x8F #x90 #x91 #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 #x9A #x9B #x9C #x9D #x9E #x9F #x06DD #x070F #x180E #x200C #x200D #x2028 #x2029 #x2060 #x2061 #x2062 #x2063 #x206A #x206B #x206C #x206D #x206E #x206F #xFEFF #xFFF9 #xFFFA #xFFFB #xFFFC #x1D173 #x1D174 #x1D175 #x1D176 #x1D177 #x1D178 #x1D179 #x1D17A))
        ((eq form :code-points)
         '(128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 153 154 155 156 157 158 159 1757 1807 6158 8204 8205 8232 8233 8288 8289 8290 8291 8298 8299 8300 8301 8302 8303 65279 65529 65530 65531 65532 119155 119156 119157 119158 119159 119160 119161 119162))
        (t '())))

(defun get-tagging-characters (&optional (form :char))
  "Returns a list of tagging characters as defined in StringPrep C.9. Default returns a list of charactershex strings. This can be changed
by passing in an optional parameter of either :hex-string (returning a list of strings of the hexadecimal representation of the code-point),
 :hex (an integer represented in hex) or :code-points (which will return a list of decimals representing the code-points)."
  (cond ((eq form :char)
         (loop for x in '(917536 917537 917538 917539 917540 917541 917542 917543 917544 917545 917546 917547 917548 917549 917550 917551 917552 917553 917554 917555 917556 917557 917558 917559 917560 917561 917562 917563 917564 917565 917566 917567 917568 917569 917570 917571 917572 917573 917574 917575 917576 917577 917578 917579 917580 917581 917582 917583 917584 917585 917586 917587 917588 917589 917590 917591 917592 917593 917594 917595 917596 917597 917598 917599 917600 917601 917602 917603 917604 917605 917606 917607 917608 917609 917610 917611 917612 917613 917614 917615 917616 917617 917618 917619 917620 917621 917622 917623 917624 917625 917626 917627 917628 917629 917630 917631)
              collect (code-char x)))
        ((eq form :hex-string)
         '("E0020" "E0021" "E0022" "E0023" "E0024" "E0025" "E0026" "E0027" "E0028" "E0029" "E002A" "E002B" "E002C" "E002D" "E002E" "E002F" "E0030" "E0031" "E0032" "E0033" "E0034" "E0035" "E0036" "E0037" "E0038" "E0039" "E003A" "E003B" "E003C" "E003D" "E003E" "E003F" "E0040" "E0041" "E0042" "E0043" "E0044" "E0045" "E0046" "E0047" "E0048" "E0049" "E004A" "E004B" "E004C" "E004D" "E004E" "E004F" "E0050" "E0051" "E0052" "E0053" "E0054" "E0055" "E0056" "E0057" "E0058" "E0059" "E005A" "E005B" "E005C" "E005D" "E005E" "E005F" "E0060" "E0061" "E0062" "E0063" "E0064" "E0065" "E0066" "E0067" "E0068" "E0069" "E006A" "E006B" "E006C" "E006D" "E006E" "E006F" "E0070" "E0071" "E0072" "E0073" "E0074" "E0075" "E0076" "E0077" "E0078" "E0079" "E007A" "E007B" "E007C" "E007D" "E007E" "E007F"))
        ((eq form :code-points)
         '(917536 917537 917538 917539 917540 917541 917542 917543 917544 917545 917546 917547 917548 917549 917550 917551 917552 917553 917554 917555 917556 917557 917558 917559 917560 917561 917562 917563 917564 917565 917566 917567 917568 917569 917570 917571 917572 917573 917574 917575 917576 917577 917578 917579 917580 917581 917582 917583 917584 917585 917586 917587 917588 917589 917590 917591 917592 917593 917594 917595 917596 917597 917598 917599 917600 917601 917602 917603 917604 917605 917606 917607 917608 917609 917610 917611 917612 917613 917614 917615 917616 917617 917618 917619 917620 917621 917622 917623 917624 917625 917626 917627 917628 917629 917630 917631))
        (t '())))

;; With a small set of code points, checking a hash table or checking the ranges has very little difference in speed
(defparameter *non-character-code-points* (make-hash-table))

(loop for x in '((#xFDD0 . #xFDEF)(#xFFFE .  #xFFFF)(#x1FFFE .  #x1FFFF)(#x2FFFE .  #x2FFFF)(#x3FFFE .  #x3FFFF)(#x4FFFE .  #x4FFFF)
   (#x5FFFE .  #x5FFFF)(#x6FFFE .  #x6FFFF)(#x7FFFE .  #x7FFFF)(#x8FFFE .  #x8FFFF)(#x9FFFE .  #x9FFFF)(#xAFFFE .  #xAFFFF)
   (#xBFFFE .  #xBFFFF)(#xCFFFE .  #xCFFFF)(#xDFFFE .  #xDFFFF)(#xEFFFE .  #xEFFFF)(#xFFFFE .  #xFFFFF)(#x10FFFE .  #x10FFFF))
   do (loop for y from (car x) to (cdr x) do (setf (gethash (code-char y) *non-character-code-points*) t)))

(defun get-inappropriate-for-plain-text-characters (&optional (form :char))
  "Returns a list of characters considered inappropriate for plain text characters as defined in StringPrep C.6. Default returns a list of charactershex strings. This can be changed by passing in an optional parameter of either :hex-string (returning a list of strings of the hexadecimal representation of the code-point)  or :code-points (which will return a list of decimals representing the code-points)."
  (cond ((eq form :char)
         (loop for x in '(65529 65530 65531 65532 65533) collect (code-char x)))
        ((eq form :hex-string)
         '("FFF9" "FFFA" "FFFB" "FFFC" "FFFD"))
        ((eq form :code-points)
         '(65529 65530 65531 65532 65533))
        (t '())))

(defun get-inappropriate-for-canonical-representation-char-p (&optional (form :char))
  "Returns a list of characters considered inappropriate for canonical representation under RFC 3454 Table C.7. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345, a hex string  or an integer expressed in hex, e.g. #xE001"
  (cond ((eq form :char)
                  (loop for x in '(12272 12273 12274 12275 12276 12277 12278 12279 12280 12281 12282 12283) collect (code-char x)))
        ((eq form :code-points)
         '(12272 12273 12274 12275 12276 12277 12278 12279 12280 12281 12282 12283))
        ((eq form :hex-string)
              '("2FF0" "2FF1" "2FF2" "2FF3" "2FF4" "2FF5" "2FF6" "2FF7" "2FF8" "2FF9" "2FFA" "2FFB"))
        (t nil)))

(defun get-printable-ascii (&optional (form :char))
  "Returns a list of the printable ascii characters (32 to 126). This does not include extended ascii.
The default format is to return a list of characters.
This can be changed by passing the optional :char-string (to return a list of strings containing the character) or :hex-string (to return a
list of string containing the code-point as a hexadecimal) result or :code-point (to return a list of decimal numbers). Passing any other
parameter will return an empty list."
  (cond ((eq form :char)
         '(#\  #\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+ #\, #\- #\. #\/ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\: #\; #\< #\= #\> #\? #\@ #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\[ #\\ #\] #\^ #\_ #\` #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z #\{ #\| #\} #\~))
    ((eq form :char-string)
         '(" " "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" ":" ";" "<" "=" ">" "?" "@" "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "[" "\\" "]" "^" "_" "`" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "{" "|" "}" "~"))
        ((eq form :hex-string)
         '("20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "2A" "2B" "2C" "2D" "2E" "2F" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "3A" "3B" "3C" "3D" "3E" "3F" "40" "41" "42" "43" "44" "45" "46" "47" "48" "49" "4A" "4B" "4C" "4D" "4E" "4F" "50" "51" "52" "53" "54" "55" "56" "57" "58" "59" "5A" "5B" "5C" "5D" "5E" "5F" "60" "61" "62" "63" "64" "65" "66" "67" "68" "69" "6A" "6B" "6C" "6D" "6E" "6F" "70" "71" "72" "73" "74" "75" "76" "77" "78" "79" "7A" "7B" "7C" "7D" "7E" ))
        ((eq form :code-point)
         '(32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126))
        (t ())))

(defparameter *printable-ascii* (make-hash-table :size 95))

(loop for x in (get-printable-ascii) do (setf (gethash x *printable-ascii*) t))
