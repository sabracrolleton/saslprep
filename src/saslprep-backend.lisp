;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-POSTGRES; -*-

(in-package :saslprep)

(defun char-mapped-to-nothing-p (chr)
  "Returns t if the character should be mapped to nothing per RFC 3454 Table B.1 and RFC 4013"
;  (gethash ch hsh)
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to char-mapped-to-nothing-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (member chr-code-point '(#x00AD #x1806 #x200B #x2060 #xFEFF #x034F #x180B #x180C #x180D #x200C #x200D #xFE00 #xFE01 #xFE02 #xFE03 #xFE04 #xFE05 #xFE06 #xFE07 #xFE08 #xFE09 #xFE0A #xFE0B #xFE0C #xFE0D #xFE0E #xFE0F))
        t
        nil)))

(defun char-mapped-to-space-p (chr)
  "If character is mapped to space per RFC 3454 Table C.1.2 and RFC 4013, then return t, else nil"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to char-mapped-to-space-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
  (if (member chr-code-point '(#x00A0 #x1680 #x2000 #x2001 #x2002 #x2003 #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x200B #x202F #x205F #x3000))
      t
      nil)))

(defun non-char-code-point-p (chr)
  "Returns t if the parameter is a non-character code point as defined in RFC 3454 Table C.4. and RFC 4013"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to non-character-code-point-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
  (if (and (>= chr-code-point #xFDD0)
           (or
            (and (>= chr-code-point #xFDD0) (<= chr-code-point #xFDEF))
            (and (>= chr-code-point #xFFFE) (<= chr-code-point #xFFFF))
            (and (>= chr-code-point #x1FFFE) (<= chr-code-point #x1FFFF))
            (and (>= chr-code-point #x2FFFE) (<= chr-code-point #x2FFFF))
            (and (>= chr-code-point #x3FFFE) (<= chr-code-point #x3FFFF))
            (and (>= chr-code-point #x4FFFE) (<= chr-code-point #x4FFFF))
            (and (>= chr-code-point #x5FFFE) (<= chr-code-point #x5FFFF))
            (and (>= chr-code-point #x6FFFE) (<= chr-code-point #x6FFFF))
            (and (>= chr-code-point #x7FFFE) (<= chr-code-point #x7FFFF))
            (and (>= chr-code-point #x8FFFE) (<= chr-code-point #x8FFFF))
            (and (>= chr-code-point #x9FFFE) (<= chr-code-point #x9FFFF))
            (and (>= chr-code-point #xAFFFE) (<= chr-code-point #xAFFFF))
            (and (>= chr-code-point #xBFFFE) (<= chr-code-point #xBFFFF))
            (and (>= chr-code-point #xCFFFE) (<= chr-code-point #xCFFFF))
            (and (>= chr-code-point #xDFFFE) (<= chr-code-point #xDFFFF))
            (and (>= chr-code-point #xEFFFE) (<= chr-code-point #xEFFFF))
            (and (>= chr-code-point #xFFFFE) (<= chr-code-point #xFFFFF))
            (and (>= chr-code-point #x10FFFE) (<= chr-code-point #x10FFFF))))
      t
      nil)))

(defun non-ascii-space-char-p (chr)
  "Returns t if the parameter is a non-ascii-space character as defined in RFC 3454 Table C.1.2. and RFC 4013"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to non-ascii-space-char-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (or
         (= chr-code-point #x00A0)
         (= chr-code-point #x1680)
         (= chr-code-point #x2000)
         (= chr-code-point #x2001)
         (= chr-code-point #x2002)
         (= chr-code-point #x2003)
         (= chr-code-point #x2004)
         (= chr-code-point #x2005)
         (= chr-code-point #x2006)
         (= chr-code-point #x2007)
         (= chr-code-point #x2008)
         (= chr-code-point #x2009)
         (= chr-code-point #x200A)
         (= chr-code-point #x200B)
         (= chr-code-point #x202F)
         (= chr-code-point #x205F)
         (= chr-code-point #x3000))
      t
      nil)))

(defun ascii-control-char-p (chr)
  "Returns t if chr is an ascii control character as defined by RFC 3454 Table C.2.1"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to ascii-control-char-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (or (and (>= chr-code-point #x0000) (<= chr-code-point #x001F))
            (= chr-code-point #x007F))
        t nil)))

(defun non-ascii-control-char-p (chr)
  "Returns t if chr is an non-ascii control character as defined by RFC 3454 Table C.2.2. Chr must be a character."
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to non-ascii-control-char-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (or (and (>= chr-code-point #x0080) (<= chr-code-point #x009F))
            (and (>= chr-code-point #x206A) (<= chr-code-point #x206F))
            (and (>= chr-code-point #xFFF9) (<= chr-code-point #xFFFC))
            (= chr-code-point #x06DD)
            (= chr-code-point #x070F)
            (= chr-code-point #x180E)
            (= chr-code-point #x200C)
            (= chr-code-point #x200D)
            (= chr-code-point #x2028)
            (= chr-code-point #x2029)
            (= chr-code-point #x2060)
            (= chr-code-point #x2061)
            (= chr-code-point #x2062)
            (= chr-code-point #x2063)
            (= chr-code-point #xFEFF))
        t nil)))

(defun surrogate-code-points-p (chr)
  "Returns t if chr is a surrogate-code-point as defined by RFC 3454 Table C.5. The parameter must be a character, e.g. #\U+E001"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to private-use-character-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (and (>= chr-code-point #xD800)
             (<= chr-code-point #xDFFF))
        t
        nil)))

(defun private-use-char-p (chr)
  "Returns t if chr is a code-point within the private range E000-F8FF; [PRIVATE USE, PLANE 0]
   F0000-FFFFD; [PRIVATE USE, PLANE 15]
   100000-10FFFD; [PRIVATE USE, PLANE 16]. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345 or an integer expressed
   in hex, e.g. #xE001 as set out in RFC 3454 Table C.3 and RFC 4013"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to private-use-character-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
  (cond ((< chr-code-point #xE000) nil)
        ((or (and (>= chr-code-point #xE000)
                 (<= chr-code-point #xF8FF))
            (and (>= chr-code-point #xF0000)
                 (<= chr-code-point #xFFFFD))
            (and (>= chr-code-point #x100000)
                 (<= chr-code-point #x10FFFD)))
         t)
        (t nil))))

(defun inappropriate-for-plain-text-p (chr)
  "Returns t if the parameter is inappropriate-for-plain-text-p as defined in RFC 3454 Table C.6 and RFC 4013."
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to non-ascii-space-char-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (and (>= chr-code-point #xFFF9) (<= chr-code-point #xFFFD))
        t
        nil)))

(defun inappropriate-for-canonical-representation-char-p (chr)
  "Returns t if chr is a code-point considered by StringPrep  RFC 3454 Table C.7 and RFC 4013 as inappropriate for canonical representation characters. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345, a hex string  or an integer expressed in hex, e.g. #xE001"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to non-ascii-space-char-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (and (>= chr-code-point #x2FF0) (<= chr-code-point #x2FFB))
        t
        nil)))

(defun tagging-char-p (chr)
  "Returns t if the parameter is a tagging character defined in RFC 3454 Table C.9 and RFC 4013."
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to tagging-char-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (or
         (= chr-code-point #xE0001)
         (and (>= chr-code-point #xE0020) (<= chr-code-point #xE007F)))
        t
        nil)))

(defun change-display-property-char-p (chr)
  "Returns t if chr is a code-point considered by RFC 3454 TAble C.8 and RFC 4013 as a change of display properties. The parameter can be either a character, e.g. #\U+E001, a decimal code point e.g. 57345, a hex string  or an integer expressed in hex, e.g. #xE001"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to tagging-char-p" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer chr-code-point))
    (if (member chr-code-point '(#x0340 #x0341 #x200E #x200F #x202A #x202B #x202C #x202D #x202E #x206A #x206B #x206C #x206D #x206E #x206F))
        t
        nil)))

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

(defun char-with-bidirectional-property-R-or-AL-p (chr)
  "Returns t if char has bidirectional property R or AL per RFC 3454 Table D.1 and RFC 4013"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to char-with-bidirectional-property-R-or-AL" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer ))
    (if (or
         (member chr-code-point '(#x05BE #x05C0 #x05C3  #x061B #x061F #x06DD #x0710 #x07B1 #x200F #xFB1D #xFB3E))
               (and (>= chr-code-point #x05D0)(<= chr-code-point #x05EA))
               (and (>= chr-code-point #x05F0)(<= chr-code-point #x05F4))
               (and (>= chr-code-point #x0621)(<= chr-code-point #x063A))
               (and (>= chr-code-point #x0640)(<= chr-code-point #x064A))
               (and (>= chr-code-point #x066D)(<= chr-code-point #x066F))
               (and (>= chr-code-point #x0671)(<= chr-code-point #x06D5))
               (and (>= chr-code-point #x06E5)(<= chr-code-point #x06E6))
               (and (>= chr-code-point #x06FA)(<= chr-code-point #x06FE))
               (and (>= chr-code-point #x0700)(<= chr-code-point #x070D))
               (and (>= chr-code-point #x0712)(<= chr-code-point #x072C))
               (and (>= chr-code-point #x0780)(<= chr-code-point #x07A5))
               (and (>= chr-code-point #xFB1F)(<= chr-code-point #xFB28))
               (and (>= chr-code-point #xFB2A)(<= chr-code-point #xFB36))
               (and (>= chr-code-point #xFB38)(<= chr-code-point #xFB3C))
               (and (>= chr-code-point #xFB40)(<= chr-code-point #xFB41))
               (and (>= chr-code-point #xFB43)(<= chr-code-point #xFB44))
               (and (>= chr-code-point #xFB46)(<= chr-code-point #xFBB1))
               (and (>= chr-code-point #xFBD3)(<= chr-code-point #xFD3D))
               (and (>= chr-code-point #xFD50)(<= chr-code-point #xFD8F))
               (and (>= chr-code-point #xFD92)(<= chr-code-point #xFDC7))
               (and (>= chr-code-point #xFDF0)(<= chr-code-point #xFDFC))
               (and (>= chr-code-point #xFE70)(<= chr-code-point #xFE74))
               (and (>= chr-code-point #xFE76)(<= chr-code-point #xFEFC)))
              t nil)))

(defun char-with-bidirectional-property-L-p (chr)
  "Returns t if chr has bidirectional property L per RFC 3454 Table D.2 and RFC 4013"
  (when (not (characterp chr))
    (bad-char-error "Passing unknown type data to char-with-bidirectional-property-L" :value chr))
  (let ((chr-code-point (char-code chr)))
    (declare (optimize speed)
             (integer ))
  (if (or
       (and (>= chr-code-point #x0041)(<= chr-code-point #x005A))
       (and (>= chr-code-point  #x0061)(<= chr-code-point #x007A))
       (and (>= chr-code-point  #x00C0)(<= chr-code-point #x00D6))
       (and (>= chr-code-point  #x00D8)(<= chr-code-point #x00F6))
       (and (>= chr-code-point  #x00F8)(<= chr-code-point #x0220))
       (and (>= chr-code-point  #x0222)(<= chr-code-point #x0233))
       (and (>= chr-code-point  #x0250)(<= chr-code-point #x02AD))
       (and (>= chr-code-point  #x02B0)(<= chr-code-point #x02B8))
       (and (>= chr-code-point  #x02BB)(<= chr-code-point #x02C1))
       (and (>= chr-code-point  #x02D0)(<= chr-code-point #x02D1))
       (and (>= chr-code-point  #x02E0)(<= chr-code-point #x02E4))
       (and (>= chr-code-point  #x0388)(<= chr-code-point #x038A))
       (and (>= chr-code-point  #x038E)(<= chr-code-point #x03A1))
       (and (>= chr-code-point  #x03A3)(<= chr-code-point #x03CE))
       (and (>= chr-code-point  #x03D0)(<= chr-code-point #x03F5))
       (and (>= chr-code-point  #x0400)(<= chr-code-point #x0482))
       (and (>= chr-code-point  #x048A)(<= chr-code-point #x04CE))
       (and (>= chr-code-point  #x04D0)(<= chr-code-point #x04F5))
       (and (>= chr-code-point  #x04F8)(<= chr-code-point #x04F9))
       (and (>= chr-code-point  #x0500)(<= chr-code-point #x050F))
       (and (>= chr-code-point  #x0531)(<= chr-code-point #x0556))
       (and (>= chr-code-point  #x0559)(<= chr-code-point #x055F))
       (and (>= chr-code-point  #x0561)(<= chr-code-point #x0587))
       (and (>= chr-code-point  #x0905)(<= chr-code-point #x0939))
       (and (>= chr-code-point  #x093D)(<= chr-code-point #x0940))
       (and (>= chr-code-point  #x0949)(<= chr-code-point #x094C))
       (and (>= chr-code-point  #x0958)(<= chr-code-point #x0961))
       (and (>= chr-code-point  #x0964)(<= chr-code-point #x0970))
       (and (>= chr-code-point  #x0982)(<= chr-code-point #x0983))
       (and (>= chr-code-point  #x0985)(<= chr-code-point #x098C))
       (and (>= chr-code-point  #x098F)(<= chr-code-point #x0990))
       (and (>= chr-code-point  #x0993)(<= chr-code-point #x09A8))
       (and (>= chr-code-point  #x09AA)(<= chr-code-point #x09B0))
       (and (>= chr-code-point  #x09B6)(<= chr-code-point #x09B9))
       (and (>= chr-code-point  #x09BE)(<= chr-code-point #x09C0))
       (and (>= chr-code-point  #x09C7)(<= chr-code-point #x09C8))
       (and (>= chr-code-point  #x09CB)(<= chr-code-point #x09CC))
       (and (>= chr-code-point  #x09DC)(<= chr-code-point #x09DD))
       (and (>= chr-code-point  #x09DF)(<= chr-code-point #x09E1))
       (and (>= chr-code-point  #x09E6)(<= chr-code-point #x09F1))
       (and (>= chr-code-point  #x09F4)(<= chr-code-point #x09FA))
       (and (>= chr-code-point  #x0A05)(<= chr-code-point #x0A0A))
       (and (>= chr-code-point  #x0A0F)(<= chr-code-point #x0A10))
       (and (>= chr-code-point  #x0A13)(<= chr-code-point #x0A28))
       (and (>= chr-code-point  #x0A2A)(<= chr-code-point #x0A30))
       (and (>= chr-code-point  #x0A32)(<= chr-code-point #x0A33))
       (and (>= chr-code-point  #x0A35)(<= chr-code-point #x0A36))
       (and (>= chr-code-point  #x0A38)(<= chr-code-point #x0A39))
       (and (>= chr-code-point  #x0A3E)(<= chr-code-point #x0A40))
       (and (>= chr-code-point  #x0A59)(<= chr-code-point #x0A5C))
       (and (>= chr-code-point  #x0A66)(<= chr-code-point #x0A6F))
       (and (>= chr-code-point  #x0A72)(<= chr-code-point #x0A74))
       (and (>= chr-code-point  #x0A85)(<= chr-code-point #x0A8B))
       (and (>= chr-code-point  #x0A8F)(<= chr-code-point #x0A91))
       (and (>= chr-code-point  #x0A93)(<= chr-code-point #x0AA8))
       (and (>= chr-code-point  #x0AAA)(<= chr-code-point #x0AB0))
       (and (>= chr-code-point  #x0AB2)(<= chr-code-point #x0AB3))
       (and (>= chr-code-point  #x0AB5)(<= chr-code-point #x0AB9))
       (and (>= chr-code-point  #x0ABD)(<= chr-code-point #x0AC0))
       (and (>= chr-code-point  #x0ACB)(<= chr-code-point #x0ACC))
       (and (>= chr-code-point  #x0AE6)(<= chr-code-point #x0AEF))
       (and (>= chr-code-point  #x0B02)(<= chr-code-point #x0B03))
       (and (>= chr-code-point  #x0B05)(<= chr-code-point #x0B0C))
       (and (>= chr-code-point  #x0B0F)(<= chr-code-point #x0B10))
       (and (>= chr-code-point  #x0B13)(<= chr-code-point #x0B28))
       (and (>= chr-code-point  #x0B2A)(<= chr-code-point #x0B30))
       (and (>= chr-code-point  #x0B32)(<= chr-code-point #x0B33))
       (and (>= chr-code-point  #x0B36)(<= chr-code-point #x0B39))
       (and (>= chr-code-point  #x0B3D)(<= chr-code-point #x0B3E))
       (and (>= chr-code-point  #x0B47)(<= chr-code-point #x0B48))
       (and (>= chr-code-point  #x0B4B)(<= chr-code-point #x0B4C))
       (and (>= chr-code-point  #x0B5C)(<= chr-code-point #x0B5D))
       (and (>= chr-code-point  #x0B5F)(<= chr-code-point #x0B61))
       (and (>= chr-code-point  #x0B66)(<= chr-code-point #x0B70))
       (and (>= chr-code-point  #x0B85)(<= chr-code-point #x0B8A))
       (and (>= chr-code-point  #x0B8E)(<= chr-code-point #x0B90))
       (and (>= chr-code-point  #x0B92)(<= chr-code-point #x0B95))
       (and (>= chr-code-point  #x0B99)(<= chr-code-point #x0B9A))
       (and (>= chr-code-point  #x0B9E)(<= chr-code-point #x0B9F))
       (and (>= chr-code-point  #x0BA3)(<= chr-code-point #x0BA4))
       (and (>= chr-code-point  #x0BA8)(<= chr-code-point #x0BAA))
       (and (>= chr-code-point  #x0BAE)(<= chr-code-point #x0BB5))
       (and (>= chr-code-point  #x0BB7)(<= chr-code-point #x0BB9))
       (and (>= chr-code-point  #x0BBE)(<= chr-code-point #x0BBF))
       (and (>= chr-code-point  #x0BC1)(<= chr-code-point #x0BC2))
       (and (>= chr-code-point  #x0BC6)(<= chr-code-point #x0BC8))
       (and (>= chr-code-point  #x0BCA)(<= chr-code-point #x0BCC))
       (and (>= chr-code-point  #x0BE7)(<= chr-code-point #x0BF2))
       (and (>= chr-code-point  #x0C01)(<= chr-code-point #x0C03))
       (and (>= chr-code-point  #x0C05)(<= chr-code-point #x0C0C))
       (and (>= chr-code-point  #x0C0E)(<= chr-code-point #x0C10))
       (and (>= chr-code-point  #x0C12)(<= chr-code-point #x0C28))
       (and (>= chr-code-point  #x0C2A)(<= chr-code-point #x0C33))
       (and (>= chr-code-point  #x0C35)(<= chr-code-point #x0C39))
       (and (>= chr-code-point  #x0C41)(<= chr-code-point #x0C44))
       (and (>= chr-code-point  #x0C60)(<= chr-code-point #x0C61))
       (and (>= chr-code-point  #x0C66)(<= chr-code-point #x0C6F))
       (and (>= chr-code-point  #x0C82)(<= chr-code-point #x0C83))
       (and (>= chr-code-point  #x0C85)(<= chr-code-point #x0C8C))
       (and (>= chr-code-point  #x0C8E)(<= chr-code-point #x0C90))
       (and (>= chr-code-point  #x0C92)(<= chr-code-point #x0CA8))
       (and (>= chr-code-point  #x0CAA)(<= chr-code-point #x0CB3))
       (and (>= chr-code-point  #x0CB5)(<= chr-code-point #x0CB9))
       (and (>= chr-code-point  #x0CC0)(<= chr-code-point #x0CC4))
       (and (>= chr-code-point  #x0CC7)(<= chr-code-point #x0CC8))
       (and (>= chr-code-point  #x0CCA)(<= chr-code-point #x0CCB))
       (and (>= chr-code-point  #x0CD5)(<= chr-code-point #x0CD6))
       (and (>= chr-code-point  #x0CE0)(<= chr-code-point #x0CE1))
       (and (>= chr-code-point  #x0CE6)(<= chr-code-point #x0CEF))
       (and (>= chr-code-point  #x0D02)(<= chr-code-point #x0D03))
       (and (>= chr-code-point  #x0D05)(<= chr-code-point #x0D0C))
       (and (>= chr-code-point  #x0D0E)(<= chr-code-point #x0D10))
       (and (>= chr-code-point  #x0D12)(<= chr-code-point #x0D28))
       (and (>= chr-code-point  #x0D2A)(<= chr-code-point #x0D39))
       (and (>= chr-code-point  #x0D3E)(<= chr-code-point #x0D40))
       (and (>= chr-code-point  #x0D46)(<= chr-code-point #x0D48))
       (and (>= chr-code-point  #x0D4A)(<= chr-code-point #x0D4C))
       (and (>= chr-code-point  #x0D60)(<= chr-code-point #x0D61))
       (and (>= chr-code-point  #x0D66)(<= chr-code-point #x0D6F))
       (and (>= chr-code-point  #x0D82)(<= chr-code-point #x0D83))
       (and (>= chr-code-point  #x0D85)(<= chr-code-point #x0D96))
       (and (>= chr-code-point  #x0D9A)(<= chr-code-point #x0DB1))
       (and (>= chr-code-point  #x0DB3)(<= chr-code-point #x0DBB))
       (and (>= chr-code-point  #x0DC0)(<= chr-code-point #x0DC6))
       (and (>= chr-code-point  #x0DCF)(<= chr-code-point #x0DD1))
       (and (>= chr-code-point  #x0DD8)(<= chr-code-point #x0DDF))
       (and (>= chr-code-point  #x0DF2)(<= chr-code-point #x0DF4))
       (and (>= chr-code-point  #x0E01)(<= chr-code-point #x0E30))
       (and (>= chr-code-point  #x0E32)(<= chr-code-point #x0E33))
       (and (>= chr-code-point  #x0E40)(<= chr-code-point #x0E46))
       (and (>= chr-code-point  #x0E4F)(<= chr-code-point #x0E5B))
       (and (>= chr-code-point  #x0E81)(<= chr-code-point #x0E82))
       (and (>= chr-code-point  #x0E87)(<= chr-code-point #x0E88))
       (and (>= chr-code-point  #x0E94)(<= chr-code-point #x0E97))
       (and (>= chr-code-point  #x0E99)(<= chr-code-point #x0E9F))
       (and (>= chr-code-point  #x0EA1)(<= chr-code-point #x0EA3))
       (and (>= chr-code-point  #x0EAA)(<= chr-code-point #x0EAB))
       (and (>= chr-code-point  #x0EAD)(<= chr-code-point #x0EB0))
       (and (>= chr-code-point  #x0EB2)(<= chr-code-point #x0EB3))
       (and (>= chr-code-point  #x0EC0)(<= chr-code-point #x0EC4))
       (and (>= chr-code-point  #x0ED0)(<= chr-code-point #x0ED9))
       (and (>= chr-code-point  #x0EDC)(<= chr-code-point #x0EDD))
       (and (>= chr-code-point  #x0F00)(<= chr-code-point #x0F17))
       (and (>= chr-code-point  #x0F1A)(<= chr-code-point #x0F34))
       (and (>= chr-code-point  #x0F3E)(<= chr-code-point #x0F47))
       (and (>= chr-code-point  #x0F49)(<= chr-code-point #x0F6A))
       (and (>= chr-code-point  #x0F88)(<= chr-code-point #x0F8B))
       (and (>= chr-code-point  #x0FBE)(<= chr-code-point #x0FC5))
       (and (>= chr-code-point  #x0FC7)(<= chr-code-point #x0FCC))
       (and (>= chr-code-point  #x1000)(<= chr-code-point #x1021))
       (and (>= chr-code-point  #x1023)(<= chr-code-point #x1027))
       (and (>= chr-code-point  #x1029)(<= chr-code-point #x102A))
       (and (>= chr-code-point  #x1040)(<= chr-code-point #x1057))
       (and (>= chr-code-point  #x10A0)(<= chr-code-point #x10C5))
       (and (>= chr-code-point  #x10D0)(<= chr-code-point #x10F8))
       (and (>= chr-code-point  #x1100)(<= chr-code-point #x1159))
       (and (>= chr-code-point  #x115F)(<= chr-code-point #x11A2))
       (and (>= chr-code-point  #x11A8)(<= chr-code-point #x11F9))
       (and (>= chr-code-point  #x1200)(<= chr-code-point #x1206))
       (and (>= chr-code-point  #x1208)(<= chr-code-point #x1246))
       (and (>= chr-code-point  #x124A)(<= chr-code-point #x124D))
       (and (>= chr-code-point  #x1250)(<= chr-code-point #x1256))
       (and (>= chr-code-point  #x125A)(<= chr-code-point #x125D))
       (and (>= chr-code-point  #x1260)(<= chr-code-point #x1286))
       (and (>= chr-code-point  #x128A)(<= chr-code-point #x128D))
       (and (>= chr-code-point  #x1290)(<= chr-code-point #x12AE))
       (and (>= chr-code-point  #x12B2)(<= chr-code-point #x12B5))
       (and (>= chr-code-point  #x12B8)(<= chr-code-point #x12BE))
       (and (>= chr-code-point  #x12C2)(<= chr-code-point #x12C5))
       (and (>= chr-code-point  #x12C8)(<= chr-code-point #x12CE))
       (and (>= chr-code-point  #x12D0)(<= chr-code-point #x12D6))
       (and (>= chr-code-point  #x12D8)(<= chr-code-point #x12EE))
       (and (>= chr-code-point  #x12F0)(<= chr-code-point #x130E))
       (and (>= chr-code-point  #x1312)(<= chr-code-point #x1315))
       (and (>= chr-code-point  #x1318)(<= chr-code-point #x131E))
       (and (>= chr-code-point  #x1320)(<= chr-code-point #x1346))
       (and (>= chr-code-point  #x1348)(<= chr-code-point #x135A))
       (and (>= chr-code-point  #x1361)(<= chr-code-point #x137C))
       (and (>= chr-code-point  #x13A0)(<= chr-code-point #x13F4))
       (and (>= chr-code-point  #x1401)(<= chr-code-point #x1676))
       (and (>= chr-code-point  #x1681)(<= chr-code-point #x169A))
       (and (>= chr-code-point  #x16A0)(<= chr-code-point #x16F0))
       (and (>= chr-code-point  #x1700)(<= chr-code-point #x170C))
       (and (>= chr-code-point  #x170E)(<= chr-code-point #x1711))
       (and (>= chr-code-point  #x1720)(<= chr-code-point #x1731))
       (and (>= chr-code-point  #x1735)(<= chr-code-point #x1736))
       (and (>= chr-code-point  #x1740)(<= chr-code-point #x1751))
       (and (>= chr-code-point  #x1760)(<= chr-code-point #x176C))
       (and (>= chr-code-point  #x176E)(<= chr-code-point #x1770))
       (and (>= chr-code-point  #x1780)(<= chr-code-point #x17B6))
       (and (>= chr-code-point  #x17BE)(<= chr-code-point #x17C5))
       (and (>= chr-code-point  #x17C7)(<= chr-code-point #x17C8))
       (and (>= chr-code-point  #x17D4)(<= chr-code-point #x17DA))
       (and (>= chr-code-point  #x17E0)(<= chr-code-point #x17E9))
       (and (>= chr-code-point  #x1810)(<= chr-code-point #x1819))
       (and (>= chr-code-point  #x1820)(<= chr-code-point #x1877))
       (and (>= chr-code-point  #x1880)(<= chr-code-point #x18A8))
       (and (>= chr-code-point  #x1E00)(<= chr-code-point #x1E9B))
       (and (>= chr-code-point  #x1EA0)(<= chr-code-point #x1EF9))
       (and (>= chr-code-point  #x1F00)(<= chr-code-point #x1F15))
       (and (>= chr-code-point  #x1F18)(<= chr-code-point #x1F1D))
       (and (>= chr-code-point  #x1F20)(<= chr-code-point #x1F45))
       (and (>= chr-code-point  #x1F48)(<= chr-code-point #x1F4D))
       (and (>= chr-code-point  #x1F50)(<= chr-code-point #x1F57))
       (and (>= chr-code-point  #x1F5F)(<= chr-code-point #x1F7D))
       (and (>= chr-code-point  #x1F80)(<= chr-code-point #x1FB4))
       (and (>= chr-code-point  #x1FB6)(<= chr-code-point #x1FBC))
       (and (>= chr-code-point  #x1FC2)(<= chr-code-point #x1FC4))
       (and (>= chr-code-point  #x1FC6)(<= chr-code-point #x1FCC))
       (and (>= chr-code-point  #x1FD0)(<= chr-code-point #x1FD3))
       (and (>= chr-code-point  #x1FD6)(<= chr-code-point #x1FDB))
       (and (>= chr-code-point  #x1FE0)(<= chr-code-point #x1FEC))
       (and (>= chr-code-point  #x1FF2)(<= chr-code-point #x1FF4))
       (and (>= chr-code-point  #x1FF6)(<= chr-code-point #x1FFC))
       (and (>= chr-code-point  #x210A)(<= chr-code-point #x2113))
       (and (>= chr-code-point  #x2119)(<= chr-code-point #x211D))
       (and (>= chr-code-point  #x212A)(<= chr-code-point #x212D))
       (and (>= chr-code-point  #x212F)(<= chr-code-point #x2131))
       (and (>= chr-code-point  #x2133)(<= chr-code-point #x2139))
       (and (>= chr-code-point  #x213D)(<= chr-code-point #x213F))
       (and (>= chr-code-point  #x2145)(<= chr-code-point #x2149))
       (and (>= chr-code-point  #x2160)(<= chr-code-point #x2183))
       (and (>= chr-code-point  #x2336)(<= chr-code-point #x237A))
       (and (>= chr-code-point  #x249C)(<= chr-code-point #x24E9))
       (and (>= chr-code-point  #x3005)(<= chr-code-point #x3007))
       (and (>= chr-code-point  #x3021)(<= chr-code-point #x3029))
       (and (>= chr-code-point  #x3031)(<= chr-code-point #x3035))
       (and (>= chr-code-point  #x3038)(<= chr-code-point #x303C))
       (and (>= chr-code-point  #x3041)(<= chr-code-point #x3096))
       (and (>= chr-code-point  #x309D)(<= chr-code-point #x309F))
       (and (>= chr-code-point  #x30A1)(<= chr-code-point #x30FA))
       (and (>= chr-code-point  #x30FC)(<= chr-code-point #x30FF))
       (and (>= chr-code-point  #x3105)(<= chr-code-point #x312C))
       (and (>= chr-code-point  #x3131)(<= chr-code-point #x318E))
       (and (>= chr-code-point  #x3190)(<= chr-code-point #x31B7))
       (and (>= chr-code-point  #x31F0)(<= chr-code-point #x321C))
       (and (>= chr-code-point  #x3220)(<= chr-code-point #x3243))
       (and (>= chr-code-point  #x3260)(<= chr-code-point #x327B))
       (and (>= chr-code-point  #x327F)(<= chr-code-point #x32B0))
       (and (>= chr-code-point  #x32C0)(<= chr-code-point #x32CB))
       (and (>= chr-code-point  #x32D0)(<= chr-code-point #x32FE))
       (and (>= chr-code-point  #x3300)(<= chr-code-point #x3376))
       (and (>= chr-code-point  #x337B)(<= chr-code-point #x33DD))
       (and (>= chr-code-point  #x33E0)(<= chr-code-point #x33FE))
       (and (>= chr-code-point  #x3400)(<= chr-code-point #x4DB5))
       (and (>= chr-code-point  #x4E00)(<= chr-code-point #x9FA5))
       (and (>= chr-code-point  #xA000)(<= chr-code-point #xA48C))
       (and (>= chr-code-point  #xAC00)(<= chr-code-point #xD7A3))
       (and (>= chr-code-point  #xD800)(<= chr-code-point #xFA2D))
       (and (>= chr-code-point  #xFA30)(<= chr-code-point #xFA6A))
       (and (>= chr-code-point  #xFB00)(<= chr-code-point #xFB06))
       (and (>= chr-code-point  #xFB13)(<= chr-code-point #xFB17))
       (and (>= chr-code-point  #xFF21)(<= chr-code-point #xFF3A))
       (and (>= chr-code-point  #xFF41)(<= chr-code-point #xFF5A))
       (and (>= chr-code-point  #xFF66)(<= chr-code-point #xFFBE))
       (and (>= chr-code-point  #xFFC2)(<= chr-code-point #xFFC7))
       (and (>= chr-code-point  #xFFCA)(<= chr-code-point #xFFCF))
       (and (>= chr-code-point  #xFFD2)(<= chr-code-point #xFFD7))
       (and (>= chr-code-point  #xFFDA)(<= chr-code-point #xFFDC))
       (and (>= chr-code-point  #x10300)(<= chr-code-point #x1031E))
       (and (>= chr-code-point  #x10320)(<= chr-code-point #x10323))
       (and (>= chr-code-point  #x10330)(<= chr-code-point #x1034A))
       (and (>= chr-code-point  #x10400)(<= chr-code-point #x10425))
       (and (>= chr-code-point  #x10428)(<= chr-code-point #x1044D))
       (and (>= chr-code-point  #x1D000)(<= chr-code-point #x1D0F5))
       (and (>= chr-code-point  #x1D100)(<= chr-code-point #x1D126))
       (and (>= chr-code-point  #x1D12A)(<= chr-code-point #x1D166))
       (and (>= chr-code-point  #x1D16A)(<= chr-code-point #x1D172))
       (and (>= chr-code-point  #x1D183)(<= chr-code-point #x1D184))
       (and (>= chr-code-point  #x1D18C)(<= chr-code-point #x1D1A9))
       (and (>= chr-code-point  #x1D1AE)(<= chr-code-point #x1D1DD))
       (and (>= chr-code-point  #x1D400)(<= chr-code-point #x1D454))
       (and (>= chr-code-point  #x1D456)(<= chr-code-point #x1D49C))
       (and (>= chr-code-point  #x1D49E)(<= chr-code-point #x1D49F))
       (and (>= chr-code-point  #x1D4A5)(<= chr-code-point #x1D4A6))
       (and (>= chr-code-point  #x1D4A9)(<= chr-code-point #x1D4AC))
       (and (>= chr-code-point  #x1D4AE)(<= chr-code-point #x1D4B9))
       (and (>= chr-code-point  #x1D4BD)(<= chr-code-point #x1D4C0))
       (and (>= chr-code-point  #x1D4C2)(<= chr-code-point #x1D4C3))
       (and (>= chr-code-point  #x1D4C5)(<= chr-code-point #x1D505))
       (and (>= chr-code-point  #x1D507)(<= chr-code-point #x1D50A))
       (and (>= chr-code-point  #x1D50D)(<= chr-code-point #x1D514))
       (and (>= chr-code-point  #x1D516)(<= chr-code-point #x1D51C))
       (and (>= chr-code-point  #x1D51E)(<= chr-code-point #x1D539))
       (and (>= chr-code-point  #x1D53B)(<= chr-code-point #x1D53E))
       (and (>= chr-code-point  #x1D540)(<= chr-code-point #x1D544))
       (and (>= chr-code-point  #x1D54A)(<= chr-code-point #x1D550))
       (and (>= chr-code-point  #x1D552)(<= chr-code-point #x1D6A3))
       (and (>= chr-code-point  #x1D6A8)(<= chr-code-point #x1D7C9))
       (and (>= chr-code-point  #x20000)(<= chr-code-point #x2A6D6))
       (and (>= chr-code-point  #x2F800)(<= chr-code-point #x2FA1D))
       (and (>= chr-code-point  #xF0000)(<= chr-code-point #xFFFFD))
       (and (>= chr-code-point  #x100000)(<= chr-code-point #x10FFFD))
       (member chr-code-point  '(#x00AA  #x00B5 #x00BA #x038C #x02EE #x037A #x0386 #x0589 #x0903 #x0950 #x09B2 #x09D7 #x0A5E #x0A83 #x0A8D #x0AC9 #x0AD0 #x0AE0 #x0B40 #x0B57 #x0B83 #x0B9C #x0BD7 #x0CBE #x0CDE #x0D57 #x0DBD #x0E84 #x0E8A #x0E8D #x0EA5 #x0EA7 #x0EBD #x0EC6 #x0F36 #x0F38 #x0F7F #x0F85 #x0FCF #x102C #x1031 #x1038 #x10FB #x1248 #x1258 #x1288 #x12B0 #x12C0 #x1310 #x17DC #x1F59 #x1F5B #x1F5D #x1FBE #x200E #x2071 #x207F #x2102 #x2107 #x2115 #x2124 #x2126 #x2128 #x2395 #x1D4A2 #x1D4BB #x1D546)))
      t nil)))
