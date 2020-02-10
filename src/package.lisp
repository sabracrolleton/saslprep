;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :saslprep
  (:use :common-lisp)
  (:export #:saslprep-normalize
           #:char-mapped-to-nothing-p
           #:char-mapped-to-space-p
           #:non-char-code-point-p
           #:non-ascii-space-char-p
           #:ascii-control-char-p
           #:non-ascii-control-char-p
           #:surrogate-code-points-p
           #:private-use-char-p
           #:inappropriate-for-plain-text-p
           #:inappropriate-for-canonical-representation-char-p
           #:tagging-char-p
           #:change-display-property-char-p
           #:char-printable-ascii-p
           #:string-printable-ascii-p
           #:code-point-printable-ascii-p
           #:char-with-bidirectional-property-R-or-AL-p
           #:char-with-bidirectional-property-L-p))
