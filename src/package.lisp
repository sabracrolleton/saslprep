;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; Package: CL-USER; -*-

(defpackage :saslprep
  (:use :common-lisp)
  (:export #:saslprep-normalize
           #:char-mapped-to-nothing-p
           #:char-mapped-to-space-p
           #:string-mapped-to-nothing
           #:string-mapped-to-space
           #:non-char-code-point-p
           #:non-ascii-space-char-p
           #:ascii-control-char-p
           #:non-ascii-control-char-p
           #:surrogate-code-point-p
           #:private-use-char-p
           #:inappropriate-for-plain-text-char-p
           #:inappropriate-for-canonical-representation-char-p
           #:tagging-char-p
           #:change-display-property-char-p
           #:char-printable-ascii-p
           #:string-printable-ascii-p
           #:code-point-printable-ascii-p
           #:char-with-bidirectional-property-l-p
           #:char-with-bidirectional-property-r-or-al-p))
