;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defsystem cl-phonetic
  :name "cl-phonetic"
  :version "0.0.0"
  :author "Brandon Guttersohn"
  :licence "MIT"
  :description "Process phoneme sequences in Common Lisp."
  :serial t
  :components ((:file "defpackage")
               (:file "util")
               (:file "phonetic"))
  :depends-on (#:cl-ppcre
               #:cl-utilities
               #:cl-arrows
               #:alexandria))
