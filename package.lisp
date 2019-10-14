;;;; package.lisp

(defpackage #:compactl
  (:use #:cl)
  (:documentation "Lisp-native compact data structures")
  (:export
   ;; essential protocol functions
   #:access
   #:rank
   #:select
   ;; classes
   #:wavelet-tree
   ))

(defpackage #:compactl.huffman
  (:use #:cl #:pqueue))

(defpackage #:compactl.test
  (:use #:cl #:compactl #:compactl.huffman)
  (:documentation "Testing namespace for COMPACTL"))

(defpackage #:compactl.ex
  (:use #:cl #:compactl #:compactl.huffman)
  (:documentation "Examples of using COMPACTL"))
