;;; bitvectors/fast.lisp

(in-package #:compactl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; space tradeoffs for constant time

;; (defclass bitvector-con-select () ()
;;   (:documentation
;;    "bitvector supporting constant-time select operations."))

;; (defclass bitvector-con-rank () ()
;;   (:documentation
;;    "bitvector supporting constant-time rank operations."))

;; (defclass bitvector-con-access () ()
;;   (:documentation
;;    "bitvector supporting constant-time access operations."))

;; (defclass bitvector-fast
;;     (bitvector-con-access bitvector-con-rank bitvector-con-select)
;;   (:documentation
;;    "bitvector supporting constant-time operations for access, rank, ~
;;    and select"))
