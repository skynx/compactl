;;; bitvectors/compressed.lisp

(in-package #:compactl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPRESSION
;; coding operations

#|
Reference:

Navarro, Gonzalo. Chapter 4, section 4.1, "Access". Compact Data
Structures. CUP (2016).

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; binomial coefficients

(defparameter *binomial-coefficients*
  (make-hash-table :test #'equal))

(defun binomial-coefficient (n k)
  (if (or (zerop k) (= n k))
      1
      (+ (binomial-coefficient (1- n) (1- k))
	 (binomial-coefficient (1- n) k))))

(defun memoize (fn cache)
  (lambda (arg)
    (let ((value-place (gethash arg cache)))
      (if value-place
	  value-place
	  (setf (gethash arg cache) (apply fn arg))))))

(defparameter %mbc (memoize #'binomial-coefficient *binomial-coefficients*))

(defun %precompute-bc (block-size)
  (loop for n from 0 to block-size
     while n do
       (loop for k from 0 to n
	  while k do
	    (funcall %mbc (list n k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; compressed bitvector classes

(defclass zero-order-compressed-bitvector (bitvectorc)
  (;; n
   (length-uncompressed
    :documentation "referred to as n in other slot docstrings")
   ;; b
   (bit-block-size
    :documentation "referred to as b in other slot docstrings")
   ;; C, with k = ⌈n/b⌉ cells
   (bit-block-classes
    :documentation "a compact array of FIXED element width of length k = ⌈n/b⌉"
    )	     ;; a compact array of fixed element width
					;
   ;; O, also with k cells
   (bit-block-class-offsets
    :documentation "a compact array of VARIABLE element width of length k = ⌈n/b⌉"
    ) ;; a compact array of variable element width
					;
   ;; P, with ⌈n/bk⌉ pointers to the starting positions of 1 out of
   ;; each k cells in O
   (POINTERS-INTO-OFFSETS
    :documentation "THIS IS NOT ACTUALLY A SLOT IN THE CLASS: IT IS PART OF VARIABLE WIDTH COMPACT ARRAY"
    ) ;; related to variable element width
					;
   ;; L, with b cells
   (lookup/bit-block-class->offset)))

(defclass high-order-compressed-bitvector (bitvectorc)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; block coding

(defgeneric encode-block (B block-size)
  (:documentation
   "
Input:    An integer, seen as the compressed-bitvector B[1,n].
Output:   The pair (c,o) that encodes B.
"))

(defgeneric decode-block (class offset block-size)
  (:documentation "
Input:    A pair (c,o) of block class and offset, and block-size b.
Output:   Bit array B[1,b] (an integer) corresponding to (c,o).
"))

;;;;;;;;;;;;;;; (block coding)
;;; methods

;; more direct bitread method

;; (defmethod bitread ((B integer) (j integer))
;;   (ldb (byte 1 j) B))

;; encode and decode

(defmethod encode-block ((B integer) (block-size integer))
  (loop
     for bit-class = (loop for j from 0 to (1- block-size) sum (ldb (byte 1 j) B) ;(bitread B j)
			  )
     for j from 1 to b
     for bits1-read = bit-class
     while (< 0 bits1-read (1+ (- b j)))
     when (= 1 (bitread B j))
     sum (funcall %mbc (list (- b j) bit-class)) into bit-class-offset
     do (setf bits1-read (1- bits1-read))
     finally (return (values bit-class bit-class-offset))))
#+ignore
(defmethod decode-block ((class integer) (offset integer) (block-size integer))
  )


