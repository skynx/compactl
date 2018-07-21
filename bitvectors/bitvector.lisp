;; class (and gf) defs

(in-package #:compactl)

;;; BITVECTOR

(defclass bitvectorc ()
  (;; (contents :initarg :contents
   ;; 	     :accessor contents)
   )
  (:documentation "Just a name."))

;; e.g., a near-synonym for arrayc-fixed-element-size

(defclass bitvector-naive (arrayc-fixed-element-size)
  ((element-size :initform 1 :allocation :class))
  (:documentation "Synonym for ARRAYC-FIXED-ELEMENT-SIZE with fixed size of 1 bit."))

;; (defmethod initialize-instance :after
;;     ((B bitvector-fixed-width) &key length element-size)
;;   (declare (ignore length element-size))
;;   (setf (contents B)
;; 	(slot-value B 'virtual-bit-array)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADT bitvector supports these query operations:

;; access

;; rank

;; select

;;;;;;;;;;;;;;;
;;; implementation for example class bitvector-naive

(defmethod access ((B bitvector-naive) (i integer))
  (readc B i))

;;; to treat a single integer as a small bitvector
(defmethod access ((B integer) (i integer))
  (if (logbitp i B) 1 0))

(defmethod rank ((B bitvector-naive) (index integer) &optional (value 1))
  (case value
    (1
     (loop for i upfrom 0 as x across (vchunks B)
	while (<= i (floor (/ index +w+)))
	;; index lies past the current word
	when (< (* (1+ i) +w+) index)
	sum (logcount x) into rank
	;; index lies in the current word
	when (<= (* i +w+) index  (* (1+ i) +w+))
	sum (logcount (mask-field (byte (mod index +w+) 0) x)) into rank
	finally (return rank)))
    (0 (- index (rank B index)))))

(defmethod rank ((B integer) (index integer) &optional (value 1))
  (case value
    (1 (logcount (mask-field (byte index 0) B)))
    (0 (- index (rank B index)))))

;;; SELECT
(defmethod select ((B bitvector-naive) (j integer) &optional (value 1))
  (let ((len (lengthc B)))
    #+compactl-debug
    (format t "~%B: ~b~%len: ~s | j: ~s | v: ~s

|~1,2@Ti~1,2@T|~1,4@Ttop~1,4@Tspl~1,4@Tbtm~1,8@T|~1,2@Tsr~1,8@T|~%"
	    (vchunks B) len j value)
    (if (> j (rank B len value)) ;; there are fewer than j such bits
					;
	(1+ len) ;; indicate so with an impossibly large value
					;
	(loop ;; binary search for the correct index
	   for i from 0 ; to len
	   ;; set or reset the search interval based on difference
	   ;; between rank and j
	   for btm = 0 then (or (and (> j split-rank)
				     split)
				btm)
	   for top = len then (or (and (<= j split-rank)
				       split)
				  top)
	   for split = (+ btm (ceiling (/ (- top btm) 2)))
	   for split-rank = (rank B split value)
	     #+compactl-debug do
	     #+compactl-debug
	     (format t "|~1,2@T~s~1,2@T|~1,4@T~s~1,4@T~s~3,4@T~s~1,8@T|~1,2@T~s~1,8@T|~%"
		     i top split btm split-rank)
	   until (= btm (1- top))
	   finally (return btm)))))

;;; TODO: how to define a select method on integer?

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
