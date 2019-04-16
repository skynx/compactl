;;; bitvectors/fast.lisp

(in-package #:compactl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; space tradeoffs for faster execution time

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; class structure

;;; faster RANK

(defclass bitvector-sampled-rank (bitvector-naive)
  ((rank-samples)))

(defclass bitvector-sampled-rank-sparse (bitvector-sampled-rank)
  ((rank-sampling-step :accessor rstep :initarg :rstep)))

(defclass bitvector-sampled-rank-dense (bitvector-sampled-rank-sparse)
  ((rank-samples-relative)))

;;; faster SELECT

(defclass bitvector-sampled-select (bitvector-sampled-rank)
  ((select-samples)))

(defclass bitvector-sampled-select-simple
    (bitvector-sampled-rank-sparse bitvector-sampled-select)
  ((select-sampling-step :accessor sstep :initarg :sstep)))

#+ignore 				; I don't really understand this yet
(defclass bitvector-sampled-select-faster
    (bitvector-sampled-rank-dense bitvector-sampled-select)
  ((V-block-long-or-short)
   (I-long-block-select-samples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; initialization

;;; faster RANK

(defmethod update-instance-for-different-class :after
    ((previous bitvector-naive) (current bitvector-sampled-rank-sparse) &key rstep)
  (let* ((n (lengthc current))
	 (W (vchunks current))
	 (s (* rstep +w+)) ;; how many bits per step?
	 (rlen (floor (/ n s)))
	 (R (make-array rlen :element-type 'integer)))
    (when (zerop rlen) (error "Supplied :rstep of ~s must be at most ~
     the number of words (roughly, fixnums) in this bitvector's ~
     underlying virtual array, which is ~s." rstep (length W)))
    ;; precompute a sampling of rank values, store in R
    (loop for i upfrom 0 as x across W
       sum (logcount x) into rsum
       when (and (> i 0) (zerop (mod i rstep)))
       do (setf (aref R (1- (/ i rstep))) rsum))
    ;; set slot rank-samples to the array of sampled rank values
    (setf (slot-value current 'rank-samples) R)))

(defmethod rank ((B bitvector-sampled-rank-sparse) (i integer) &optional (v bit))
  ;; see p. 74 of Navarro
  )


#|

For a previously define bitvector-naive, you can get the fast-rank
overhead bits in one step:

(change-class * 'bitvector-sampled-rank-dense :rstep 10)

|#

(defmethod update-instance-for-different-class :after
    ((previous bitvector-naive)
     (current bitvector-sampled-rank-dense) &key rstep)
  ;; next precompute densely sampled ranks relative to sparse samples
  (let* (;;(n (lengthc current))
	 (W (vchunks current))
	 (s (* rstep +w+))
	 (R-prime (make-instance 'arrayc-fixed-element-size
				 :length (length W) ;;(ceiling (/ n +w+)) ;; ==> (1- (length W))
				 :element-size (ceiling (log (- s +w+) 2)))))
    ;; 
    (loop for i upfrom 0 as x across W
       sum (logcount x) into rprime-sum
       do (writec R-prime i rprime-sum)
       when (and (> i 0) (zerop (mod i s))) sum (- rprime-sum) into rprime-sum)
    (setf (slot-value current 'rank-samples-relative) R-prime)))

(defmethod rank ((B bitvector-sampled-rank-dense) (i integer) &optional (v bit))
  ;; see p. 75 of Navarro
  )

;;; faster SELECT

(defmethod initialize-instance :after
    ((instance bitvector-sampled-select-simple)))

#+ignore
(defmethod initialize-instance :after
    ((instance bitvector-sampled-select-faster)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; space tradeoffs for constant time

(defclass bitvector-con-select () ()
  (:documentation
   "bitvector supporting constant-time select operations."))

(defclass bitvector-con-rank () ()
  (:documentation
   "bitvector supporting constant-time rank operations."))

(defclass bitvector-con-access () ()
  (:documentation
   "bitvector supporting constant-time access operations."))

(defclass bitvector-fast
    (bitvector-con-access bitvector-con-rank bitvector-con-select)
  (:documentation
   "bitvector supporting constant-time operations for access, rank, ~
   and select"))
