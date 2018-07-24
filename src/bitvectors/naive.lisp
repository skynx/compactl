;;; bitvectors/naive.lisp
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

;;;;;;; access

(defmethod access ((B bitvector-naive) (i integer))
  (bitread B i))

(defmethod access ((B bit-vector) (i integer))
  (aref B i))

;;; to treat a single integer as a small bitvector
#+ignore
(defmethod access ((B integer) (i integer))
  (if (logbitp i B) 1 0))

;;;;;;; rank

;;; naive compact bitvector
(defmethod rank ((B bitvector-naive) (index integer) &optional (value 1))
  (case value
    (1
     (loop for i upfrom 0 as x across (vchunks B)
	while (<= i (floor (/ index +w+)))
	;; index lies past the current word
	when (<= (* (1+ i) +w+) index)
	sum (logcount x) into rank
	;; index is a multiple of +w+
	when (= (* i +w+) index)
	sum (bitread B index) into rank
	;; index lies in the current word
	when (< (* i +w+) index (* (1+ i) +w+))
	sum (logcount (mask-field (byte (1+ (mod index +w+)) 0) x)) into rank
	finally (return rank)))
    (0 (- (min (1+ index) (lengthc B)) (rank B index)))))

;;; simpler assuming built-in bit-vector
(defmethod rank ((B bit-vector) (index integer) &optional (value 1))
  (case value
    (1 (loop for i upfrom 0 as x across B
	  while (<= i index) count (= 1 x) into rank finally (return rank)))
    (0 (- (min (1+ index) (length B)) (rank B index)))))

;;; very easy with an integer, but length is not knowable
#+ignore
(defmethod rank ((B integer) (index integer) &optional (value 1))
  (case value
    (1 (logcount (mask-field (byte (1+ index) 0) B)))
    (0 (- (1+ index) (rank B index)))))

;;;;;;; select

(defun %select-binary-search (B k value ubound)
  
  (cond
    ((> k (rank B ubound value)) ;; there are fewer than k such bits
					;
     ;; indicate so with an impossibly large index
     ubound)
    ((zerop k) -1)
    (t ;; k is positive and less than ubound
					;
     (loop ;; binary search for the correct index
					;
	;; set or reset the search interval based on difference
	;; between rank and j
	for btm = 0 then (or (and (> k split-rank)
				  (1+ split))
			     btm)
	for top = ubound then (or (and (<= k split-rank)
				       split)
				  top)
	
	for split = (+ btm (floor (/ (- top btm) 2)))
	for split-rank = (rank B split value)
	;; do (format t "~s ~s ~s: ~s, ~s~%" btm split top k split-rank)
	until (= btm split top)
	finally (return split)))))

(defmethod select ((B bitvector-naive) (j integer) &optional (value 1))
  (%select-binary-search B j value (1+ (lengthc B))))

(defmethod select ((B bit-vector) (j integer) &optional (value 1))
  (%select-binary-search B j value (length B)))

;;; TODO: how to define a select method on integer?
