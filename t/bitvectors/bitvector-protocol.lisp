;;; bitvector-protocol.lisp

;(in-package #:compactl.test)
(in-package #:compactl)



(let* ((len 20000)
       (X1 (make-random-state))
       (X2 (make-random-state))
       (bv0 (make-instance 'bitvector-naive :length len)))

  (loop repeat 500000 do
       (if (= (random 2) 1)
	   (bitset bv0 (random (1- len) X1))
	   (bitclear bv0 (random (1- len) X2)))))



(time
 (let ((rank0 (rank bv0 (lengthc bv0) 0))
       (rank1 (rank bv0 (lengthc bv0) 1))
       (X1 (make-random-state))
       (X2 (make-random-state)))
   (loop repeat 5000 do
	(let* ((k  (random rank1 X1))
	       (p1 (select bv0 k 1)))
	  (when
	      (not (= k (rank bv0 p1 1)))
	    (error "RANK_1 on ~s >>> ~s! position ~s" bv0 k p1))
	  (when
	      (and (> p1 -1)
		   (not (= 1 (access bv0 p1))))
	    (error "ACCESS on ~s expecting 1 >>> ~s! position ~s, value ~s"
		   bv0 k p1 (access bv0 p1))))
	(let* ((k  (random rank0 X2))
	       (p0 (select bv0 k 0)))
	  (when
	      (not (= k (rank bv0 p0 0)))
	    (error "RANK_0 on ~s >>> ~s! position ~s" bv0 k p0))
	  (when
	      (and (> p0 -1)
		   (not (= 0 (access bv0 p0))))
	    (error "ACCESS on ~s expecting 0 >>> ~s! position ~s, value ~s"
		   bv0 k p0 (access bv0 p0)))))
   (format t "~%:)~%Protocol conformance check passed for class BITVECTOR-NAIVE~%~%")))
