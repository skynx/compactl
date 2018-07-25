;;; wavelet-tree.lisp

;(in-package #:compactl.test)
(in-package #:compactl)



;;this should not signal an error condition:

(let* ((hamlet "tobeornottobethatisthequestion")
       (src-seq (coerce hamlet 'list))
       (wt (make-instance 'wavelet-tree
			  :sort-key #'char-code
			  :from-seq src-seq)))
  (format t "~s" src-seq)
  (loop for x in src-seq
     do
       (loop for k from 1 to (rank wt (length src-seq) x)
	  do
	    (let* ((q (select wt k x))
		   (n (nth q src-seq))
		   (a (access wt q))
		   (r (rank wt q x)))
	      (unless (eql x n)
		(error "x != n"))
	      (unless (eql x a)
		(error "x != a"))
	      (unless (eql k r)
		(error "r != k")))
	    )))


(setf dwt0 (list (make-array 2 :element-type 'bit :fill-pointer 0 :adjustable t) nil nil))

(time
 (let ((X0 (make-random-state)))
   (loop repeat 100000 do (%%update-online-wavelet-tree dwt0 (random 50)))))

(time
 (%wt-select 12 dwt0 3000))

(time
 (%wt-access 0 0 dwt0 (%wt-select 5 dwt0 100)))


#|
THIS IS NOT A REAL PROPERTY CHECKING STATEMENT.
IT IS A FANTASY.

(let* ((alphabet (a-random :range :over :fixnum))
       (src-seq (a-random :sequence :over alphabet))
       (wt (make-instance 'wavelet-tree
			  :from-seq src-seq
			  :hi (max alphabet)
			  :lo (min alphabet))))
  (forall with alpha from alphabet
	  and n from 0 to (lengthc wt)
	  and j from 0 to (rank B n alpha)
	  (= alpha (nth (select wt j alpha) src-seq))))

|#
