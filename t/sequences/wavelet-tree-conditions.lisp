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
