;;; protocol.lisp

(in-package #:compactl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditions

(define-condition protocol-condition (condition)
  ((operation :initarg :operation
	      :reader protocol-operation)
   (object :initarg :object
	   :reader object-for))
  (:report (lambda (condition stream)
	     (format stream "Attempting operation ~a on object ~a~%"
		     (protocol-operation condition)
		     (object-for condition)))))

(define-condition protocol-error (protocol-condition error)
  ())

(define-condition protocol-range-error (protocol-error)
  ((index :initarg :index
	  :reader range-error-index)
   (upper :initarg :upper
	  :reader range-error-upper-bound))
  (:report (lambda (condition stream)
	     (format stream
		     "Index i = ~a is out of bounds 0 ≤ i ≤ ~a for object ~a;~%
Could not perform operation ~a"
		     (range-error-index condition)
		     (range-error-upper-bound condition)
		     (object-for condition)
		     (protocol-operation condition)))))

(define-condition protocol-error-dynamic (protocol-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plain protocol

(defgeneric access (B i)
  (:documentation
   "
Given a bitvector B[0,n], returns the bit B[i], for any 0 ≤ i ≤ n.
"))


(defgeneric rank (B i &optional v)
  (:documentation
   " Returns the number of occurences of bit v ∈ {0,1} in bitvector
B[0,n] from position 0 to position i in, for any 0 ≤ i ≤ n; in
particular,

; (= 0 (rank v B -1)).

If omitted, assume (= v 1).

RANK inverts SELECT in its second argument, i:

;  ∀ v ∈ B. (= i (rank B (select B i v) v))
"))


(defgeneric select (B k &optional v)
  (:documentation
   " Returns the position in B[0,n] of the k-th occurrence of the bit
v∈{0,1}, for any k ≥ 0; that is, (select B k v) returns the smallest
index i such that B[i] = v and (rank B i v) is k.

As a consequence,

;  ∀ v ∈ B. (= i (rank B (select B i v) v))

Assume the following.

; (= -1 (select B 0 v)) and
; (= (1+ n) (select v B j)) if (> k (rank v B n)).

If omitted, assume (= v 1).

ACCESS inverts SELECT in its final argument, v:

;  ∀ V ∈ B. (= v (access B (select B i v)))
"))


;;; pred, succ; sum, search, read



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dynamic protocol extras

(defgeneric insert (B i v)
  (:documentation
   "Inserts the value v between B[i-1] and B[i], 1 ≤ i ≤ n+1."))

(defgeneric deletec (B i)
  (:documentation
   "Deletes B[i], 1 ≤ i ≤ n."))
