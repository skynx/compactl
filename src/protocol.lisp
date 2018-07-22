;;; protocol.lisp

(in-package #:compactl)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; plain protocol

(defgeneric access (B i)
  (:documentation
   "
Given a bitvector B[1,n], returns the bit B[i], for any 1 ≤ i ≤ n.

;  ∀ v ∈ B. (= v (access B (select B i v)))
"))

(defgeneric rank (B i &optional v)
  (:documentation
   " Returns the number of occurences of bit v ∈ {0,1} in bitvector
B[1,n], for any 1 ≤ i ≤ n; in particular, 

; (= 0 (rank v B 0)).

If omitted, assume (= v 1).

;  ∀ v ∈ B. (= i (rank B (select B (1+ i) v)))
"))

(defgeneric select (B j &optional v)
  (:documentation
   "Returns the position in B of the j-th occurrence of bit v ∈ {0,1},
for any j ≥ 0; assume

; (= 0 (select v B 0)) and
; (= (1+ n) (select v B j)) if (> j (rank v B n)).

If omitted, assume (= v 1)."))


;;; pred, succ; sum, search, read



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dynamic protocol extras

(defgeneric insert (B i v)
  (:documentation
   "Inserts the value v between B[i-1] and B[i], 1 ≤ i ≤ n+1."))

(defgeneric deletec (B i)
  (:documentation
   "Deletes B[i], 1 ≤ i ≤ n."))
