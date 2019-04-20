;;; huffman.lisp

(in-package :compactl.huffman)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Essential Huffman tree classes

(defclass huffman-tree ()
  ((tree-root :initarg :root
	      :accessor ht-root)
   (alphabet :initarg :alphabet
	     :accessor alphabet)
   (estimate/symbol->probability
    :initarg :estim-sym-probs
    :accessor estimated-symbol-probabilities)))

(defclass binary-tree-node ()
  ((left :initarg :left
	 :accessor left)
   (right :initarg :right
	  :accessor right)))

(defclass huffman-tree-node (binary-tree-node)
  ((weight :initarg :weight
	   :accessor ht-weight)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary methods/classes

(defclass ht-symbol-probability-lookup ()
  ((symbol->probability
    :initarg :sym->prob
    :accessor sym->prob
    :documentation
    "A place to store the data associated with a symbol probability distribution.")))

(defclass ht-hashtable-symbol-probability-lookup (ht-symbol-probability-lookup)
  ((symbol->probability :type 'hash-table
			:initform (make-hash-table))))

(defgeneric symbol->probability (sym ht-symprob-lookup)
  (:documentation "Provides a means to acquire the probability of a symbol"))

(defmethod symbol->probability (sym (htspl ht-hashtable-symbol-probability-lookup))
  (gethash sym (sym->prob htspl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Static Huffman algorithm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; defining weight

(defun %ht-merge (ht &key left right)
  (make-instance 'huffman-tree-node
		 :left left
		 :right right
		 :weight (+ (ht-node-weight ht left)
			    (ht-node-weight ht right))))

(defgeneric ht-node-weight (ht node)
  (:documentation "Returns the weight of a Huffman-tree node."))

(defmethod ht-node-weight ((ht huffman-tree) (node character))
  (symbol->probability node (estimated-symbol-probabilities ht)))

(defmethod ht-node-weight ((ht huffman-tree) (node huffman-tree-node))
  (ht-weight node))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constructing the tree

(defun construct-huffman-tree (alphabet estimated-symbol-probabilities)
  ""
  (let ((pq (make-pqueue #'< :key-type 'number :value-type 't))
	;; ^ a priority queue for maintainging the node weights

	;; the tree itself (with no root yet!):
	(ht (make-instance 'huffman-tree
			   :alphabet alphabet
			   :estim-sym-probs estimated-symbol-probabilities))
	(h (sym->prob estimated-symbol-probabilities)))
    ;; initialize the priority queue with probability estimates for
    ;; symbols (characters) in the alphabet
    (loop
       for k being the hash-key in h
       and v being the hash-value in h
       do (pqueue-push k v pq))
    ;; then progressively merge subtrees according to weight priorities
    (loop until (<= (pqueue-length pq) 1)
       ;; only one thing is pushed, but two things are popped, so we
       ;; are assured this will eventually stop
       do (let* ((t2 (pqueue-pop pq))
		 (t1 (pqueue-pop pq))
		 (nu (%ht-merge ht :left t1 :right t2)))
	    (pqueue-push nu (ht-weight nu) pq)))
    ;; set the final element on the pqueue as the tree root
    (setf (ht-root ht) (pqueue-pop pq))
    ;; and finally provide the Huffman tree object as a return value
    ht))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; little helpy boys

(defun frequentist-estimator (string)
  (let ((h (make-hash-table)))
    (loop for x across string
       do (if (gethash x h)
	      (incf (gethash x h))
	      (setf (gethash x h) 1)))
    (make-instance 'ht-hashtable-symbol-probability-lookup :sym->prob h)))


#|

(let ((mu "abracadabra"))
  (construct-huffman-tree
   (remove-duplicates (coerce mu 'list))
   (frequentist-estimator mu)))

; could return several valid HTs, such as this:


			   X
			  / \
		         /   X
			a   / \
			   X   \
			  / \   r
			 X   \
			/ \   b
		       /   \
                      c     d

|#
