;;; wavelet-tree.lisp

(in-package #:compactl)

;;; compact sequences SEQC
(defclass seqc ()
  ((length :initarg :length :reader lengthc)
   (alphabet :reader alphabet
    :documentation "An ordered symbol set (list)")
   (alphabet->index :initform (make-hash-table)
		    :accessor alphabet->index)))

;;; not all sequences will necessarily represent their alphabet lookup
;;; this way
(defgeneric alpha->index (seqc alpha))

(defmethod alpha->index ((seqc seqc) alpha)
  (gethash alpha (alphabet->index seqc)))

;;; WT

(defclass wavelet-tree (seqc)
  ((tree-root :accessor wt-root)
   ;; Just in case the index is shifted, i.e. for coding
   (alphabet-lo :accessor lo
		:documentation "Lowest index in alphabet")
   (alphabet-hi :accessor hi
		:documentation "Highest index in alphabet")))

(defmethod initialize-instance :after
    ((S wavelet-tree)
     &key
       from-seq
       (sort-key #'identity sort-key-supplied-p)
       (alphabet (and from-seq
		      (sort (coerce (remove-duplicates from-seq)
				    'list)
			    #'<
			    :key sort-key))
		 alphabet-supplied-p)
       ;; lo hi
       (length (length from-seq)))
  ;; check inputs
  (when (null from-seq)
    (error "No input sequence was supplied to initialize ~s." S))
  (when (and alphabet-supplied-p (null sort-key-supplied-p))
    (warn "No :sort-key initarg was supplied, but an alphabet was: ~
    This may have unexpected consequences."))
  (when (and from-seq alphabet-supplied-p)
    (warn "Both :from-seq and :alphabet supplied as initargs: This may ~
    have unexpected consequences."))
  ;; make/set slot values
  (let* ((alphabet->index (alphabet->index S))
	 (hi (loop for i upfrom 0 as x in alphabet
		do (setf (gethash x alphabet->index) i)
		finally (return (1- i))))
	 (index-seq (mapcar (lambda (x) (gethash x alphabet->index)) from-seq)))
    (setf
     (slot-value S 'alphabet) alphabet
     (slot-value S 'length) length
     ;; index bounds
     (lo S) 0
     (hi S) hi
     ;; tree root
     (wt-root S)
     (construct-wavelet-tree index-seq length
			     #+compactl-debug t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WT Construction

;;; length is an important parameter!
(defun construct-wavelet-tree
    (sequence length &optional #+compactl-debug (show-progress nil)
     &key (bitvector-class 'bitvector-naive))
  "
Given a SEQUENCE (with a supplied LENGTH) of integer codes for symbols
in some alphabet, construct the (binary) wavelet tree with structure
corresponding to the code tree for this coding of the alphabet.

When the integer coding is simply the numerical index of a symbol in
alphabet order, the tree is balanced.  When the symbols are encoded
with a particular coding tree (e.g. Huffman), the resulting wavelet
tree is isomorphic to that coding tree.
"
  #+compactl-debug
  (cerror "Make wavelet tree from ~a." "Entering WT node." sequence)
  (if (= length 1) nil 			;)
      (let ((B (make-instance bitvector-class :length length)))
	(multiple-value-bind
	      (z
	       seq-left s-min-left s-max-left
	       seq-right s-min-right s-max-right)
	    (loop
	       ;; sequence is listp: length is here as a stop for
	       ;; circular inputs
	       for i upfrom 0 to (1- length) as s in (the list sequence)
	       ;; lo side
	       if (not (logbitp 0 s))
	       count i into z
	       and do (bitclear B i)
	       and collect (ash s -1) into seq-left
	       and minimize s into s-min-left
	       and maximize s into s-max-left
		 #+compactl-debug
	       and
		 #+compactl-debug
	       do
		 #+compactl-debug
		 (cerror "put ~s into v.B at ~s and collect upper bits ~
		 of ~b under sequence~%~1,4@T~a~%into LEFT ~
		 subsequence~%~1,4@T~a"
			 "Got a ~s at position ~s from ~b in ~
			 sequence~%~1,4@T~s" 0 i s sequence seq-left)
	       ;; hi side
	       if (logbitp 0 s)
	       do (bitset B i)
	       and collect (ash s -1) into seq-right
	       and minimize s into s-min-right
	       and maximize s into s-max-right
		 #+compactl-debug
	       and
		 #+compactl-debug
	       do
		 #+compactl-debug
		 (cerror "put ~s into v.B at ~s and collect upper bits ~
		 of ~b under sequence~%~1,4@T~a~%into RIGHT ~
		 subsequence~%~1,4@T~a"
			 "Got a ~s at position ~s from ~b in ~
			 sequence~%~1,4@T~s" 1 i s sequence seq-right)
	       ;; number of lo-side bits and tree node L/R children
	       finally
		 (return (values z
				 seq-left s-min-left s-max-left
				 seq-right s-min-right s-max-right)))
	  ;; exclusively for snooping around
	  #+compactl-debug
	  (when show-progress
	    (format t "Z: ~s~%L: ~s~%R: ~s~%B: ~{~b~}~%~%"
	  	    z seq-left seq-right (coerce (vchunks B) 'list)))
	  (list
	   B ;; the real thing
	   (unless (= s-min-left s-max-left)
	     (construct-wavelet-tree seq-left z
				     #+compactl-debug show-progress
				     :bitvector-class bitvector-class))
	   (unless (= s-min-right s-max-right)
	     (construct-wavelet-tree seq-right (- length z)
				     #+compactl-debug show-progress
				     :bitvector-class bitvector-class)))))))



;;;;;;;;;;;;;;; (WT)
;;; query protocol

;; these three functions are closely related to
;; #'construct-wavelet-tree



(defun %wt-access (beta depth node index)
  (if (consp node)
      ;; internal node
      ;; ACCUMULATE THE LEAF SYMBOL BY NAVIGATING/BITSETTING
      (case (access (car node) index)
	;; lo bit ==> left node
	(0
	 #+compactl-debug
	 (cerror "set β[~s] ← 0, go LEFT for (v.l).B[rank_0(v.B,~s) - ~
	 1] = (v.l).B[~s]"
		 "LEFT branch at depth ~s, index ~s; next index ~a"
		 depth index (1- (rank (car node) index 0)))
	 (%wt-access beta (1+ depth) (cadr node)
		     (1- (rank (car node) index 0))))
	;; hi bit ==> right node
	(1
	 #+compactl-debug
	 (cerror "set β[~s] ← 1, go RIGHT for (v.r).B[rank_1(v.B,~s) - ~
	 1] = (v.r).B[~s]"
		 "RIGHT branch at depth ~s, index ~s; next index ~a"
		 depth index (1- (rank (car node) index 1)))
	 (%wt-access (logior (byte 1 depth) beta) (1+ depth) (caddr node)
		     (1- (rank (car node) index 1)))))
      ;; leaf node
      (progn
	#+compactl-debug
	(cerror "return β=~s" "LEAF at ~b, index ~s, node ~s" beta index node)
	beta)))


(defun %wt-rank (alpha node index)
  "number of occurences up to position INDEX of symbol ALPHA in
sequence encoded under NODE"
  (if (consp node)
      (case (logand 1 alpha)
	  (0
	    #+compactl-debug
	    (cerror "set (α'.0 = ~b = α) ← α', ~2,6@Tgo LEFT for ~
	    RANK_α'(v.l, rank_0(v.B,~a) - 1) = RANK_α'(v.l, ~a) "
		    "At α = ~a.~%Going to find rank_0(B,~a)"
		    alpha index (1- (rank (car node) index 0)))
	    (%wt-rank (ash alpha -1) (cadr node)
		      (1- (rank (car node) index 0))))
	  (1
	    #+compactl-debug
	    (cerror "set (α'.1 = ~b = α) ← α', ~2,6@Tgo RIGHT for ~
	    RANK_α'(v.r, rank_1(v.B,~a) - 1) = RANK_α'(v.r, ~a)"
		    "At α = ~a.~%Going to find rank_1(B,~a)"
		    alpha index (1- (rank (car node) index)))
	    (%wt-rank (ash alpha -1) (caddr node)
		      (1- (rank (car node) index)))))
      ;; leaf
      (1+ index)))


(defun %wt-select (alpha node count)
  "position in node of j-th occurence (j=count>0) of symbol with code alpha"
  ;; NB increments and decrements here manage the discrepancy in
  ;; meaning between index and count
  (if (consp node)
      ;; internal
      (if (evenp alpha)
	  (progn
	    #+compactl-debug
	    (cerror "set (α'.0 = ~b = α) ← α', ~2,6@Tgo LEFT  for select_0(v.B, SELECT_α(v.l,~a))"
		    "At α = ~a.~%Going to find rank_0(B,~a)" alpha count)
	    (select (car node) (1+ (%wt-select (ash alpha -1) (cadr node) count)) 0))
	  (progn
	    #+compactl-debug
	    (cerror "set (α'.1 = ~b = α) ← α', ~2,6@Tgo RIGHT for select_1(v.B, SELECT_α(v.r,~a))"
		    "At α = ~a.~%Going to find rank_1(B,~a)" alpha count)
	    (select (car node) (1+ (%wt-select (ash alpha -1) (caddr node) count)) 1)))
      ;; leaf ==> lo = hi
      (1- count)))




(defmethod access ((sequence wavelet-tree) (index integer))
  (nth (%wt-access 0 0 (wt-root sequence) index) (alphabet sequence)))

(defmethod rank ((sequence wavelet-tree) (index integer) &optional (alpha (lo sequence)))
  (%wt-rank (alpha->index sequence alpha) (wt-root sequence) index))

(defmethod select ((sequence wavelet-tree) (count integer) &optional (alpha (nth 0 (alphabet sequence))))
  #+compactl-debug
  (format t "Finding position of occurence ~s of symbol ~s (~s).
;(B,j,a,b):~%" index alpha (alpha->index sequence alpha))
  (%wt-select (alpha->index sequence alpha) (wt-root sequence) count))



(defun %stupefy (node)
  (if (consp node)
      (list (format nil "~B" (vchunks (car node)))
	    (%stupefy (cadr node))
	    (%stupefy (caddr node)))
      (format nil "X")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; online WT

;; (defclass online-wavelet-tree (wavelet-tree)
;;   ((code-table)))


;; ;;; pretending to use built-in bit-vector
;; (let* ((node (list ))
;;        (alpha #b01)
;;        (B (car node))
;;        (r (rank B (length B) (logand 1 alpha)))
;;        (new-bv (make-array r
;; 			   :element-type 'bit
;; 			   :adjustable t
;; 			   :fill-pointer r)))
;;   (case (logand 2 alpha)
;;     (0
;;      (vector-push-extend 0 new-bv)
;;      (rplaca (cdr node) (list new-bv nil nil)))
;;     (2
;;      (vector-push-extend 1 new-bv)
;;      (rplaca (cddr node) (list new-bv nil nil)))))


(defun %%construct-online-wavelet-tree-node (alpha &optional left-pad root-p)
  (if (zerop alpha)
      (if root-p
	  ;; a zero at the root must still initialize the tree
	  (list (make-array 2 :element-type 'bit :adjustable t :fill-pointer 1)
		nil nil)
	  ;; otherwise, a zero indicates a leaf ==> NIL
	  nil)
      (let ((B (make-array (+ 2 (or left-pad 0))
			   :element-type 'bit
			   :adjustable t
			   :fill-pointer (or left-pad 0))))
	#+compactl-debug
	(cerror "new tree node" "Making a new wavelet-tree node")
	(vector-push-extend (logand 1 alpha) B)
	(list
	 B ;; an adjustable built-in bit-vector
	 (unless (logbitp 1 alpha)
	   (%%construct-online-wavelet-tree-node (ash alpha -1)))
	 (unless (not (logbitp 1 alpha))
	   (%%construct-online-wavelet-tree-node (ash alpha -1)))))))


(defun %%update-online-wavelet-tree (node alpha)
  "Destructively modifies the wavelet tree under NODE to represent the
sequence S.α, where S is the sequence represented by NODE before
calling this function."
  (if (consp node)
      (case (logand 1 alpha) ;; next bit of alpha
	(0
	 #+compactl-debug
	 (cerror "push ~s onto v.B and go LEFT"
		 "stepping..." (logand 1 alpha))
	 (vector-push-extend (logand 1 alpha) (car node))
	 (if (cadr node)
	     (%%update-online-wavelet-tree (cadr node) (ash alpha -1))
	     (rplaca (cdr node)		; lo ==> left ==> CDR
		     (%%construct-online-wavelet-tree-node
		      (ash alpha -1)
		      (1- (rank (car node) (length (car node)) (logand 1 alpha))))))
	 ;; to reduce update calls over a stream, return node
	 node)
	(1
	 #+compactl-debug
	 (cerror "push ~s onto v.B and go RIGHT"
		 "stepping..." (logand 1 alpha))
	 (vector-push-extend (logand 1 alpha) (car node))
	 (if (caddr node)
	     (%%update-online-wavelet-tree (caddr node) (ash alpha -1))
	     (rplaca (cddr node) 	; hi ==> right ==> CDDR
		     (%%construct-online-wavelet-tree-node
		      (ash alpha -1)
		      (1- (rank (car node) (length (car node)) (logand 1 alpha))))))
	 ;; to reduce updates...
	 node))
	;; only called when input node at depth zero is nil
      (%%construct-online-wavelet-tree-node alpha 0 t)))
