;;; array.lisp

(in-package #:compactl)

;;; ARRAYC

(defclass arrayc ()
  ((arrayc-length :initarg :length
		  :reader lengthc)))

;;; ADT arrayc supports these operations:

(defgeneric readc (A i)
  (:documentation "
For compact arrays.
Returns A[i], for any i s.t. 1 ≤ i ≤ n = (lengthc A).
"))

(defgeneric writec (A i x)
  (:documentation "
For compact arrays.
Sets A[i] ← x, for any i s.t. 1 ≤ i ≤ n = (lengthc A) and any x.
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elements of fixed size

(defparameter +w+ 64 ;; INFER THIS!
  "Number of bits in the system machine word.")

;;; as an array of integers

(defclass arrayc-virtual (arrayc)
  ((virtual-bit-array :type '(array fixnum)
		      :reader vchunks)))

;;; treating each consecutive chunk of a fixed number of bits as a
;;; logical block

(defclass arrayc-fixed-element-size (arrayc-virtual)
  ((element-size :initarg :element-size
		 :reader element-size)))

(defmethod initialize-instance :after
    ((A arrayc-fixed-element-size)
     ;; subclasses may have an element-size slot with :class
     ;; allocation
     &key (element-size (element-size A)))
  (setf (slot-value A 'virtual-bit-array)
	(make-array (ceiling (/ (* element-size (lengthc A))
				     +w+))
		    :element-type (list 'unsigned-byte +w+))))

;;; reading and writing on bit arrays

(defgeneric bitread (B j)
  (:documentation "
Input:

  Bit array B[1,n], seen as the integer array W[1, ⌈n/w⌉], and the
  position j to READ.

Output: Returns B[j].
"))


#| In order for bitset, bitclear, and bitread to be considered
correct, this form should never signal an error condition:

(let ((A (make-instance 'arrayc-fixed-element-size
			:element-size 5
			:length 10)))
  (loop for i from 1 to (lengthc A) while i do
       (bitset A i)
       (when (not (= 1 (bitread A i))) (error "bitset/bitread is broken!"))
       (bitclear A i)
       (when (not (= 0 (bitread A i))) (error "bitclear/bitread is broken!"))))
|#



(defmethod bitread ((B arrayc-virtual) (j integer))
  #+ignore
  (let ((W (slot-value B 'virtual-bit-array))
	;; position within block
	#+ignore
	(r (mod j +w+))
	;; index in the actual array of the block covering position
	(j-- (ceiling (/ j +w+)))))
  (ldb (byte 1 (mod j +w+)) (aref (vchunks B) (floor (/ j +w+))))
  #+ignore
  (mod (floor (/ (aref W (ceiling (/ j +w+)))
		 (expt 2 (- +w+ r))))
       2))

(define-setf-expander bitread (B j &environment env)
  "Set the bit in a compact array to the given value using SETF."
  (get-setf-expansion
   `(ldb (byte 1 (mod ,j +w+))
	 (aref (vchunks ,B) (floor (/ ,j +w+))))
   env))

(defgeneric bitset (B j)
  (:documentation "
Input:

  Bit array B[1,n], seen as the integer array W[1, ⌈n/w⌉], and the
  position j to SET.

Output: Sets B[j] ← 1.
"))

(defmethod bitset ((B arrayc-virtual) (j integer))
  #+ignore
  (let ((W (slot-value B 'virtual-bit-array))
	;; position within block
	(r (mod (1- j) +w+))
	;; index in the actual array of the block covering position
	(j-- (1- (ceiling (/ j +w+))))))
  (setf (bitread B j) (the bit 1))
  #+ignore
  (setf
   (ldb (byte 1 (mod j +w+))
	(aref (vchunks B) (floor (/ j +w+))))
   1)
  #+ignore
  (when (= 0
	   (mod (floor (/ (aref W j--)
			  (expt 2 (- +w+ r))))
		2))
    (setf (aref W (ceiling (/ j +w+)))
	  (+ (aref W (ceiling (/ j +w+)))
	     (expt 2 (- +w+ r))))))

(defgeneric bitclear (B j)
  (:documentation "
Input:

   Bit array B[1,n], seen as the integer array W[1, ⌈n/w⌉], and the
   position j to CLEAR.

Output:  Sets B[j] ← 0.
"))

(defmethod bitclear ((B arrayc-fixed-element-size) (j integer))
  (setf (bitread B j) (the bit 0))
  #+ignore
  (let ((W (slot-value B 'virtual-bit-array))
	;; position within virtual block
	(r (mod (1- j) +w+))
	;; index in the actual array of the block covering position
	(j-- (1- (ceiling (/ j +w+)))))
    (setf (ldb (byte 1 r) (aref W j--)) 0)
    #+ignore
    (when (= 1 (mod (floor (/ (aref W (ceiling (/ j +w+)))
			      (expt 2 (- +w+ r))))
		    2))
      (setf (aref W (ceiling (/ j +w+)))
	    (- (aref W (ceiling (/ j +w+)))
	       (expt 2 (- +w+ r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (fixed size elements)
;;; readc and writec implementation

;; utilities bitsread and bitswrite operate on the integer vector
;; providing the virtual bit array

;;;;;;;;;;;;;;;
;; reading:

(defun bitsread (W j0 j1)
  (let ((j0-- (floor (/ j0 +w+)))
	(j1-- (floor (/ j1 +w+))))
    (cond ((> j0 j1) nil)
	  ;; endpoints in the same virtual block of W
	  ((= j0-- j1--)
	   (ldb (byte (- j1 j0) (mod j0 +w+)) (aref W j1--))
	   #+ignore ;; when you don't know about the byte functions
	   (let ((r (1+ (mod (1- j1) +w+))))
	     (mod (floor (/ (aref W j1--)
			    (expt 2 (- +w+ r))))
		  (expt 2 (1+ (- j1 j0))))))
	  ;; endpoints in separate virtual blocks of W
	  (t
	   ;; assuming you never need to read a range that spans more
	   ;; than two virtual blocks, since arrayc is not
	   ;; advantageous if (> l +w+), l = (element-size A)
	   (let ((r (mod j0 +w+))
		 (s (mod j1 +w+)))
	     (+ (ash (ldb (byte s 0) (aref W j1--)) (- +w+ r))
		(ldb (byte (- +w+ r) r) (aref W j0--))))
	   #+ignore
	   (let ((r (1+ (mod (1- j1) +w+))))
	     (+ (floor (/ (aref W j1--)
			  (expt 2 (- +w+ r))))
		(* (mod (aref W j0--)
			(expt 2 (- +w+ r)))
		   (expt 2 r))))))))

(defmethod readc ((A arrayc-fixed-element-size) (i integer))
  (let ((len (element-size A)))
    (bitsread (vchunks A) (* len i) (* len (1+ i)))))

;;;;;;;;;;;;;;;
;; writing:

(defun bitswrite (W j0 j1 x)
  (let ((j0-- (floor (/ j0 +w+)))
	(j1-- (floor (/ j1 +w+))))
    (when (and (adjustable-array-p W)
	       (> j1-- (1- (length W))))
      (adjust-array W (1+ j1--)))
    (cond ((> j0 j1) (values))
	  ((= j0-- j1--)   ;; endpoints in same block of W
	   (setf (ldb (byte (- j1 j0) (mod j0 +w+)) (aref W j1--)) x)
	   #+ignore
	   (let ((r (1+ (mod j1 +w+))))
	     ;; first clear the range
	     (setf (aref W j1--)
		   (- (aref W j1--)
		      (* (mod (floor (/ (aref W j1--)
					(expt 2 (- +w+ r))))
			      (expt 2 (1+ (- j1 j1))))
			 (expt 2 (- +w+ r)))))
	     ;; then set cleared area to x
	     (setf (aref W j1--) (+ (aref W j1--)
				    (* x (expt 2 (- +w+ r)))))))
	  (t ;; endpoints in adjacent blocks
	   (let ((r (mod j0 +w+)))
	     (setf
	      ;; lower block
	      (ldb (byte (- +w+ r) r) (aref W j0--))
	      (ldb (byte (- +w+ r) 0) x)
	      ;; upper block
	      (ldb (byte (mod j1 +w+) 0) (aref W j1--))
	      (ldb (byte (mod j1 +w+) (- +w+ r)) x)))
	   #+ignore
	   (let ((r (1+ (mod (1- j0) +w+))))
	     ;; place upper end of x in later block
	     (setf (aref W j1--)
		   (+ (mod (aref W j1--)
			   (expt 2 (- +w+ r)))
		      (* (mod x (expt 2 r))
			 (expt 2 (- +w+ r)))))
	     ;; place lower end of x in earlier block
	     (setf (aref W j0--)
		   (+ (- (aref W j0--)
			 (mod (aref W j0--)
			      (expt 2 (- +w+ r))))
		      (floor (/ x
				(expt 2 r))))))))
    (values)))

(defmethod writec ((A arrayc-fixed-element-size) (i integer) x)
  (let ((W (slot-value A 'virtual-bit-array))
	(len (element-size A)))
    (bitswrite W (* len i) (* len (1+ i)) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Using fractions of bits

(defclass arrayc-fractional (arrayc-virtual)
  ((denom :type 'integer
	  :initarg :denom
	  :reader denom)
   (symbols-per-chunk :initarg :symbols-per-chunk
		      :accessor symbols-per-chunk))
  (:documentation "
Uses a fractional number of bits to represent an element, assuming
that this compact array stores only values v such that

    0 ≤ v ≤ d.

If d << +w+, then each element \"occupies\" slightly more than log(d)
bits in the virtual bit array.
"))

(defmethod initialize-instance :after
    ((A arrayc-fractional)
     &key
       denom
       (symbols-per-chunk (floor (/ +w+ (log denom 2))))
       (length symbols-per-chunk))
  (setf (slot-value A 'symbols-per-chunk) symbols-per-chunk
	(slot-value A 'arrayc-length) length
	(slot-value A 'virtual-bit-array)
	(make-array (ceiling (/ length symbols-per-chunk)) :element-type (list 'unsigned-byte +w+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; readc and writec:

(defmethod readc ((A arrayc-fractional) (i integer))
  (let* ((d (denom A))
	 (k (symbols-per-chunk A))
	 (j (floor (/ i k)))
	 (r (1+ (mod i k))))
    (mod (floor (/ (aref (vchunks A) j) (expt d (- k r)))) d)))

(defmethod writec ((A arrayc-fractional) (i integer) x)
  (let* ((d (denom A))
	 (k (symbols-per-chunk A))
	 (j (floor (/ i k)))
	 (r (1+ (mod i k))))
    (setf (aref (vchunks A) j)
	  (+ (aref (vchunks A) j)
	     (- (* (readc A i) (expt d (- k r))))
	     (* x (expt d (- k r)))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elements of varying size

(defclass arrayc-encoded (arrayc)
  ((encoded-bit-array :type '(array fixnum)
		      :reader echunks)))

(defclass arrayc-varying-element-size (arrayc-encoded)
  (;; (coding-transform :reader codef :allocation 'class)
   ;; (decoding-transform :reader decodef :allocation 'class)
   (logical-block-position-skip-length ;; k
    :initarg :skip-length
    :reader skip-length)
   (logical-block-start-positions ;; P
    :documentation
    "Logical block starting positions in the virtual-bit-array, sampled
from every k-th logical block, where k is the value of
logical-block-position-skip-length")))

;;; sampled pointers

(defclass arrayc-varying-sampled (arrayc-varying-element-size)
  ((coding-transform
    :reader codef ;:allocation :class
    :initform (lambda (x) (gamma-code (1+ x))))
   (decoding-transform
    :reader decodef ;:allocation :class
    :initform (lambda (y) (1- (gamma-decode y)))))
  #+ignore ;; there is no such thing!
  (logical-block-length ;; b
   :documentation "Integer denoting the length of a logical block"
   :initarg :block-length))

(defmethod shared-initialize :after
    ((A arrayc-varying-element-size) slot-names
;     &rest initargs
     &key length skip-length)
  (declare (ignore slot-names))
  (setf (slot-value A 'encoded-bit-array)
	(make-array (ceiling (log (log (or length (lengthc A)) 2) 2))
		    :element-type (list 'unsigned-byte +w+)
		    :adjustable t)
	(slot-value A 'logical-block-position-skip-length) skip-length
;	(slot-value A 'logical-block-length) logical-block-length
	(slot-value A 'logical-block-start-positions)
	(make-array (ceiling (/ length skip-length))
		    :element-type 'integer)))

(defmethod update-instance-for-different-class :after
    ((fixed arrayc-fixed-element-size) (sampled arrayc-varying-sampled)
     &key skip-length)
  (let ((B (echunks sampled))
	(P (slot-value sampled 'logical-block-start-positions))
	(k skip-length)
	(sampled-codef (codef sampled))
	#+ignore
	(sampled-decodef (codef sampled)))
    (setf (slot-value sampled 'arrayc-length) (lengthc fixed))
    ;; single pass over fixed array
    (loop
       for i from 0 to (1- (lengthc fixed))
       with el-encoded of-type integer
       do (setf el-encoded (funcall sampled-codef (readc fixed i)))
       with len of-type integer do (setf len (integer-length el-encoded))
       sum len into len-sum
       maximize len into len-max
       ;; sample every k-th position into P
       when (= 0 (mod i k))
       do (setf (aref P (/ i k)) (- len-sum len))
       ;; write encoded element into B
       do (bitswrite B (- len-sum len) len-sum el-encoded)
       finally (return len-max))))

;;; dense pointers

(defclass arrayc-varying-dense (arrayc-varying-element-size)
  ((dense-block-positions ;; P'
     :documentation
     "Intermediate encoding of logical block positions, which together
with logical-block-start-positions comprises a two-level scheme")))

;; (defmethod initialize-instance :after
;;     (arrayc-varying-element-size-dense &key length skip-length))

#+ignore
(defmethod shared-initialize :after
    ((A arrayc-varying-dense) slot-names
     &rest initargs
     &key length block-length skip-length)
  (setf (slot-value A 'dense-block-positions)
	(make-instance 'arrayc-fixed-element-size
		       :element-size ??
		       :length length)
   ;; (slot-value A 'logical-block-position-skip-length) skip-length
	;; (slot-value A 'logical-block-length) logical-block-length
	;; (slot-value A 'logical-block-start-positions)
	;; (make-array (ceiling (/ length skip-length)) :element-type 'integer)
	))

#+ignore
(defmethod change-class (arrayc-fixed-element-size arrayc-varying-dense))

;;; partial sums

;;; direct access codes

;;; Elias-Fano codes
