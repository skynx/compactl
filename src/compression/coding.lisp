;;; coding.lisp

(in-package #:compactl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variable length codes for integers

;;; unary codes

(defun unary-code1 (x) "Convenient pre-compression transform for small
positive integers."
       (if (> x 1) (byte (1- x) 1) 0))

;;; γ-codes

(defun gamma-code (x) 
  (let ((lenx (integer-length x)))
    (if (> lenx 0)
	;; concatenate the unary code of lenx with all but the highest
	;; order bit of x (that place will hold a 0)
	(+ (ash (unary-code1 lenx) (1- lenx))
	   (mask-field (byte (1- lenx) 0) x))
	;; only positive integers have a γ-code
	(values))))

(defun gamma-decode (y)
  (if (= y 0)
      1
      (loop for p from (1- (integer-length y)) downto 0
	 until (not (logbitp p y))
	 finally (return (+ (ash 1 p) (logand (byte p 0) y))))))


#|
Correctness check for small positive integers:

(loop for i from 1 to 50000000 while i do
     (if (not (= i (gamma-decode (gamma-code i))))
	 (error "WHOA")))
|#

;;; δ-codes

;;; Rice codes

;;; variable bye (vByte) codes

;;; Simple-9

;;; PforDelta
