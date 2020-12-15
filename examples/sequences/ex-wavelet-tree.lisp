;;; ex-wavelet-tree.lisp

(in-package :compactl.ex)

(defmethod ms:class-persistent-slots ((obj wavelet-tree))
  '(alphabet alphabet->index alphabet-hi alphabet-lo length 
    tree-root))

(defmethod ms:class-persistent-slots ((obj bitvector-naive))
  '(arrayc-length element-size virtual-bit-array))

(let* ((hamlet "tobeornottobethatisthequestion")
       (src-seq (coerce hamlet 'list))
       (wt (make-instance 'wavelet-tree
			  :sort-key #'char-code
			  :from-seq src-seq)))
  wt)
