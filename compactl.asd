;;;; compactl.asd

(asdf:defsystem :compactl
  :description "

COMPACTL.

 Lisp-native compact data structures.  Based on Gonzalo Navarro's
 book, Compact Data Structures (Cambridge Univ. Press, 2016).

"
  :author "Sky Hester"
  :license  "none specified"
  :version "0.0.1"
  :depends-on ("priority-queue")
  :components
  ((:module "base"
	    :pathname #.(make-pathname :directory '(:relative))
	    :components ((:file "package")))
   ;; This system's organization was initially based on Figure 1.1,
   ;; p. 5 of Navarro (2016).
   (:module "src"
	    :depends-on ("base")
	    :components
	    ((:file "protocol")
	     (:module "compression"
		      :components ((:file "coding")
				   (:file "huffman")))
	     (:module "arrays"
		      :depends-on ("protocol"
				   "compression")
		      :components ((:file "array")))
	     (:module "bitvectors"
		      :depends-on ("protocol"
				   "compression"
				   "arrays")
		      :components ((:file "naive")
				   #+ignore
				   (:file "fast")
				   #+ignore
				   (:file "compressed")))
	     #+ignore
	     (:module "parentheses"
		      :depends-on ("compression"
				   "bitvectors"))
	     #+ignore
	     (:module "permutations"
		      :depends-on ("arrays"
				   "bitvectors"))
	     (:module "sequences"
		      :depends-on ("protocol"
				   "bitvectors"
				   #+ignore
				   "permutations")
		      :components ((:file "wavelet-tree")))
	     #+ignore
	     (:module "trees"
		      :depends-on ("bitvectors"
				   "parentheses"))
	     #+ignore
	     (:module "graphs"
		      :depends-on ("parentheses"
				   "sequences"))
	     #+ignore
	     (:module "grids"
		      :depends-on ("sequences"
				   "graphs"))
	     #+ignore
	     (:module "texts"
		      :depends-on ("compression"
				   "trees"
				   "sequences"))))))

(asdf:defsystem :compactl.ex
    :description "

COMPACTL EXAMPLES.

 Example usage of Lisp-native compact data structures defined by
 system COMPACTL.

"
  :author "Sky Hester"
  :license  "none specified"
  :version "0.0.1"
  :depends-on ("compactl" "marshal")
  :pathname "examples"
  :components
  ((:module "sequences"
	    :components
	    ((:file "ex-wavelet-tree")))))
