;;;; compactl.asd

(asdf:defsystem :compactl
  :description " COMPACTL.
 Lisp-native compact data structures.  Based on Gonzalo Navarro's
book, Compact Data Structures."
  :author "Sky Hester"
  :license  "none specified"
  :version "0.0.1"
  :components
  ((:module "base"
	    :pathname #.(make-pathname :directory '(:relative))
	    :components ((:file "package")))
   (:module "src"
	    :depends-on ("base")
	    :components
	    ((:file "protocol")
	     (:module "compression"
		      :components ((:file "coding")))
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
