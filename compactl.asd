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
	    :components ((:file "package")
			 (:file "protocol")))
   (:module "compression"
	    :depends-on ("base")
	    :components ((:file "coding")))
   (:module "arrays"
	    :depends-on ("base"
			 "compression")
	    :components ((:file "array")))
   (:module "bitvectors"
	    :depends-on ("base"
			 "compression"
			 "arrays")
	    :components ((:file "bitvector")))
   #+ignore
   (:module "parentheses"
	    :depends-on ("compression"
			 "bitvectors"))
   #+ignore
   (:module "permutations"
	    :depends-on ("arrays"
			 "bitvectors"))
   (:module "sequences"
	    :depends-on ("base"
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
			 "sequences"))))
